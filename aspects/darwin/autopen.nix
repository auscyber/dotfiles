{
  den,
  lib,
  inputs,
  rootPath,
  ...
}:
# The codesigning key, held by an `autopen serve` daemon. Builds sign through
# its socket with a remote-key handle (`signerFor`; it holds only the socket
# path, the reference path and the verification key) and never see key
# material. Whoever can open the reference file -- `nixbld` -- can sign.
let
  stateDir = "/var/lib/autopen";
  runDir = "/var/run/autopen";
  socket = "${runDir}/autopen.sock";
  ref = "${stateDir}/codesign.ref";
  user = "_autopen";

  autopenFor = pkgs: inputs.autopen.packages.${pkgs.stdenv.hostPlatform.system}.autopen;

  # One identity for the fleet, at a fixed path, so an overlay (which has no
  # host config) can find its public half.
  keyBase = rootPath + "/secrets/generated/autopen/codesign-key";
in
{
  # Its own nixpkgs: autopen needs a newer rustc than ours may carry.
  ff.autopen.url = "github:emilazy/boot-security-tools";

  # Generated once, on the admin host: the key is the secret; its verification
  # key and the self-signed certificate land beside it as `.vk` and `.crt.der`.
  den.aspects.agenix-rekey.age.generators.autopen_codesign =
    {
      pkgs,
      lib,
      file,
      ...
    }:
    let
      a = lib.getExe (autopenFor pkgs);
      base = lib.escapeShellArg (lib.removeSuffix ".age" file);
    in
    ''
      dir=$(mktemp -d)
      trap 'rm -rf "$dir"' EXIT
      ${a} signing-key software rsa3072-pkcs1-sha256 generate --output "$dir/key" >&2
      ${a} signing-key get-verification-key --signing-key "$dir/key" --output ${base}.vk >&2
      cert=(--verification-key ${base}.vk --purpose code-signing
        --common-name ${lib.escapeShellArg den.lib.autopen.commonName}
        --not-before "$(date -u +%Y-%m-%dT00:00:00Z)" --lifetime-days 7300)
      ${a} x509 create-tbs-certificate --output "$dir/tbs" "''${cert[@]}" >&2
      ${a} sign --signing-key "$dir/key" --output "$dir/tbs.sig" "$dir/tbs" >&2
      ${a} x509 create-certificate --signature "$dir/tbs.sig" --output ${base}.crt.der "''${cert[@]}" >&2
      cat "$dir/key"
    '';

  den.aspects.autopen = {
    includes = [ den.aspects.agenix-rekey ];
    secretScope = "autopen";
    secrets.codesign-key = {
      rekeyFile = keyBase + ".age";
      generator.script = "autopen_codesign";
      owner = user;
    };

    darwin =
      { pkgs, scoped, ... }:
      {
        users.knownUsers = [ user ];
        users.users.${user} = {
          uid = 498;
          gid = 498;
          home = stateDir;
          shell = "/usr/bin/false";
          description = "autopen signing service";
        };
        users.knownGroups = [ user ];
        users.groups.${user}.gid = 498;

        # The reference file IS the capability: whoever can open it can sign.
        system.activationScripts.postActivation.text = ''
          install -d -m 0750 -o ${user} -g nixbld ${stateDir}
          install -d -m 0750 -o ${user} -g nixbld ${runDir}
          [ -e ${ref} ] || install -m 0640 -o ${user} -g nixbld /dev/null ${ref}
        '';

        launchd.daemons.autopen.serviceConfig = {
          ProgramArguments = [
            (lib.getExe (autopenFor pkgs))
            "serve"
            "--socket-path"
            socket
            "--signing-key-ref"
            ref
            scoped.autopen.secrets.codesign-key.path
          ];
          UserName = user;
          GroupName = "nixbld";
          # 0770: connecting to a socket needs write permission.
          Umask = 7;
          KeepAlive = true;
          RunAtLoad = true;
          StandardErrorPath = "${stateDir}/autopen.log";
        };
      };
  };

  # `nix run .#autopen-ci-serve` on a CI runner: a throwaway key served on the
  # fixed socket, so signed wrappers build. It does not match the committed
  # certificate, which is fine: CI proves they build, nothing installs them.
  perSystem =
    { pkgs, lib, ... }:
    {
      packages.autopen-ci-serve = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (
        pkgs.writeShellApplication {
          name = "autopen-ci-serve";
          runtimeInputs = [
            (autopenFor pkgs)
            pkgs.coreutils
          ];
          text = ''
            umask 000
            sudo install -d -m 0777 ${stateDir} ${runDir}
            keydir=$(mktemp -d)
            autopen signing-key software rsa3072-pkcs1-sha256 generate --output "$keydir/key"
            : >${ref}
            nohup autopen serve --socket-path ${socket} --signing-key-ref ${ref} "$keydir/key" \
              >"$keydir/serve.log" 2>&1 &
            for _ in $(seq 50); do
              [ -S ${socket} ] && exit 0
              sleep 0.2
            done
            cat "$keydir/serve.log" >&2
            exit 1
          '';
        }
      );
    };

  den.lib.autopen = {
    inherit
      socket
      ref
      runDir
      autopenFor
      ;
    commonName = "Dendritic Autopen Codesign";

    # Null until `gen-secrets` has written the public half.
    signerFor =
      pkgs:
      let
        vk = keyBase + ".vk";
        crt = keyBase + ".crt.der";
        autopen = autopenFor pkgs;
      in
      if !(builtins.pathExists vk && builtins.pathExists crt) then
        null
      else
        {
          tool = pkgs.callPackage ../../packages/autopen-codesign/package.nix { inherit autopen; };
          certificate = builtins.path {
            path = crt;
            name = "autopen-codesign.crt.der";
          };
          remoteKey = pkgs.runCommand "autopen-codesign-remote.key" { } ''
            ${lib.getExe autopen} signing-key remote create --socket-path ${socket} \
              --file-ref-path ${ref} --output $out \
              --verification-key ${
                builtins.path {
                  path = vk;
                  name = "autopen-codesign.vk";
                }
              }
          '';
        };
  };
}
