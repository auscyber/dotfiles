{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.secrets;
  mapListOrAttrs = f: x: if builtins.isList x then map f x else lib.mapAttrs (_: f) x;
  mapListOrAttrsToList = f: x: if builtins.isList x then map f x else lib.mapAttrsToList (_: f) x;

in
{
  options = {
    auscybernix.secrets = {
      enable = lib.mkEnableOption "Enable sops integration for managing secrets.";
      configId = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "config id to put in rekeys and generated";
      };
    };
  };
  config = lib.mkIf cfg.enable {
    age.generators = {
      toEnv =
        # takes values of form { SECRET_KEY = placheholders.secret; } and converts to env file
        {
          deps,
          value,
          pkgs,
          decrypt,
          ...
        }:
        let
          env = builtins.toJSON value;
        in
        ''
          (
          value='${env}'
          ${pkgs.lib.concatStrings (
            mapListOrAttrsToList (dep: ''
              echo '${dep.placeholder}' > /dev/stderr
              echo "s|${dep.placeholder}|$(${decrypt} ${lib.escapeShellArg dep.file})|" > /dev/stderr
              value=$(echo "$value" | sed "s|${dep.placeholder}|$(${decrypt} ${lib.escapeShellArg dep.file})|g")
            '') deps
          )}
          echo "$value" | ${pkgs.lib.getExe pkgs.jq} -r 'to_entries|map("\(.key)=\(.value|tostring)")|.[]'
          )
        '';
      toYAML =
        {
          deps,
          value,
          pkgs,
          decrypt,
          ...
        }:
        let
          yaml = pkgs.lib.generators.toYAML { } value;
        in
        ''
                set -euo pipefail
                yaml='${yaml}'
          ${pkgs.lib.concatStrings (
            mapListOrAttrsToList (dep: ''
              yaml=$(echo "$yaml" | sed "s|${dep.placeholder}|$(${decrypt} ${lib.escapeShellArg dep.file})|g")
            '') deps
          )}
                echo "$yaml"
        '';
      toINI =
        {
          deps,
          value,
          pkgs,
          decrypt,
          ...
        }:
        let
          ini = pkgs.lib.generators.toINI { } value;
        in
        ''
                set -euo pipefail
                ini='${ini}'
          ${pkgs.lib.concatStrings (
            mapListOrAttrsToList (dep: ''
              ini=$(echo "$ini" | sed "s|${dep.placeholder}|$(${decrypt} ${lib.escapeShellArg dep.file})|g")
            '') deps
          )}
            echo "$ini"
        '';

    };

    age.secrets.github_token = {
      rekeyFile = ./github_token.age;
      #            intermediary = true;
    };

    age.rekey = {
      generatedSecretsDir = ../../secrets/generated + "/${cfg.configId}";

      localStorageDir = ../.. + "/secrets/rekeyed/${cfg.configId}";
      agePlugins =
        with pkgs;
        [
		age-plugin-gpg
        ]
        ++ lib.optionals pkgs.stdenv.isDarwin [
          pkgs.age-plugin-1p
          pkgs.age-plugin-se
        ];
      # Obtain this using `ssh-keyscan` or by looking it up in your ~/.ssh/known_hosts
      # The path to the master identity used for decryption. See the option's description for more information.
      masterIdentities = [
        #        ./main.pub
        #        {
        #          git
        #        }
        #        ./publickey.txt
        #        "age1se1qf48z2tlp6ua8hpyg7vypm0dw8z8nmgusell62r8vpyufemre6escazv5f2"

        #{
        #identity = "/users/ivypierlot/.config/se.txt";
        #pubkey = "age1se1q2ae7s32el5t7fpsut9996tch347w55ysut8jhed3f05tjgt293lz55u5p";

        #}
        {
          identity = ./age-yubikey.pub;
          pubkey = "age1yubikey1qv6zc6sjz4klkjxnnt2sv8ptlcjtmhphduu4rrqjuw88jn2nftuu6ep0kr3";

        }
        {
          identity = ./main.pub; # Private key
          pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeCdR16VYTNmoEekYk/b1sskC+trPx9tpOBJoKML17H"; # Public key
        }
      ];
      #masterIdentities = [ "/home/myuser/master-key" ]; # External master key
      #masterIdentities = [
      #  # It is possible to specify an identity using the following alternate syntax,
      #  # this can be used to avoid unecessary prompts during encryption.
      #  {
      #    identity = "/home/myuser/master-key.age"; # Password protected external master key
      #    pubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq"; # Specify the public key explicitly
      #  }
      #];
      storageMode = "local";
      # Choose a directory to store the rekeyed secrets for this host.
      # This cannot be shared with other hosts. Please refer to this path
      # from your flake's root directory and not by a direct path literal like ./secrets

    };
  };
}
