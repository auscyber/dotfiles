{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.age;

  isDarwin = lib.attrsets.hasAttrByPath [ "environment" "darwinConfig" ] options;

  mapListOrAttrs = f: x: if builtins.isList x then map f x else lib.mapAttrs (_: f) x;
  mapListOrAttrsToList = f: x: if builtins.isList x then map f x else lib.mapAttrsToList (_: f) x;
  secretsMountPoint = "secrets";
  templatesMountPoint = "templates";

  ageBin = config.age.ageBin;

  users = config.users.users;

  sysusersEnabled =
    if isDarwin then
      false
    else
      options.systemd ? sysusers && (config.systemd.sysusers.enable || config.services.userborn.enable);

  mountCommand =
    if isDarwin then
      ''
        if ! diskutil info "${cfg.ageMountPoint}" &> /dev/null; then
            num_sectors=1048576
            dev=$(hdiutil attach -nomount ram://"$num_sectors" | sed 's/[[:space:]]*$//')
            newfs_hfs -v agenix "$dev"
            mount -t hfs -o nobrowse,nodev,nosuid,-m=0751 "$dev" "${cfg.ageMountPoint}"
        fi
      ''
    else
      ''
        grep -q "${cfg.ageMountPoint} ramfs" /proc/mounts ||
          mount -t ramfs none "${cfg.ageMountPoint}" -o nodev,nosuid,mode=0751
      '';
  newGeneration = ''
    _agenix_generation="$(basename "$(readlink ${cfg.secretsDir})" || echo 0)"
    _old_generation="$_agenix_generation"
    (( ++_agenix_generation ))
    echo "[agenix] creating new generation in ${cfg.ageMountPoint}/$_agenix_generation"
    mkdir -p "${cfg.ageMountPoint}"
    chmod 0751 "${cfg.ageMountPoint}"
    ${mountCommand}
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation"
    chmod 0751 "${cfg.ageMountPoint}/$_agenix_generation"
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation/${secretsMountPoint}"
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation/${templatesMountPoint}"
  '';

  chownGroup = if isDarwin then "admin" else "keys";
  # chown the secrets mountpoint and the current generation to the keys group
  # instead of leaving it root:root.
  chownMountPoint = ''
    chown :${chownGroup} "${cfg.ageMountPoint}" "${cfg.ageMountPoint}/$_agenix_generation"
  '';

  setTruePath = secretType: ''
    ${
      if secretType.symlink then
        ''
          _truePath="${cfg.ageMountPoint}/$_agenix_generation/${
            if secretType.type == "template" then templatesMountPoint else secretsMountPoint
          }/${secretType.name}"
        ''
      else
        ''
          _truePath="${secretType.path}"
        ''
    }
  '';
  setupStart = secretType: ''
    ${setTruePath secretType}
    echo "setting up ${secretType.type} '${secretType.name}' at '$_truePath'..."
    TMP_FILE="$_truePath.tmp"
    mkdir -p "$(dirname "$_truePath")"
    IDENTITIES=()
    for identity in ${toString cfg.identityPaths}; do
      test -r "$identity" || continue
      test -s "$identity" || continue
      IDENTITIES+=(-i)
      IDENTITIES+=("$identity")
    done

    test "''${#IDENTITIES[@]}" -eq 0 && echo "[agenix] WARNING: no readable identities found!"

    mkdir -p "$(dirname "$_truePath")"

  '';

  installTemplate = templateType: ''
    ${setupStart templateType}
    echo "generating template '${templateType.name}' to '$_truePath'..."
    cp ${templateType.file} "$TMP_FILE"
     
    ${builtins.concatStringsSep "\n" (
      lib.flip mapListOrAttrsToList templateType.placeholderMap (dep: ''
        echo "replacing placeholder ${dep.placeholder} in ${templateType.name}..."
        test -f "${dep.file}" || echo "[agenix] WARNING: dependency file ${dep.file} does not exist!"

        ${pkgs.gnused}/bin/sed -i.bak "s|${dep.placeholder}|$(${ageBin} --decrypt "''${IDENTITIES[@]}" ${lib.escapeShellArg dep.file})|g" "$TMP_FILE"
        rm -f "$TMP_FILE.bak"
      '')
    )}
    # check if the template changed then add units
    if [ -f "${cfg.templateDir}/${templateType.name}" ]; then
      if ${pkgs.diffutils}/bin/cmp -s "${cfg.templateDir}/${templateType.name}" "$TMP_FILE"; then
        :
      else
        echo "template ${templateType.name} changed."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") templateType.restartUnits)}
        ${concatStringsSep "\n" (
          map (unit: "USER_UNITS_TO_RESTART+=('${templateType.owner}:${unit}')") templateType.restartUserUnits
        )}
      fi
    else
        echo "template ${templateType.name} is new."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") templateType.restartUnits)}
        ${concatStringsSep "\n" (
          map (unit: "USER_UNITS_TO_RESTART+=('${templateType.owner}:${unit}')") templateType.restartUserUnits
        )}
    fi



    chmod ${templateType.mode} "$TMP_FILE"
    mv -f "$TMP_FILE" "$_truePath"
    ${optionalString templateType.symlink ''
      [ "${templateType.path}" != "${cfg.templateDir}/${templateType.name}" ] && ln -sfT "${cfg.templateDir}/${templateType.name}" "${templateType.path}"
    ''}
  '';

  installSecret = secretType: ''
    ${setupStart secretType}
    echo "decrypting '${secretType.file}' to '$_truePath'..."

        [ "${secretType.path}" != "${cfg.secretsDir}/${secretType.name}" ] && mkdir -p "$(dirname "${secretType.path}")"
    (
      umask u=r,g=,o=
      test -f "${secretType.file}" || echo '[agenix] WARNING: encrypted file ${secretType.file} does not exist!'
      test -d "$(dirname "$TMP_FILE")" || echo "[agenix] WARNING: $(dirname "$TMP_FILE") does not exist!"
      LANG=${
        config.i18n.defaultLocale or "C"
      } ${ageBin} --decrypt "''${IDENTITIES[@]}" -o "$TMP_FILE" "${secretType.file}"
    )

    if [ -f "${cfg.secretsDir}/${secretType.name}" ]; then
      if ${pkgs.diffutils}/bin/cmp -s "${cfg.secretsDir}/${secretType.name}" "$TMP_FILE"; then
        :
      else
        echo "${secretType.name} changed."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") secretType.restartUnits)}
        ${concatStringsSep "\n" (
          map (unit: "USER_UNITS_TO_RESTART+=('${secretType.owner}:${unit}')") secretType.restartUserUnits
        )}
      fi
    else
        echo "${secretType.name} is new."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") secretType.restartUnits)}
        ${concatStringsSep "\n" (
          map (unit: "USER_UNITS_TO_RESTART+=('${secretType.owner}:${unit}')") secretType.restartUserUnits
        )}
    fi

    chmod ${secretType.mode} "$TMP_FILE"
    mv -f "$TMP_FILE" "$_truePath"

    ${optionalString secretType.symlink ''
      [ "${secretType.path}" != "${cfg.secretsDir}/${secretType.name}" ] && ln -sfT "${cfg.secretsDir}/${secretType.name}" "${secretType.path}"
    ''}
  '';

  testIdentities = map (path: ''
    test -f ${path} || echo '[agenix] WARNING: config.age.identityPaths entry ${path} not present!'
  '') cfg.identityPaths;

  cleanupAndLink = ''
    _agenix_generation="$(basename "$(readlink ${cfg.secretsDir})" || echo 0)"
    (( ++_agenix_generation ))
    echo "[agenix] symlinking new secrets to ${cfg.secretsDir} (generation $_agenix_generation)..."
    ln -sfT "${cfg.ageMountPoint}/$_agenix_generation/${secretsMountPoint}" ${cfg.secretsDir}
    echo "[agenix] symlinking new templates to ${cfg.templateDir} (generation $_agenix_generation)..."
    ln -sfT "${cfg.ageMountPoint}/$_agenix_generation/${templatesMountPoint}" ${cfg.templateDir}

    (( _agenix_generation > 1 )) && {
    echo "[agenix] removing old secrets (generation $(( _agenix_generation - 1 )))..."
    rm -rf "${cfg.ageMountPoint}/$(( _agenix_generation - 1 ))"
    }
  '';

  installSecrets = builtins.concatStringsSep "\n" (
    [
      "echo '[agenix] decrypting secrets...'"
      "UNITS_TO_RESTART=()"
      "USER_UNITS_TO_RESTART=()"
    ]
    ++ testIdentities
    ++ (map installSecret (builtins.attrValues cfg.secrets))
    ++ (map installTemplate (builtins.attrValues cfg.templates))
    ++ [
      cleanupAndLink
      (optionalString (!isDarwin) ''
        if [ "''${#UNITS_TO_RESTART[@]}" -ne 0 ]; then
          UNIQUE_UNITS=($(printf "%s\n" "''${UNITS_TO_RESTART[@]}" | sort -u))
          echo "[agenix] restarting units: ''${UNIQUE_UNITS[*]}"
          ${pkgs.systemd}/bin/systemctl try-restart "''${UNIQUE_UNITS[@]}"
        fi
        if [ "''${#USER_UNITS_TO_RESTART[@]}" -ne 0 ]; then
          UNIQUE_USER_UNITS=($(printf "%s\n" "''${USER_UNITS_TO_RESTART[@]}" | sort -u))
          echo "[agenix] restarting user units: ''${UNIQUE_USER_UNITS[*]}"
          for entry in "''${UNIQUE_USER_UNITS[@]}"; do
             user="''${entry%%:*}"
             unit="''${entry#*:}"
             ${pkgs.systemd}/bin/systemctl --user -M "$user@" try-restart "$unit"
          done
        fi
      '')
      (optionalString isDarwin ''
        if [ "''${#UNITS_TO_RESTART[@]}" -ne 0 ]; then
          UNIQUE_UNITS=($(printf "%s\n" "''${UNITS_TO_RESTART[@]}" | sort -u))
          echo "[agenix] restarting units: ''${UNIQUE_UNITS[*]}"
          for unit in "''${UNIQUE_UNITS[@]}"; do
            launchctl kickstart -k "system/$unit"
          done
        fi
        if [ "''${#USER_UNITS_TO_RESTART[@]}" -ne 0 ]; then
          UNIQUE_USER_UNITS=($(printf "%s\n" "''${USER_UNITS_TO_RESTART[@]}" | sort -u))
          echo "[agenix] restarting user units: ''${UNIQUE_USER_UNITS[*]}"
          for entry in "''${UNIQUE_USER_UNITS[@]}"; do
             user="''${entry%%:*}"
             unit="''${entry#*:}"
             uid=$(id -u "$user")
             launchctl kickstart -k "gui/$uid/$unit"
          done
        fi
      '')
    ]
  );

  chownSecret = secretType: ''
    ${setTruePath secretType}
    chown ${secretType.owner}:${secretType.group} "$_truePath"
  '';

  chownSecrets = builtins.concatStringsSep "\n" (
    [ "echo '[agenix] chowning...'" ]
    ++ [ chownMountPoint ]
    ++ (map chownSecret (builtins.attrValues cfg.secrets))
    ++ (map chownSecret (builtins.attrValues cfg.templates))
  );
  templateType = types.submodule (
    { config, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = config._module.args.name;
          description = "The name of this secret.";
        };
        dependencies = mkOption {
          type =
            with types;
            oneOf [
              (listOf unspecified)
              (attrsOf unspecified)
            ];
          example = literalExpression ''[ config.age.secrets.basicAuthPw1 nixosConfigurations.machine2.config.age.secrets.basicAuthPw ]'';
          default = [ ];
          description = ''
            Other secrets on which this template depends. This guarantees that in the final
            `agenix generate` script, all dependencies will be generated before
            this secret is generated, allowing you use their outputs via the passed `decrypt` function.

            The given dependencies will be passed to the defined `script` via the `deps` parameter,
            which will be a list or attrset of their true source locations (`rekeyFile`).

            This should refer only to secret definitions from `config.age.secrets` that
            have a generator. This is useful if you want to create derived secrets,
            such as generating a .htpasswd file from several basic auth passwords.

            You may refer to age secrets of other nixos hosts as long as all hosts
            are rekeyed via the same flake.
          '';
        };
        restartUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            List of units to restart when this template is updated.
          '';
        };
        restartUserUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            List of user units to restart when this template is updated.
          '';
        };
        type = mkOption {
          type = types.enum [ "template" ];
          readOnly = true;
          internal = true;
          default = "template";
        };

        content = mkOption {
          type = types.functionTo (types.str);
        };
        owner = mkOption {
          type = types.str;
          default = "0";
        };
        group = mkOption {
          type = types.str;
          default = "0";
        };
        mode = mkOption {
          type = types.str;
          default = "0400";
        };
        path = mkOption {
          type = types.str;
          default = "${config.age.templateDir}/${config.name}";
          description = "The path where the generated file will be written to.";
        };
        placeholderMap = mkOption {
          type = types.attrsOf types.attrs;
          default = mapListOrAttrs (
            dep:
            dep
            // {
              placeholder = "<AGE_PLACEHOLDER:${builtins.hashString "sha256" dep.name}:${builtins.hashString "sha256" dep.file}:>";
            }
          ) config.dependencies;
          internal = true;
          readOnly = true;
          description = ''
            Placeholders to be used in the content script. Each placeholder key will be
            replaced by its corresponding value in the final generated secret content.
          '';
        };
        file = mkOption {
          type = types.path;
          default = pkgs.writeTextFile {
            name = "agenix-template-${config.name}-content";
            text = config.content {
              pkgs = pkgs;
              lib = lib;
              placeholders = mapListOrAttrs (dep: dep.placeholder) config.placeholderMap;
            };
          };
          defaultText = literalExpression ''
                pkgs.writeTextFile {
                  name = "agenix-template-''${config.name}-content";
                  text = config.content {
                    pkgs = pkgs;
                    lib = lib;
                    deps = ''${mapListOrAttrsToList (dep: dep.path) config.dependencies};
                  };
                }
              description = "The path where the generated file will be written to.";
            };
          '';
        };
        symlink = mkEnableOption "symlinking templates to their destination" // {
          default = true;
        };

      };

    }
  );

  secretType = types.submodule (
    { config, ... }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = config._module.args.name;
          defaultText = literalExpression "config._module.args.name";
          description = ''
            Name of the file used in {option}`age.secretsDir`
          '';
        };
        type = mkOption {
          type = types.enum [ "secret" ];
          readOnly = true;
          internal = true;
          default = "secret";
        };
        file = mkOption {
          type = types.path;
          description = ''
            Age file the secret is loaded from.
          '';
        };
        path = mkOption {
          type = types.str;
          default = "${cfg.secretsDir}/${config.name}";
          defaultText = literalExpression ''
            "''${cfg.secretsDir}/''${config.name}"
          '';
          description = ''
            Path where the decrypted secret is installed.
          '';
        };
        mode = mkOption {
          type = types.str;
          default = "0400";
          description = ''
            Permissions mode of the decrypted secret in a format understood by chmod.
          '';
        };
        restartUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            List of units to restart when this secret is updated.
          '';
        };
        restartUserUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            List of user units to restart when this secret is updated.
          '';
        };
        owner = mkOption {
          type = types.str;
          default = "0";
          description = ''
            User of the decrypted secret.
          '';
        };
        group = mkOption {
          type = types.str;
          default = users.${config.owner}.group or "0";
          defaultText = literalExpression ''
            users.''${config.owner}.group or "0"
          '';
          description = ''
            Group of the decrypted secret.
          '';
        };
        symlink = mkEnableOption "symlinking secrets to their destination" // {
          default = true;
        };
      };
    }
  );
in
{
  imports = [
    (mkRenamedOptionModule [ "age" "sshKeyPaths" ] [ "age" "identityPaths" ])
  ];

  options.age = {
    templates = mkOption {
      type = types.attrsOf templateType;
      default = { };
      description = ''
        Attrset of age secret templates to be generated.
      '';
    };

    ageBin = mkOption {
      type = types.str;
      default = "${pkgs.age}/bin/age";
      defaultText = literalExpression ''
        "''${pkgs.age}/bin/age"
      '';
      description = ''
        The age executable to use.
      '';
    };
    secrets = mkOption {
      type = types.attrsOf secretType;
      default = { };
      description = ''
        Attrset of secrets.
      '';
    };
    secretsDir = mkOption {
      type = types.path;
      default = "/run/agenix/secrets";
      description = ''
        Folder where secrets are symlinked to
      '';
    };
    templateDir = mkOption {
      type = types.path;
      default = "/run/agenix/templates";
      description = ''
        Folder where rendered templates are symlinked to
      '';
    };
    ageMountPoint = mkOption {
      type =
        types.addCheck types.str (
          s:
          (builtins.match "[ \t\n]*" s) == null # non-empty
          && (builtins.match ".+/" s) == null
        ) # without trailing slash
        // {
          description = "${types.str.description} (with check: non-empty without trailing slash)";
        };
      default = "/run/agenix.d";
      description = ''
        Where secrets are created before they are symlinked to {option}`age.secretsDir`
      '';
    };
    identityPaths = mkOption {
      type = types.listOf types.path;
      default =
        if isDarwin then
          [
            "/etc/ssh/ssh_host_ed25519_key"
            "/etc/ssh/ssh_host_rsa_key"
          ]
        else if (config.services.openssh.enable or false) then
          map (e: e.path) (
            lib.filter (e: e.type == "rsa" || e.type == "ed25519") config.services.openssh.hostKeys
          )
        else
          [ ];
      defaultText = literalExpression ''
        if isDarwin
        then [
          "/etc/ssh/ssh_host_ed25519_key"
          "/etc/ssh/ssh_host_rsa_key"
        ]
        else if (config.services.openssh.enable or false)
        then map (e: e.path) (lib.filter (e: e.type == "rsa" || e.type == "ed25519") config.services.openssh.hostKeys)
        else [];
      '';
      description = ''
        Path to SSH keys to be used as identities in age decryption.
      '';
    };

  };

  config = mkIf (cfg.secrets != { } || cfg.templates != { }) (mkMerge [
    {
      assertions = [
        {
          assertion = cfg.identityPaths != [ ];
          message = "age.identityPaths must be set, for example by enabling openssh.";
        }
      ];
    }

    (optionalAttrs (!isDarwin) {
      # When using sysusers we no longer be started as an activation script
      # because those are started in initrd while sysusers is started later.
      systemd.services.agenix-install-secrets = mkIf sysusersEnabled {
        wantedBy = [ "sysinit.target" ];
        after = [ "systemd-sysusers.service" ];
        unitConfig.DefaultDependencies = "no";

        path = [
          pkgs.mount
          pkgs.diffutils
          pkgs.systemd
          pkgs.gnused
        ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = pkgs.writeShellScript "agenix-install" (concatLines [
            newGeneration
            installSecrets
            chownSecrets
          ]);
          RemainAfterExit = true;
        };
      };

      # Create a new directory full of secrets for symlinking (this helps
      # ensure removed secrets are actually removed, or at least become
      # invalid symlinks).
      system.activationScripts = mkIf (!sysusersEnabled) {
        agenixNewGeneration = {
          text = newGeneration;
          deps = [
            "specialfs"
          ];
        };

        agenixInstall = {
          text = installSecrets;
          deps = [
            "agenixNewGeneration"
            "specialfs"
          ];
        };

        # So user passwords can be encrypted.
        users.deps = [ "agenixInstall" ];

        # Change ownership and group after users and groups are made.
        agenixChown = {
          text = chownSecrets;
          deps = [
            "users"
            "groups"
          ];
        };

        # So other activation scripts can depend on agenix being done.
        agenix = {
          text = "";
          deps = [ "agenixChown" ];
        };
      };
    })

    (optionalAttrs isDarwin {
      launchd.daemons.activate-agenix = {
        script = ''
          set -e
          set -o pipefail
          export PATH="${pkgs.gnused}/bin:${pkgs.gnugrep}/bin:${pkgs.coreutils}/bin:${pkgs.diffutils}/bin:@out@/sw/bin:/usr/bin:/bin:/usr/sbin:/sbin"
          ${newGeneration}
          ${installSecrets}
          ${chownSecrets}
          exit 0
        '';
        serviceConfig = {
          RunAtLoad = true;
          KeepAlive.SuccessfulExit = false;
        };
      };
    })
  ]);
}
