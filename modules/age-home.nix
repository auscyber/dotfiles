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

  ageBin = lib.getExe config.age.package;

  mapListOrAttrs = f: x: if builtins.isList x then map f x else lib.mapAttrs (_: f) x;
  mapListOrAttrsToList = f: x: if builtins.isList x then map f x else lib.mapAttrsToList (_: f) x;

  secretsMountPoint = "/secrets";
  templatesMountPoint = "/templates";

  newGeneration = ''
    _agenix_generation="$(basename "$(dirname "$(readlink "${cfg.secretsDir}")")" || echo 0)"
    (( ++_agenix_generation ))
    echo "[agenix] creating new generation in ${cfg.ageMountPoint}/$_agenix_generation"
    mkdir -p "${cfg.ageMountPoint}"
    chmod 0751 "${cfg.ageMountPoint}"
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation"
    chmod 0751 "${cfg.ageMountPoint}/$_agenix_generation"
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation${secretsMountPoint}"
    mkdir -p "${cfg.ageMountPoint}/$_agenix_generation${templatesMountPoint}"
  '';

  setTruePath = secretType: ''
    ${
      if secretType.symlink then
        ''
          _truePath="${cfg.ageMountPoint}/$_agenix_generation/${
            if secretType.type == "template" then "templates" else "secrets"
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

            _secret=$(${ageBin} --decrypt "''${IDENTITIES[@]}" ${lib.escapeShellArg dep.file})
            _secret="''${_secret//\\/\\\\}"
            _secret="''${_secret//|/\\|}"
            _secret="''${_secret//&/\\&}"
            _secret="''${_secret//$'\n'/\\n}"
            sed -i.bak "s|${dep.placeholder}|''${_secret}|g" "$TMP_FILE"
            rm -f "$TMP_FILE.bak"
          '')
        )}
        # check if the template changed then add units
        if [ -f "$_truePath" ]; then
          if cmp -s "$_truePath" "$TMP_FILE"; then
            :
          else
            echo "template ${templateType.name} changed."
            ${concatStringsSep "\n" (
              map (unit: "UNITS_TO_RESTART+=('${unit}')") templateType.restartUnits
            )}
          fi
        else
            echo "template ${templateType.name} is new."
            ${concatStringsSep "\n" (
              map (unit: "UNITS_TO_RESTART+=('${unit}')") templateType.restartUnits
            )}
        fi

        chmod ${templateType.mode} "$TMP_FILE"
        mv -f "$TMP_FILE" "$_truePath"
    ${optionalString templateType.symlink ''
      # shellcheck disable=SC2193,SC2050
      [ "${templateType.path}" != "${cfg.templateDir}/${templateType.name}" ] && ln -sfT "${cfg.templateDir}/${templateType.name}" "${templateType.path}"
    ''}

  '';

  installSecret = secretType: ''
    ${setupStart secretType}

    # shellcheck disable=SC2193,SC2050
    [ "${secretType.path}" != "${cfg.secretsDir}/${secretType.name}" ] && mkdir -p "$(dirname "${secretType.path}")"
    (
      umask u=r,g=,o=
      test -f "${secretType.file}" || echo '[agenix] WARNING: encrypted file ${secretType.file} does not exist!'
      test -d "$(dirname "$TMP_FILE")" || echo "[agenix] WARNING: $(dirname "$TMP_FILE") does not exist!"
      LANG=${
        config.i18n.defaultLocale or "C"
      } ${ageBin} --decrypt "''${IDENTITIES[@]}" -o "$TMP_FILE" "${secretType.file}"
    )
    if [ -f "$_truePath" ]; then
      if cmp -s "$_truePath" "$TMP_FILE"; then
        :
      else
        echo "${secretType.name} changed."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") secretType.restartUnits)}
      fi
    else
        echo "${secretType.name} is new."
        ${concatStringsSep "\n" (map (unit: "UNITS_TO_RESTART+=('${unit}')") secretType.restartUnits)}
    fi
    chmod ${secretType.mode} "$TMP_FILE"
    mv -f "$TMP_FILE" "$_truePath"

    ${optionalString secretType.symlink ''
      # shellcheck disable=SC2193,SC2050
      [ "${secretType.path}" != "${cfg.secretsDir}/${secretType.name}" ] && ln -sfT "${cfg.secretsDir}/${secretType.name}" "${secretType.path}"
    ''}
  '';

  testIdentities = map (path: ''
    test -f ${path} || echo '[agenix] WARNING: config.age.identityPaths entry ${path} not present!'
  '') cfg.identityPaths;

  cleanupAndLink = ''
    _agenix_generation="$(basename "$(dirname "$(readlink "${cfg.secretsDir}")")" || echo 0)"
    (( ++_agenix_generation ))
    echo "[agenix] symlinking new secrets to ${cfg.secretsDir} (generation $_agenix_generation)..."
     # Ensure parent dir exists (e.g. .../agenix if targets are .../agenix/secrets)
    mkdir -p "$(dirname "${cfg.secretsDir}")"
    mkdir -p "$(dirname "${cfg.templateDir}")"

    ln -sfT "${cfg.ageMountPoint}/$_agenix_generation${secretsMountPoint}" "${cfg.secretsDir}"

    echo "[agenix] symlinking new templates to ${cfg.templateDir} (generation $_agenix_generation)..."
    ln -sfT "${cfg.ageMountPoint}/$_agenix_generation${templatesMountPoint}" "${cfg.templateDir}"

    (( _agenix_generation > 1 )) && {
    echo "[agenix] removing old secrets (generation $(( _agenix_generation - 1 )))..."
    rm -rf "${cfg.ageMountPoint}/$(( _agenix_generation - 1 ))"
    }
  '';

  installSecrets = builtins.concatStringsSep "\n" (
    [
      "echo '[agenix] decrypting secrets...'"
      "UNITS_TO_RESTART=()"
    ]
    ++ testIdentities
    ++ (map installSecret (builtins.attrValues cfg.secrets))
    ++ (map installTemplate (builtins.attrValues cfg.templates))
    ++ [
      cleanupAndLink
      ''
        if [ "''${#UNITS_TO_RESTART[@]}" -ne 0 ]; then
          mapfile -t UNIQUE_UNITS < <(printf "%s\n" "''${UNITS_TO_RESTART[@]}" | sort -u)
          echo "[agenix] restarting units: ''${UNIQUE_UNITS[*]}"
          if [ "$(uname)" = "Darwin" ]; then
             for unit in "''${UNIQUE_UNITS[@]}"; do
               launchctl kickstart -k "gui/$(id -u)/$unit" || true
             done
          else
             systemctl --user try-restart "''${UNIQUE_UNITS[@]}"
          fi
        fi
      ''
    ]
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
          default = [ ];
          description = ''
            Other secrets on which this template depends.
          '';
        };
        restartUnits = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            List of units to restart when this template is updated.
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
        mode = mkOption {
          type = types.str;
          default = "0400";
        };
        path = mkOption {
          type = types.str;
          default = "${cfg.templateDir}/${config.name}";
          description = "The path where the generated file will be written to.";
        };
        placeholderMap = mkOption {
          type = types.attrsOf types.attrs;
          default = mapListOrAttrs (
            dep:
            dep
            // {
              placeholder = "<AGE_PLACEHOLDER:${builtins.hashString "sha256" dep.name}:${builtins.hashString "sha256" (toString dep.file)}:>";
            }
          ) config.dependencies;
          internal = true;
          readOnly = true;
          description = ''
            Placeholders to be used in the content script.
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
              deps = mapListOrAttrsToList (dep: dep.path) config.dependencies;
            };
          };
          description = "The path where the generated file will be written to.";
        };
        symlink = mkEnableOption "symlinking templates to their destination" // {
          default = true;
        };

      };

    }
  );

  secretType = types.submodule (
    {
      config,
      name,
      ...
    }:
    {
      options = {
        name = mkOption {
          type = types.str;
          default = name;
          description = ''
            Name of the file used in ''${cfg.secretsDir}
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
        symlink = mkEnableOption "symlinking secrets to their destination" // {
          default = true;
        };
      };
    }
  );

  mountingScript =
    let
      app = pkgs.writeShellApplication {
        name = "agenix-home-manager-mount-secrets";
        runtimeInputs =
          with pkgs;
          [
            coreutils
            diffutils
            gnused
          ]
          ++ lib.optionals stdenv.isLinux [ systemd ];
        text = ''
          ${newGeneration}
          ${installSecrets}
          exit 0
        '';
      };
    in
    lib.getExe app;

  userDirectory =
    dir:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
      baseDir =
        if isDarwin then "$(${lib.getExe pkgs.getconf} DARWIN_USER_TEMP_DIR)" else "\${XDG_RUNTIME_DIR}";
    in
    "${baseDir}/${dir}";

  userDirectoryDescription =
    dir:
    literalExpression ''
      "''${XDG_RUNTIME_DIR}"/''${dir} on linux or "$(getconf DARWIN_USER_TEMP_DIR)"/''${dir} on darwin.
    '';
in
{
  options.age = {
    package = mkPackageOption pkgs "age" { };

    secrets = mkOption {
      type = types.attrsOf secretType;
      default = { };
      description = ''
        Attrset of secrets.
      '';
    };

    templates = mkOption {
      type = types.attrsOf templateType;
      default = { };
      description = ''
        Attrset of templates.
      '';
    };

    identityPaths = mkOption {
      type = types.listOf types.path;
      default = [
        "${config.home.homeDirectory}/.ssh/id_ed25519"
        "${config.home.homeDirectory}/.ssh/id_rsa"
      ];
      defaultText = literalExpression ''
        [
          "''${config.home.homeDirectory}/.ssh/id_ed25519"
          "''${config.home.homeDirectory}/.ssh/id_rsa"
        ]
      '';
      description = ''
        Path to SSH keys to be used as identities in age decryption.
      '';
    };

    secretsDir = mkOption {
      type = types.str;
      default = userDirectory "agenix/secrets";
      defaultText = userDirectoryDescription "agenix/secrets";
      description = ''
        Folder where secrets are symlinked to
      '';
    };

    templateDir = mkOption {
      type = types.str;
      default = userDirectory "agenix/templates";
      defaultText = userDirectoryDescription "agenix/templates";
      description = ''
        Folder where templates are symlinked to.
      '';
    };

    ageMountPoint = mkOption {
      default = userDirectory "agenix.d";
      defaultText = userDirectoryDescription "agenix.d";
      description = ''
        Where secrets are created before they are symlinked to ''${cfg.secretsDir} and templates to ''${cfg.templateDir}.
      '';
    };
  };

  config = mkIf (cfg.secrets != { } || cfg.templates != { }) {
    assertions = [
      {
        assertion = cfg.identityPaths != [ ];
        message = "age.identityPaths must be set.";
      }
    ];

    systemd.user.services.agenix = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      Unit = {
        Description = "agenix activation";
      };
      Service = {
        Type = "oneshot";
        ExecStart = mountingScript;
      };
      Install.WantedBy = [ "default.target" ];
    };

    launchd.agents.activate-agenix = {
      enable = true;
      config = {
        ProgramArguments = [ mountingScript ];
        KeepAlive = {
          Crashed = false;
          SuccessfulExit = false;
        };
        RunAtLoad = true;
        ProcessType = "Background";
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/agenix/stdout";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/agenix/stderr";
      };
    };
  };
}
