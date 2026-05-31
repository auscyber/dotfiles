{
  pkgs,
  crane,
  rev ? "dirty",
  use-nom ? true,
  nix-output-monitor ? null,
}:
assert use-nom -> nix-output-monitor != null;
let
  lib = pkgs.lib;
  craneLib = crane.mkLib pkgs;
  runtimeDeps = lib.optionals use-nom [ nix-output-monitor ];
  cargoToml = lib.importTOML ./Cargo.toml;
  src = lib.fileset.toSource {
    root = ./.;
    fileset = craneLib.fileset.commonCargoSources ./.;
  };
  skipTests = lib.concatStringsSep " " (
    [
      "--skip test_get_build_image_variants_expression"
      "--skip test_get_build_image_variants_file"
      "--skip test_get_build_image_variants_flake"
    ]
    ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
      "--skip test_build_sudo_cmd_basic"
      "--skip test_build_sudo_cmd_with_preserve_vars"
      "--skip test_build_sudo_cmd_with_preserve_vars_disabled"
      "--skip test_build_sudo_cmd_with_set_vars"
      "--skip test_build_sudo_cmd_force_no_stdin"
      "--skip test_build_sudo_cmd_with_remove_vars"
      "--skip test_build_sudo_cmd_with_askpass"
      "--skip test_build_sudo_cmd_env_added_once"
      "--skip test_elevation_strategy_passwordless_resolves"
      "--skip test_build_sudo_cmd_with_nix_config_spaces"
    ]
  );
  commonArgs = {
    inherit src;
    pname = "nh";
    version = "${cargoToml.workspace.package.version}-${rev}";
    strictDeps = true;
    cargoExtraArgs = "--workspace";
    buildInputs = lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.libiconv ];
    nativeCheckInputs = lib.optionals (!pkgs.stdenv.hostPlatform.isDarwin) [ pkgs.sudo ];
    env.NH_REV = rev;
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
in
craneLib.buildPackage (
  commonArgs
  // {
    inherit cargoArtifacts;

    nativeBuildInputs = [
      pkgs.installShellFiles
      pkgs.makeBinaryWrapper
    ];

    cargoTestExtraArgs = "-- ${skipTests}";

    postInstall = ''
      # Generate shell completions and the man page using the installed xtask.
      $out/bin/xtask dist

      # Remove xtask from the final output; it is only needed during install.
      rm $out/bin/xtask

      installShellCompletion --cmd nh ./comp/*.{bash,fish,zsh,nu}
      installManPage ./man/nh.1
    '';

    postFixup = ''
      wrapProgram $out/bin/nh \
        --prefix PATH : ${lib.makeBinPath runtimeDeps}
    '';

    nativeInstallCheckInputs = [ pkgs.versionCheckHook ];
    doInstallCheck = false;
    versionCheckProgram = "${placeholder "out"}/bin/nh";
    versionCheckProgramArg = "--version";

    meta = {
      description = "Yet another nix cli helper";
      homepage = "https://github.com/nix-community/nh";
      license = lib.licenses.eupl12;
      mainProgram = "nh";
      maintainers = with lib.maintainers; [
        drupol
        faukah
        NotAShelf
        viperML
      ];
    };
  }
)
