{
  config,
    pkgs,
  lib,
  hostname,
  isInside,
  ...
}:

{

  age.secretsDir = if pkgs.stdenv.isDarwin then "${config.home.homeDirectory}/Library/agenix/secrets" else "${config.home.homeDirectory}/.config/agenix/secrets";
  age.rekey.storageMode = "local";
  age.rekey.localStorageDir = ../../.. + "/secrets/rekeyed/${if isInside then "inside-" else ""}${config.home.username}-${hostname}";

  age.secrets."extra-nix-conf" = {
    rekeyFile = ./extra-nix-conf.age;
    generator = {
      dependencies = {
        inherit (config.age.secrets) github_token;
      };
      script =
        {
          pkgs,
          lib,
          decrypt,
          deps,
          ...
        }:
        ''
          printf 'access-tokens = github.com=%s' $(${decrypt} ${lib.escapeShellArg deps.github_token.file})
        '';

    };
  };
  nix.extraOptions = ''
    !include "${config.age.secrets."extra-nix-conf".path}"
        		'';
}
