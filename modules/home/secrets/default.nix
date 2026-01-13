{
  config,
  pkgs,
  lib,
  hostname,
  isInside,
  ...
}:

{

  age.secretsDir =
    if pkgs.stdenv.isDarwin then
      "${config.home.homeDirectory}/Library/agenix/secrets"
    else
      "${config.home.homeDirectory}/.config/agenix/secrets";
  age.ageMountPoint =
    if pkgs.stdenv.isDarwin then
      "${config.home.homeDirectory}/Library/agenix.d"
    else
      "${config.home.homeDirectory}/.config/agenix.d";
  age.templateDir =
    if pkgs.stdenv.isDarwin then
      "${config.home.homeDirectory}/Library/agenix/templates"
    else
      "${config.home.homeDirectory}/.config/agenix/templates";

  age.rekey.storageMode = "local";
  auscybernix.secrets.configId = "${
    if isInside then "inside-" else ""
  }${config.home.username}-${hostname}";
  age.templates."extra-nix-conf" = {
    dependencies = {
      inherit (config.age.secrets) github_token;
    };
    content =
      {
        pkgs,
        placeholders,
        ...
      }:
      ''
        access-tokens = github.com=${placeholders.github_token}
      '';

  };
  nix.extraOptions = ''
    !include "${config.age.templates."extra-nix-conf".path}"
        		'';
}
