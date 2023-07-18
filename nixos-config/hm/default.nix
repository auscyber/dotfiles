{ config, pkgs, system, lib, modulesPath, ... }:
{
  config = {
    manual.manpages.enable = false;
    programs = {
      command-not-found.enable = true;
      direnv = {
        enable = true;
        nix-direnv = {
          enable = true;
        };
      };
      home-manager.enable = true;
    };
    services.lorri = {
      enable = false;
    };
    home.packages = with pkgs; [
      rnix-lsp
      nixfmt
      (pkgs.writeTextFile {
        name = "packagenames";
        destination = "/etc/packagenames";
        text = config.packagenames;
      })
      (pkgs.writeTextFile {
        name = "packagecount";
        destination = "/etc/packagecount";
        text = builtins.toString config.packagecount;
      })
    ];

    xdg.configFile."nvim/lua/compiler.lua".text = ''
      			return "${pkgs.stdenv.cc}/bin/cc"
            	'';
    xdg.configFile."nix/nix.conf".text =
      let
        cachixes-to-pairs = inp: builtins.map ({ name, sha256 }: { url = "${name}.cachix.org"; inherit sha256; }) inp ++ [{ url = "cache.nixos.org"; sha256 = "6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="; }];
        urls-to-struct = builtins.map ({ url, sha256 }: { substituters = "https://${url}"; trusted-public-keys = "${url}-1:${sha256}"; });
        convert = inp:
          let res1 = lib.zipAttrs (urls-to-struct (cachixes-to-pairs inp));
          in lib.concatStringsSep "\n" (lib.mapAttrsToList (name: str: "${name} = ${lib.concatStringsSep " " str}") res1);
        lol = x: builtins.trace x x;

      in
      ''
        ${convert [
          { name = "emacsng"; sha256 = "i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI="; }
          { name = "cm-idris2-pkgs"; sha256 = "YB2oJSEsD5oMJjAESxolC2GQtE6B5I6jkWhte2gtXjk="; }
          { name = "nix-community"; sha256 = "mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="; }
        ]}
        experimental-features = nix-command flakes
      ''
    ;
    packagecount = builtins.length config.home.packages;
    packagenames = lib.concatStringsSep "\n" (lib.unique (builtins.map (p: "${p.name}") config.home.packages));
  };
  options = with lib;{
    packagenames = mkOption
      {
        type = types.str;
        default = "";
      };
    packagecount = mkOption {
      type = types.int;
      default = 0;
      description = "package count";
    };
  };


}
