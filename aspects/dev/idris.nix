{
  den,
  inputs,
  ...
}:
{
  den.aspects.idris = {
    # Declared on the aspect, not the file: the input set follows which
    # hosts pull this aspect in, and so does its partition.
    flake-file = _: {
      inputs.idris2Packages.url = "github:mattpolzin/nix-idris2-packages";
    };

    # Multiplex idris2-lsp through lspmux, co-located with the language aspect: this
    # contributes the entry to the `lsp-servers` class, which the forward on
    # den.aspects.lspmux turns into `pkgs.lspmuxed.idris2_lsp` and enables in nvim.
    includes = [ den.aspects.lspmux ];
    lsp-servers = { pkgs, ... }: {
      idris2_lsp = {
        package = pkgs.idris2Packages.idris2Lsp;
        exe = "idris2-lsp";
        nvim = false;
        extensionToLanguage = {
          ".idr" = "idris2";
          ".lidr" = "idris2";
        };
      };
    };

    nix.settings = {
      trusted-substituters = [
        "https://gh-nix-idris2-packages.cachix.org"
      ];
      trusted-public-keys = [
        "gh-nix-idris2-packages.cachix.org-1:iOqSB5DrESFT+3A1iNzErgB68IDG8BrHLbLkhztOXfo="
      ];
    };
    nvim =
      {
        config,
        pkgs,
        ...
      }:
      {
        plugins.idris2 = {
          enable = true;
          settings.server = {
            cmd = [ "${pkgs.lspmuxed.idris2_lsp}/bin/idris2-lsp" ];
          };
        };
      };

    overlays = { inputs', ... }: {
      idris2Packages = final: prev: {
        idris2Packages = inputs'.idris2Packages.packages.idris2Packages;
      };
    };
    homeManager = { pkgs, ... }: {
      home.packages = with pkgs.idris2Packages; [
        idris2
        pack
      ];
    };
  };
}
