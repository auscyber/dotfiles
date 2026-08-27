{
  den,
  inputs,
  ...
}:
{
  ff.emacs = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nixpkgs-stable.follows = "nixpkgs";
  };

  den.aspects.emacs = {
    overlays.emacs = inputs.emacs.overlays.default or (_: _: { });

    homeManager = { pkgs, ... }: {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsNativeComp or pkgs.emacs;
      };
      services.emacs.enable = true;
    };
  };
}
