{ den, ... }:
{
  flake-file.inputs.emacs.url = "github:nix-community/emacs-overlay";

  den.aspects.emacs = {
    overlays.emacs = inputs: inputs.emacs.overlays.default or (_: _: { });

    homeManager =
      { pkgs, ... }:
      {
        programs.emacs = {
          enable = true;
          package = pkgs.emacsNativeComp or pkgs.emacs;
        };
        services.emacs.enable = true;
      };
  };
}
