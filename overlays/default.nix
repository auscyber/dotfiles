{
  self,
  inputs,
  lib,
  ...
}:
{
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.sops-nix.overlays.default
          self.overlays.default

        ];
      };
    };
  flake.overlays = {
    default = lib.composeManyExtensions [

      #        inputs.hyprpanel.overlay
      (final: prev: {
        lib = prev.lib.extend self.lib.overlay;
      })
      inputs.nur.overlays.default
      inputs.nix-cachyos-kernel.overlays.pinned
      #      inputs.my-nur.overlays.default
      #      (final: prev: {
      #        nur = prev.nur // {
      #          repos = prev.nur.repos // {
      #            AusCyber = inputs.my-nur.packages."${prev.stdenv.hostPlatform.system}";
      #          };
      #        };
      #
      #      })
      inputs.attic.overlays.default
      inputs.hyprland-plugins.overlays.default
      inputs.niri.overlays.niri
      inputs.emacs.overlays.default
      inputs.rust-overlay.overlays.default
      inputs.nvfetcher.overlays.default
      #      inputs.lix-module.overlays.default
      inputs.neovim.overlays.default
      (
        final: prev:
        import ./literal.nix {
          pkgs = prev;
          system = final.stdenv.hostPlatform.system;
          inherit inputs;
        }
      )

    ];

  };
}
