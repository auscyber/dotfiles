{
  self,
  inputs,
  lib,
  ...
}:
{
  flake.overlays = {
    default = lib.composeManyExtensions [
      #        inputs.hyprpanel.overlay

      inputs.nur.overlays.default
      inputs.my-nur.overlays.default
      #      (final: prev: {
      #        nur = prev.nur // {
      #          repos = prev.nur.repos // {
      #            AusCyber = inputs.my-nur.packages."${prev.stdenv.hostPlatform.system}";
      #          };
      #        };
      #
      #      })

      inputs.emacs.overlays.default
      inputs.rust-overlay.overlays.default
      inputs.lix-module.overlays.default
      inputs.neovim.overlays.default
      (
        final: prev:
        let
          inherit (final.stdenv.hostPlatform) system;

          zen-browser = {
            aarch64-darwin = prev.nur.repos.AusCyber.zen-browser;
            x86_64-linux = inputs.zen-browser.packages.x86_64-linux.twilight;
          };
          ghostty = {
            aarch64-darwin = prev.nur.repos.AusCyber.ghostty;
            x86_64-linux = prev.ghostty;
          };
          pinentry = {
            aarch64-darwin = prev.pinentry_mac;
            x86_64-linux = prev.pinentry;
          };
        in
        {

          kmonad = inputs.kmonad.packages."${system}".default;
          karabiner-dk = prev.karabiner-dk.overrideAttrs (attrs: {
            version = "5.0.0";
            src = prev.fetchurl {
              url = "https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/releases/download/v5.0.0/Karabiner-DriverKit-VirtualHIDDevice-5.0.0.pkg";
              hash = "sha256-hKi2gmIdtjl/ZaS7RPpkpSjb+7eT0259sbUUbrn5mMc=";
            };
          });
          yabai = prev.yabai.overrideAttrs (attrs: {
            src = inputs.yabai;
          });
          jankyborders = prev.jankyborders.overrideAttrs (attrs: {
            src = inputs.jankyborders;
          });
          input-leap = prev.input-leap.overrideAttrs (attrs: {
            # patches = [ ];
            src = inputs.input-leap;
          });

          inherit (inputs.hyprland.packages."${system}") hyprland xdg-desktop-portal-hyprland;
          ivy-fetch = prev.callPackage ../packages/ivy-fetch { };
          hln = prev.callPackage ../packages/hardlink.nix { };
          pinentry = pinentry."${system}";
          desktoppr = prev.callPackage ../packages/desktoppr.nix { };
          inherit (inputs.nixos-conf-editor.packages."${system}") nixos-conf-editor;
          #            nh = inputs.nh.packages."${system}".default;
          agenix = inputs.agenix.packages."${system}".default;
          inherit (inputs.eww.packages.${system}) eww;
          inherit (inputs.rnix.packages."${system}") rnix-lsp;
          ghostty = ghostty."${system}";
          zen-browser = zen-browser."${system}";
          picom = prev.picom.overrideAttrs (attrs: {
            src = inputs.picom;
          });
          #            idris2 = idris2.packages."${system}".idris2;
          #            wezterm = (masterp {inherit system;}).wezterm;
          #              discord = (import master { inherit system config; }).discord;
          #wezterm = prev.wezterm.overrideAttrs (attrs: rec {
          #  src = inputs.wezterm;
          #  cargoDeps = attrs.cargoDeps.overrideAttrs (cattrs: {
          #    inherit src;
          #    outputHash =
          #      "sha256-iNv9JEu1aQBxhwlugrl2GdoSvF9cYgM6TXBqamrPjFo=";
          #  });
          #});

          inherit (final.idris2Pkgs) idris2;
          idris2Pkgs = inputs.idris2-pkgs.packages."${system}";
          #            minecraft-server = (import master { inherit system config; }).minecraft-server;
        }
      )
    ];

  };
}
