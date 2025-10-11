{
  inputs,
  pkgs,
  system,
}:

let
  inherit (pkgs) lib;
  zen-browser = {
    aarch64-darwin = inputs.my-nur.packages.aarch64-darwin.zen-browser;
    x86_64-linux = inputs.zen-browser.packages.x86_64-linux.beta-unwrapped;
    aarch64-linux = inputs.zen-browser.packages.aarch64-linux.beta-unwrapped;
  };
  ghostty = {
    aarch64-darwin = pkgs.ghostty-bin;
    x86_64-linux = pkgs.ghostty;
    aarch64-linux = pkgs.ghostty;
  };
  pinentry = {
    aarch64-darwin = pkgs.pinentry_mac;
    x86_64-linux = pkgs.pinentry;
    aarch64-linux = pkgs.pinentry;
  };
in
(
  {

    kmonad = inputs.kmonad.packages."${system}".default;
    kanata = inputs.my-nur.packages."${system}".kanata;
    karabiner-dk = pkgs.karabiner-dk
    #.overrideAttrs
    # (attrs: {
    #   version = "5.0.0";
    #   src = pkgs.fetchurl {
    #     url = "https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice/releases/download/v5.0.0/Karabiner-DriverKit-VirtualHIDDevice-5.0.0.pkg";
    #     hash = "sha256-hKi2gmIdtjl/ZaS7RPpkpSjb+7eT0259sbUUbrn5mMc=";
    #   };
    # })
    ;
    yabai = pkgs.yabai.overrideAttrs (attrs: {
      src = inputs.yabai;
    });
    jankyborders = pkgs.jankyborders.overrideAttrs (attrs: {
      src = inputs.jankyborders;
    });
    input-leap = pkgs.input-leap.overrideAttrs (attrs: {
      # patches = [ ];
      src = inputs.input-leap;
    });

    inherit (inputs.hyprland.packages."${system}") hyprland xdg-desktop-portal-hyprland;
    ivy-fetch = pkgs.callPackage ../packages/ivy-fetch { };
    hln = pkgs.callPackage ../packages/hardlink.nix { };
    pinentry = pinentry."${system}";
    desktoppr = pkgs.callPackage ../packages/desktoppr.nix { };
    inherit (inputs.nixos-conf-editor.packages."${system}") nixos-conf-editor;
    #            nh = inputs.nh.packages."${system}".default;
    inherit (inputs.eww.packages.${system}) eww;
    inherit (inputs.rnix.packages."${system}") rnix-lsp;
    ghostty = ghostty."${system}";
    zen-browser = zen-browser."${system}";
    picom = pkgs.picom.overrideAttrs (attrs: {
      src = inputs.picom;
    });
    mopidy-tidal = pkgs.mopidy-tidal.overrideAttrs (_: {
      doCheck = false;
    });
    gst_all_1 = pkgs.gst_all_1 // {
      gst-plugins-rs = pkgs.gst_all_1.gst-plugins-rs.overrideAttrs (attrs: {
        doCheck = false;
      });
    };
  }
  // (lib.optionalAttrs (pkgs.stdenv.isLinux) {

    ipython = pkgs.ipython.overrideAttrs (attrs: {

      doCheck = false;
    });

    libsecret = pkgs.libsecret.overrideAttrs (attrs: {
      doCheck = false;
    });

  })
)
