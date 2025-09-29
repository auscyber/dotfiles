{
  inputs,
  pkgs,
  system,
}:

let

  zen-browser = {
    aarch64-darwin = pkgs.nur.repos.AusCyber.zen-browser;
    x86_64-linux = inputs.zen-browser.packages.x86_64-linux.beta-unwrapped;
  };
  ghostty = {
    aarch64-darwin = pkgs.ghostty-bin;
    x86_64-linux = pkgs.ghostty;
  };
  pinentry = {
    aarch64-darwin = pkgs.pinentry_mac;
    x86_64-linux = pkgs.pinentry;
  };
in
{

  kmonad = inputs.kmonad.packages."${system}".default;
  kanata = pkgs.kanata.overrideAttrs (attrs: {
    src = inputs.kanata;
    doInstallCheck = false;
    cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
      inherit (attrs)
        postUnpack

        ;
      src = inputs.kanata;
      copyLockfile = false;
      hash = "sha256-HrEwD6jknATyNH1cw8wzM2l5wjs/XFDXFf1q0apGN5E=";
    };

  });
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
  agenix = inputs.agenix.packages."${system}".default;
  inherit (inputs.eww.packages.${system}) eww;
  inherit (inputs.rnix.packages."${system}") rnix-lsp;
  ghostty = ghostty."${system}";
  zen-browser = zen-browser."${system}";
  picom = pkgs.picom.overrideAttrs (attrs: {
    src = inputs.picom;
  });
}
