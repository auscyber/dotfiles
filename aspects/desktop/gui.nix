{ den, ... }: {
  den.aspects.gui = {
    includes = [
      den.aspects.onepassword
      den.aspects.ghostty
    ];
    gui.nixos = {
      services.displayManager.ly = {
        enable = true;
        settings = {
          animation = "dur_file";
          dur_file_path = builtins.toString (
            builtins.path {
              name = "blackhole.dur";
              path = ./blackhole.dur;
            }
          );
          full_color = true;
        };
      };
    };
  };

  den.aspects.razer = {
    gui.nixos = { user, pkgs, ... }: {
      hardware.openrazer.enable = true;
      users.users.${user.name}.extraGroups = [ "openrazer" ];

    };

  };

  # Lets 8BitDo's Windows Firmware Updater tool (run under Wine) see the
  # controller/receiver without root, and provides the Wine + Mono runtime
  # it needs (it's a .NET Framework app).
  # https://gist.github.com/archeYR/d687de5e484ce7b45d6a94415a04f3dc
  den.aspects.eightbitdo = {
    gui.nixos = {
      services.udev.extraRules = ''
        # 8BitDo Ultimate Bluetooth Controller boot HID interface (shared by
        # several 8BitDo devices; re-check with lsusb if a different Product
        # ID shows up while the device is in bootloader mode).
        SUBSYSTEM=="hidraw", ATTRS{idProduct}=="3208", ATTRS{idVendor}=="2dc8", TAG+="uaccess"

        # Receiver's HID interface, exposed only while no controller is
        # connected; lets the updater auto-detect the receiver and put it in
        # bootloader mode for upgrading.
        SUBSYSTEM=="hidraw", ATTRS{idProduct}=="3109", ATTRS{idVendor}=="2dc8", TAG+="uaccess"
      '';
    };

    gui.homeManager = { pkgs, ... }: {
      # stableFull bundles wine-mono + wine-gecko.
      home.packages = [
        pkgs.wineWow64Packages.stableFull
        pkgs.winetricks
      ];
    };
  };

  den.aspects.auspc.includes = [
    den.aspects.razer
    den.aspects.eightbitdo
  ];
  den.aspects.auscyber.includes = [ den.aspects.eightbitdo ];
}
