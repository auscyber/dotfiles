{ den, ... }:
# Building and flashing ESPHome firmware from the laptop, as opposed to from
# the dashboard on secondpc (../services/homeassistant/esphome.nix).
#
# Verified on this flake's nixpkgs (1cabc318) for aarch64-darwin: `esphome`
# resolves to 2025.12.2 with `meta.broken = false`, and a dry-run build reports
# "these 149 paths will be fetched" and nothing to build -- i.e. Hydra already
# built the whole closure, platformio 6.1.18 and esptool 5.1.0 included, for
# this platform. So this is a plain `home.packages` entry and needs no
# devshell, no remote builder and no container.
#
# What the laptop can and cannot do with it:
#
#   esphome compile <node>.yaml   works. platformio downloads the ESP-IDF /
#                                 Arduino toolchain into ~/.platformio at first
#                                 run -- that part is NOT in the nix closure
#                                 and needs network, same as on the server.
#   esphome run / upload          works over serial, see the driver note below.
#   esphome run --device <host>   OTA, no serial at all. The nodes are on the
#                                 house LAN, so this is the path that works
#                                 from anywhere on the tailnet.
#
# --- serial on darwin ---
#
# macOS needs no udev equivalent: a USB-UART bridge appears as /dev/cu.* and
# any user can open it. Which bridges appear is the whole question, and it
# splits three ways:
#
#   * ESP32-S2/S3/C3/C6 native USB (CDC-ACM, no bridge chip) -- Apple's own
#     AppleUSBACM driver, works out of the box, /dev/cu.usbmodem*.
#   * CP210x (Silicon Labs) and FTDI -- Apple ships drivers for both,
#     /dev/cu.usbserial*. Also out of the box.
#   * CH340/CH341/CH9102 -- the cheap clone boards. macOS has NO driver, the
#     device simply never appears, and this is the one case that needs
#     software. Hence the cask.
#
# The cask installs a kernel extension, so it is not fully declarative: the
# first activation that installs it needs the extension approved in System
# Settings -> Privacy & Security and a reboot before /dev/cu.wchusbserial*
# shows up. Nothing here can do that step, and a board that enumerates on its
# own (the first two cases above) does not need it at all.
{
  den.aspects.esp-dev = {
    includes = [ den.aspects.homebrew ];

    brew.casks = [ "wch-ch34x-usb-serial-driver" ];

    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          esphome
          # Already in esphome's own closure, but as a python dependency rather
          # than on PATH. Named separately for the flashing steps esphome does
          # not wrap -- erasing flash, reading a chip id, recovering a node
          # whose firmware no longer talks to the dashboard.
          esptool
        ];
      };
  };
}
