{ config
, lib
, pkgs
, ...
}: {
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    #    fullCompositionPipeline = true;
    modesetting.enable = true;
    open = false;
    powerManagement.enable = true;
    nvidiaSettings = true;
  };
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [ mesa ];
  };
  boot.initrd.kernelModules = [
    "nvidia"
    "nvidia_modeset"
    "nvidia_uvm"
    "nvidia_drm"
  ];

  boot.kernelParams = [ "nvidia-drm.modeset=1" ];


}
