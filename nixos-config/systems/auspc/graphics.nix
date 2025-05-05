{ config
, lib
, pkgs
, ...
}: {
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    #    fullCompositionPipeline = true;
    modesetting.enable = true;
    open = false;
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    nvidiaSettings = true;
    gsp.enable = true;
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
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;

  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "nvidia.NVreg_UsePageAttributeTable=1"


  ];


}
