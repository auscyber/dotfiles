{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  imports = [inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime inputs.nixos-hardware.nixosModules.common-pc inputs.nixos-hardware.nixosModules.common-pc-ssd];
  systemd.services."systemd-suspend" = {
	serviceConfig = {
	Environment = ''"SYSTEMD_SLEEP_FREEZE_USER_SESSIONS=false"'';
	};
  };
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    #    fullCompositionPipeline = true;
    modesetting.enable = true;
    open = true;
    powerManagement.enable = true;
    powerManagement.finegrained = false;
    nvidiaSettings = true;
    gsp.enable = true;
  };
  boot.blacklistedKernelModules = [ "amdgpu" "nouveau"];

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
  boot.extraModulePackages = with config.boot.kernelPackages; [
	nct6687d acpi_call

  ];
  hardware.enableAllFirmware = true;
  hardware.enableRedistributableFirmware = true;
boot.extraModprobeConfig = ''
options nvidia_modeset vblank_sem_control=0 nvidia NVreg_PreserveVideoMemoryAllocations=1 NVreg_TemporaryFilePath=/var/tmp
'';

  boot.kernelParams = [
    "nvidia-drm.modeset=1"
    "nvidia.NVreg_UsePageAttributeTable=1"

  ];

}
