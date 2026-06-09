{ lib, ... }:
{

  den.aspects.auspc = {
    overlays = {
      linuxZenMuQSS = self: super: {
        linuxZenWMuQSS =
          let
            llvmKernelStdenv = super.stdenvAdapters.overrideInStdenv super.llvmPackages.stdenv [
              super.llvm
              super.lld
              #          super.llvmPackages.clang-unwrapped
              super.llvmPackages.clang
            ];
            stdenv = self.ccacheStdenv.override {
              stdenv = llvmKernelStdenv;
            };
          in
          (super.linuxPackagesFor (
            super.linux_zen.override (prev: {
              #          stdenv = llvmKernelStdenv;
              inherit stdenv;
              buildPackages = super.buildPackages // {
                stdenv = stdenv;
              };

              ignoreConfigErrors = true;

              kernelPatches = [
                {
                  name = "llvm-lto";
                  patch = null;
                  structuredExtraConfig = with lib.kernel; {
                    # We are not a k8s server
                    SCHED_ALT = yes;
                    CPU_MITIGATIONS = lib.mkForce no;

                    # Clang options require a lot of extra config
                    CC_IS_CLANG = lib.mkForce yes;
                    LTO = lib.mkForce yes;
                    LTO_CLANG = lib.mkForce yes;
                    # full LTO is much more expsneive
                    LTO_CLANG_THIN = lib.mkForce yes;
                  };
                  ignoreConfigErrors = true;

                }
              ];
            })
          )).extend
            (
              self: super: {
                alx-wol = self.callPackage ../packages/alx-wol.nix {
                };
              }
            );
      };
    };

    nixos = { pkgs, config, ... }: {

      boot.kernelPackages = pkgs.linuxZenWMuQSS;
      boot.extraModulePackages = with config.boot.kernelPackages; [ alx-wol ];
    };
  };
}
