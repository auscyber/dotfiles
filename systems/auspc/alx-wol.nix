{
  lib,
  stdenv,
  fetchurl,
  fetchpatch,
  kernel,
}:

stdenv.mkDerivation rec {
  pname = "alx-wol-dkms";
  version = "6";

  srcs = {
    alx_h = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/alx.h";
      sha256 = "0ac6445e832c3413be3887917203699139ec05553270c5006b5a33ba5e2a158d";
    };
    ethtool_c = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/ethtool.c";
      sha256 = "a0df2f5f93253a7dde775479142bf3eb209b5268ad124f200915736de7cf5063";
    };
    hw_c = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/hw.c";
      sha256 = "bf9ac0fa98031523e05d5d62c08ee116fd4a437f66538c95eac18adda10ff89b";
    };
    hw_h = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/hw.h";
      sha256 = "fcc0306b37382cf8e2f75eafb6bd8696a4a5e360110e5cfe800485f4eebe55aa";
    };
    main_c = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/main.c";
      sha256 = "2bd9ee44c72a657007a9bbaef332b9435e949295773733d0ca96cea8a69e075f";
    };
    reg_h = fetchurl {
      url = "https://raw.githubusercontent.com/archlinux/linux/v6.1-arch1/drivers/net/ethernet/atheros/alx/reg.h";
      sha256 = "88a23ab8e6fe814efe81910bd8806f5988d367a628458154edaebdf4ccbe4902";
    };
    patch_wol = fetchpatch {
      url = "https://raw.githubusercontent.com/MarkWalters-dev/aur/master/alx-wol-dkms/0001-drivers-net-alx-Re-enable-WoL-functionality.patch";
      sha256 = "98f1ad3377a1b0a96dfe7b71eb6dedc0f9779033c59f5dc093b4a7779d270a89";
    };
    patch_version = fetchpatch {
      url = "https://raw.githubusercontent.com/MarkWalters-dev/aur/master/alx-wol-dkms/0002-net-alx-Add-MODULE_VERSION-to-fix-dkms-override.patch";
      sha256 = "560783c1d0cd19859047360c317312f23cf1caabbd9d15aa101e98d3219da9a5";
    };
  };

  sourceRoot = ".";
  nativeBuildInputs = kernel.moduleBuildDependencies;

  # Prepare sources and apply patches
  postPatch = ''
        mkdir workdir
        cd workdir
    	cp ${srcs.alx_h} ${srcs.ethtool_c} ${srcs.hw_c} ${srcs.hw_h} ${srcs.main_c} ${srcs.reg_h} .

        patch -p6 < ../${srcs.patch_wol}
        patch -p6 < ../${srcs.patch_version}
  '';

  makeFlags = kernel.makeFlags ++ [
    "-C"
    "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "M=$(pwd)/workdir"
  ];

  buildFlags = [ "modules" ];

  installPhase = ''
    local dest=$out/lib/modules/${kernel.modDirVersion}/extra
    mkdir -p $dest
    cp workdir/*.ko $dest
    depmod -a ${kernel.modDirVersion}
  '';

  meta = with lib; {
    description = "The alx kernel module with a patch enabling WoL applied";
    homepage = "https://bugzilla.kernel.org/show_bug.cgi?id=61651";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
  };
}
