{
  lib,
  stdenv,
  fetchFromGitHub,
  kernel,
  kernelModuleMakeFlags,
  kmod,
}:

stdenv.mkDerivation rec {
  pname = "alx-wol";
  version = kernel.version;

  src = fetchFromGitHub {
    owner = "AndiWeiss";
    repo = "alx-wol";
    rev = "master";
    hash = "sha256-Sfq1vnf+UXNtSSBjGPe0Ignu6G8clp4RrVpeT8F5Xw8=";
  };

  hardeningDisable = [
    "pic"
    "format"
  ];
  nativeBuildInputs = [ kmod ] ++ kernel.moduleBuildDependencies;

  # Move the alx driver to the top level to simplify patching and building
  modulePath = "drivers/net/ethernet/atheros/alx";
  enableParallelBuilding = true;
  kernel_dev = kernel.dev;

  patchPhase = ''
    runHook prePatch


    patch_file=$(${stdenv.shell} $src/scripts/read_tag.sh "${kernel.version}" patches 1 $src/sources.txt)

    echo "Applying patch: $patch_file"
    if [ -z "$patch_file" ]; then
      echo "No patch found for kernel version ${kernel.version}"
      exit 1
    fi


    # The patches are relative to the 'alx' directory parent (e.g. v6.13/alx/...)
    # We moved 'alx' to the current directory.
    cp -r --no-preserve=all ${kernel.src}/${modulePath} .
    ls -la .
    patch -p1 < "$src/$patch_file"
    runHook postPatch
  '';

  makeFlags = kernelModuleMakeFlags ++ [
    "-C"
    "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "KVER=${kernel.modDirVersion}"
    "KSRC=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    #    "INSTALL_MOD_PATH=${placeholder "out"}"
    #    "INSTALL_MOD_DIR=${modulePath}"
    "M=$(PWD)/alx"
  ];

  installFlags = makeFlags ++ [
    "INSTALL_MOD_PATH=${placeholder "out"}"
    "INSTALL_MOD_DIR=${modulePath}"

  ]

  ;
  installTargets = [
    "modules_install"
  ];

  meta = with lib; {
    description = "Atheros ALX Ethernet driver with WOL support";
    homepage = "https://github.com/AndiWeiss/alx-wol";
    license = licenses.gpl2;
    maintainers = with lib.maintainers; [ auscyber ];
    platforms = platforms.linux;
  };
}
