{
  lib,
  stdenv,
  fetchFromGitHub,
  kernel,
  kernelModuleMakeFlags,
  kmod,
}:

let
  alxRepo = fetchFromGitHub {
    owner = "AndiWeiss";
    repo = "alx-wol";
    rev = "master";
    hash = "sha256-Sfq1vnf+UXNtSSBjGPe0Ignu6G8clp4RrVpeT8F5Xw8=";
  };
in
stdenv.mkDerivation rec {
  pname = "alx-wol";
  version = kernel.version;

  src = kernel.src;

  hardeningDisable = [
    "pic"
    "format"
  ];
  nativeBuildInputs = [ kmod ] ++ kernel.moduleBuildDependencies;

  postUnpack = ''
    # Move the alx driver to the top level to simplify patching and building

  '';

  # Move the alx driver to the top level to simplify patching and building
  modulePath = "drivers/net/ethernet/atheros/alx";
  enableParallelBuilding = true;
  kernel_dev = kernel.dev;

  patchPhase = ''
    runHook prePatch
    cp -r $src/${modulePath} ./


    patch_file=$(${stdenv.shell} ${alxRepo}/scripts/read_tag.sh "${kernel.version}" patches 1 ${alxRepo}/sources.txt)

    echo "Applying patch: $patch_file"
    if [ -z "$patch_file" ]; then
      echo "No patch found for kernel version ${kernel.version}"
      exit 1
    fi


    # The patches are relative to the 'alx' directory parent (e.g. v6.13/alx/...)
    # We moved 'alx' to the current directory.
    cd ${modulePath}
    patch -p2 < "${alxRepo}/$patch_file"
    runHook postPatch
  '';

  makeFlags = kernelModuleMakeFlags ++ [
    "-C"
    "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "KVER=${kernel.modDirVersion}"
    "KSRC=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    #    "INSTALL_MOD_PATH=${placeholder "out"}"
    #    "INSTALL_MOD_DIR=${modulePath}"
    "M=$(PWD)"
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
    maintainers = [ ];
    platforms = platforms.linux;
  };
}
