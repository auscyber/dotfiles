{
  lib,
  stdenv,
  fetchFromGitHub,
  kernel,
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
  nativeBuildInputs = kernel.moduleBuildDependencies;

  # Move the alx driver to the top level to simplify patching and building
  prePatch = ''
    mv drivers/net/ethernet/atheros/alx .
  '';

  patchPhase = ''
    runHook prePatch

    patch_file=$(${stdenv.shell} ${alxRepo}/scripts/read_tag.sh "${kernel.version}" patches 1 ${alxRepo}/sources.txt)

    echo "Applying patch: $patch_file"
    if [ -z "$patch_file" ]; then
      echo "No patch found for kernel version ${kernel.version}"
      exit 1
    fi

    # The patches are relative to the 'alx' directory parent (e.g. v6.13/alx/...)
    # We moved 'alx' to the current directory.
    patch -p1 < "${alxRepo}/$patch_file"
  '';

  preBuild = ''
    cd alx
  '';

  makeFlags = [
    "-C"
    "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "M=$(PWD)"
    "modules"
    "CONFIG_ALX=m"
  ];

  installPhase = ''
    install -D alx.ko $out/lib/modules/${kernel.modDirVersion}/kernel/drivers/net/ethernet/atheros/alx/alx.ko
  '';

  meta = with lib; {
    description = "Atheros ALX Ethernet driver with WOL support";
    homepage = "https://github.com/AndiWeiss/alx-wol";
    license = licenses.gpl2;
    maintainers = [ ];
    platforms = platforms.linux;
  };
}
