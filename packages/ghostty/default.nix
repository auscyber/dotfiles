{
  lib,
  callPackage,
  darwin,
  xcbuild,
  stdenv,
  curl, unzip,
  zig,
  fetchurl,
  source,
  gettext,
  llvmPackages_latest,
  darwinMinVersionHook,
  xcodeenv,
  doxygen,
  jq,
  minisign,
  ncurses,
  pandoc,
  rsync,
  pkg-config,
  scdoc,
  zip,
  libGL,
  apple-sdk_26

}:
let sparkle = stdenv.mkDerivation rec {
  pname = "sparkle";
  version = "2.7.3";
SPARKLE_VERSION = "2.7.3";
buildInputs = [ unzip ];
  src = fetchurl {
    url = "https://github.com/sparkle-project/Sparkle/releases/download/${SPARKLE_VERSION}/Sparkle-for-Swift-Package-Manager.zip";
	sha256 = "sha256-LgvxWudME+exa3bjS6Vv46RGg+NUcymCJfSiosJ0Mpo=";
  };
  unpackPhase = ''
	unzip $src
	'';
	installPhase = ''
	mkdir -p $out/bin
	mkdir -p $out/Sparkle.xcframework
	ls -la $sourceRoot
	cp -r $sourceRoot/bin $out/
	cp -rp Sparkle.xcframework $out/
	'';

};
in

stdenv.mkDerivation rec {
 inherit (source) src pname version;
  # . . .

  nativeBuildInputs = [ zig curl unzip
  doxygen
        jq
        llvmPackages_latest.llvm
        minisign
        ncurses
        pandoc
        pkg-config
#		darwin.xcode_26_2_Apple_silicon
        scdoc
        zig
		xcbuild
        zip
#(xcodeenv.composeXcodeWrapper {
#  xcodeBaseDir = "/Applications/Xcode.app";
#  versions = ["26.2"];
#  })
		gettext

  apple-sdk_26
#		apple-sdk
  ];
  buildInputs = [ rsync libGL sparkle (darwinMinVersionHook "26.0")  apple-sdk_26 ];
  patches = [./framework.patch];

  preBuild = ''
	mkdir -p "$sourceRoot"
	mkdir -p "$sourceRoot/Sparkle.xcframework"
	ls -la "${sparkle}/Sparkle.xcframework"
	ln -sf "${sparkle}/Sparkle.xcframework" "$sourceRoot/Sparkle.xcframework"

'';



__impureHostDeps = lib.optionals stdenv.isDarwin [
#    "/System/Library/CoreServices/SystemVersion.plist"
#    "/System/Library/CoreServices/.SystemVersionPlatform.plist"
#	"/System/Library/Frameworks"
#	"/Library/Developer/CommandLineTools"
#	"/bin/sh"
#	"/dev/random"
#	"/dev/urandom"
#"/Applications/Xcode.app"
  ];
  postPatch = ''
#   ln -s ${callPackage ./deps.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
	rsync -lD ${./ir} $ZIG_GLOBAL_CACHE_DIR/o
	ls -l $ZIG_GLOBAL_CACHE_DIR/o/
	'';




  zigBuildFlags = [  "-Doptimize=ReleaseFast" "-Demit-macos-app=false -Dxcframework-target=native -Drenderer=opengl"];


  dontUseZigCheck = true;

  # . . .
}
