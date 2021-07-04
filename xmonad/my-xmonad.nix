{ mkDerivation, base, containers, dbus, lib, microlens
, microlens-th, process, utf8-string, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "my-xmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers dbus microlens microlens-th process utf8-string X11
    xmonad xmonad-contrib
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
