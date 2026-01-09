# simple.nix
  {stdenv, makeWrapper, gnupg,rsync}:
  stdenv.mkDerivation {
    name = "gpg-agent-wrapper";

    src = ./.;
	nativeBuildInputs = [ rsync makeWrapper gnupg ];

    # Use $CC as it allows for stdenv to reference the correct C compiler
    buildPhase = ''
      $CC wrapper.c -o wrapper
    '';
	installPhase = ''
#	  cp -r ${gnupg}/bin $out
mkdir -p $out/bin
cp wrapper $out/bin/gpg-agent

	  wrapProgram $out/bin/gpg-agent --add-flags "${gnupg}/bin/gpg-agent"
	'';

  }
