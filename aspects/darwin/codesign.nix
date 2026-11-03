# Signs sketchybar/paneru/coolabah/kanata so their TCC grants survive a rebuild.
#
# Two things invalidate a grant, and signed bundles answer both:
#
#   * the path. TCC keys a bare executable by absolute path, and every rebuild
#     moves the store path. A bundle is keyed by its identifier instead, so each
#     program becomes `libexec/<name>.app` (`in.ivymect.<name>`) and may live
#     anywhere -- no fixed path, nothing to plant, nothing to wait for at boot.
#
#   * the recorded `csreq`. An ad-hoc signature pins a `cdhash` that dies on any
#     content change; a real certificate pins `certificate root = H"..."`, which
#     survives any rebuild. A new certificate is a new hash, so it is minted once.
#
# `bin/<name>` stays the entry point in either shape a package has:
#
#   a Mach-O              signed as the bundle's executable; `bin/<name>` links
#                         to it.
#   a makeWrapper script  its `.<name>-wrapped` payload is the bundle's
#                         executable, and upstream's script (which exports
#                         LUA_PATH, PATH and the rest) is kept verbatim with only
#                         its exec target repointed into the bundle.
#
# Signing produces a separate WRAPPER derivation (`mkSignedWrapper`), not an
# `overrideAttrs` on the package. The package itself builds purely and
# substitutes like anything else; only the wrapper reaches the autopen socket
# and is `allowSubstitutes = false`, and all it does is copy `bin/` through a
# signature. That keeps signing off the critical path of every Rust and C build
# in the closure, and means a machine without the daemon fails on a few tiny
# derivations rather than being unable to build the host at all.
#
# The key is held by `autopen serve` (./autopen.nix); a build signs through its
# socket with a remote-key handle and never sees key material. Run
# `gen-secrets` once to mint the key and its self-signed certificate.
{
  den,
  lib,
  ...
}:
let
  autopen = den.lib.autopen;

  subject = autopen.commonName;

  # Entitlements, keyed by the nixpkgs attribute being signed. A plain attrset
  # rather than a `config` option on purpose: signing happens in an overlay, and
  # an overlay that read `config` would close a fixpoint through `pkgs` --
  # `nixpkgs.overlays` is an input to the very `pkgs` that evaluates `config`.
  # `mkSignedWrapper` takes `entitlements` as an argument, so anything building a
  # wrapper by hand can pass whatever it likes; this map is only the defaults for
  # the attributes the overlay signs.
  #
  # Empty for all four today: they are TCC clients, and TCC is keyed by the
  # recorded `csreq` and the executable path, not by entitlements. Kept because
  # entitlements are the thing you reach for the moment one of these needs a
  # capability the ad-hoc signature cannot express.
  entitlementsFor = { };

  # A signed WRAPPER, not a re-signed package. `sign` used to be an
  # `overrideAttrs` that pushed `__noChroot` and `allowSubstitutes = false` onto
  # the package itself, which meant sketchybar/paneru/kanata could never be
  # substituted by anything, on any machine -- every host recompiled all of them,
  # Rust included, and none of them could be built at all without the identity
  # on disk.
  #
  # Here the package builds normally, purely, and substitutes like anything else.
  # Only this `runCommand` is impure, and all it does is copy `bin/` through a
  # signature:
  #
  #   libexec/<name>.app  the signed bundle
  #   bin/<name>          the entry point into it
  #   everything else     symlinked straight through from the original
  #
  # `override`/`overrideAttrs` re-enter `mkSignedWrapper`, which is what makes
  # this usable as a module's `package`. paneru's module computes
  # `finalPackage = wrapPaneru (cfg.package.override { enableLua = true; ... })`
  # (nix/_paneru-common.nix), so a wrapper that merely FORWARDED `override` would
  # hand back the unsigned overridden package and drop the signature silently.
  # Re-signing on override means the signing survives whatever the module does to
  # the package afterwards, which is the property the old in-place overlay had
  # and the reason it was written that way.
  mkSignedWrapper =
    pkgs:
    {
      package,
      entitlements ? { },
    }:
    let
      entitlementsFile =
        if entitlements == { } then
          null
        else
          pkgs.writeText "${package.pname or package.name}-entitlements.plist" (
            lib.generators.toPlist { } entitlements
          );
      entitlementsArg = lib.optionalString (
        entitlementsFile != null
      ) "--entitlements-xml-path ${entitlementsFile}";
    in
    let
      signer = autopen.signerFor pkgs;
    in
    pkgs.runCommand "${package.name}-signed"
      {
        # Signs through this host's autopen socket, so it must never be
        # substituted: a substituted output was signed by somebody else's key.
        # The sandbox opens only the socket and the reference file.
        __darwinAllowLocalNetworking = true;
        __impureHostDeps = [
          autopen.runDir
          autopen.ref
        ];
        allowSubstitutes = false;
        preferLocalBuild = true;

        nativeBuildInputs = lib.optional (signer != null) signer.tool;

        # Consumers reach through these. `services.paneru.lua` defaults to
        # `cfg.package.luaModule.lua`, and four call sites in
        # ../wms/paneru/default.nix use `finalPackage.passthru.luaModule`, so
        # dropping passthru breaks evaluation rather than just the signature.
        passthru = (package.passthru or { }) // {
          unsigned = package;
          override =
            args:
            mkSignedWrapper pkgs {
              package = package.override args;
              inherit entitlements;
            };
          overrideAttrs =
            f:
            mkSignedWrapper pkgs {
              package = package.overrideAttrs f;
              inherit entitlements;
            };
        };
        # `lib.getExe` is used on these, so mainProgram has to survive.
        meta = package.meta or { };
      }
      ''
        # Each signed program becomes `libexec/<name>.app`, identified by
        # `in.ivymect.<name>`. TCC keys a bundle by its identifier and the
        # certificate, not by path, so the store path may move on every rebuild.
        mkdir -p "$out/bin" "$out/libexec"

        # Mach-O by magic number; `file` is not in every stdenv. Follows
        # symlinks, which matters: inside a symlinkJoin every bin/ entry is one.
        isMachO() {
        	case "$(od -An -N4 -tx1 -- "$1" 2>/dev/null | tr -d ' \n')" in
        	cffaedfe | cefaedfe | cafebabe | bebafeca) return 0 ;;
        	*) return 1 ;;
        	esac
        }

        shopt -s nullglob
        for entry in ${package}/*; do
        	name="$(basename "$entry")"
        	# `if`, not `[ ... ] && continue`: the builder runs under `set -e`.
        	if [ "$name" = bin ] || [ "$name" = libexec ]; then
        		continue
        	fi
        	ln -s "$entry" "$out/$name"
        done
        if [ -d ${package}/libexec ]; then
        	for entry in ${package}/libexec/*; do
        		ln -s "$entry" "$out/libexec/$(basename "$entry")"
        	done
        fi

        if [ ! -d ${package}/bin ]; then
        	echo "codesign: ${package} has no bin/, nothing to sign"
        	exit 0
        fi

        ${lib.optionalString (signer == null) ''
          echo "codesign: no autopen codesign key yet; run gen-secrets" >&2
          exit 1
        ''}

        for f in ${package}/bin/*; do
        	name="$(basename "$f")"

        	# makeWrapper's hidden payloads are reached through their wrapper.
        	case "$name" in
        	.*-wrapped) continue ;;
        	esac

        	if [ ! -f "$f" ] || [ ! -x "$f" ]; then
        		ln -s "$f" "$out/bin/$name"
        		continue
        	fi

        	# The Mach-O this entry runs: itself, or the `.<name>-wrapped` payload
        	# of a makeWrapper script (one layer only).
        	target="$f"
        	depth=0
        	while ! isMachO "$target"; do
        		hidden="$(dirname "$target")/.$(basename "$target")-wrapped"
        		if [ ! -e "$hidden" ]; then
        			target=""
        			break
        		fi
        		if [ "$depth" -ge 1 ]; then
        			echo "codesign: $name is wrapped more than one layer deep, which this does not handle" >&2
        			exit 1
        		fi
        		target="$hidden"
        		depth=$((depth + 1))
        	done

        	# A plain script: nothing to sign.
        	if [ -z "$target" ]; then
        		ln -s "$f" "$out/bin/$name"
        		continue
        	fi

        	id="in.ivymect.$name"
        	stage="$TMPDIR/$name.app"
        	mkdir -p "$stage/Contents/MacOS"
        	cp "$target" "$stage/Contents/MacOS/$name"
        	chmod 755 "$stage/Contents/MacOS/$name"
        	cat >"$stage/Contents/Info.plist" <<PLIST
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0"><dict>
          <key>CFBundleIdentifier</key><string>$id</string>
          <key>CFBundleExecutable</key><string>$name</string>
          <key>CFBundleName</key><string>$name</string>
          <key>CFBundlePackageType</key><string>APPL</string>
          <key>CFBundleVersion</key><string>1</string>
          <key>LSUIElement</key><true/>
        </dict></plist>
        PLIST

        	# No timestamp: the requirement TCC stores is the certificate hash.
        	autopen-codesign --remote-key ${if signer == null then "/nonexistent" else signer.remoteKey} \
        		--certificate ${if signer == null then "/nonexistent" else signer.certificate} \
        		--binary-identifier "$id" ${entitlementsArg} \
        		"$stage" "$out/libexec/$name.app"
        	exe="$out/libexec/$name.app/Contents/MacOS/$name"

        	if [ "$target" = "$f" ]; then
        		ln -s "$exe" "$out/bin/$name"
        	else
        		# Upstream's script is what exports LUA_PATH, PATH and the rest, so
        		# it is kept verbatim with only its exec target repointed.
        		cp "$f" "$out/bin/$name"
        		chmod +w "$out/bin/$name"
        		substituteInPlace "$out/bin/$name" --replace-fail "$target" "$exe"
        		chmod 755 "$out/bin/$name"
        	fi
        done
      '';

  # Signed in the overlay itself. A module may wrap these again (paneru's
  # `finalPackage`, home-manager's sketchybar): the exec chain still ends in the
  # signed bundle, which TCC identifies by bundle id, so no launchd job needs
  # repointing.
  signed = [
    "sketchybar"
    "paneru"
    "coolabah"
    "kanata-with-cmd"
    "kanata-vk-agent"
  ];

  present = pkgs: lib.filter (name: pkgs ? ${name}) signed;
in
{
  # `mkSignedWrapper` is the producer: hand it a package and (optionally)
  # entitlements, get back a signed wrapper. Exposed so anything outside this
  # aspect can build one without going through the overlay.
  flake.lib.codesign = {
    inherit
      mkSignedWrapper
      entitlementsFor
      subject
      ;
  };

  # The key, its daemon, and the sandbox setting. Kept apart from `codesign`
  # because a switch builds before it activates: deploy this first, then add
  # `codesign`, whose wrappers need the daemon running.
  den.aspects.codesign-identity = {
    includes = [ den.aspects.autopen ];

    # `relaxed` so builds stay sandboxed while the signing wrappers open only the
    # socket and the reference file through `__impureHostDeps`, which the
    # daemon allows only for paths listed here.
    darwin.nix.settings = {
      sandbox = "relaxed";
      extra-allowed-impure-host-deps = [
        autopen.runDir
        autopen.ref
      ];
    };
  };

  den.aspects.codesign = {
    includes = [ den.aspects.codesign-identity ];

    # Sorted last: `aspects/tooling/overlays.nix` orders overlays by attribute
    # name, and `paneru`/`kanata-vk-agent` only exist once their own overlays
    # have run.
    overlays.zzz-codesign = [
      (
        final: prev:
        lib.genAttrs (present prev) (
          name:
          mkSignedWrapper final {
            package = prev.${name};
            entitlements = entitlementsFor.${name} or { };
          }
        )
      )
    ];
  };
}
