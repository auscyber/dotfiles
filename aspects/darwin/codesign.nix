# Signs sketchybar/paneru/kanata with a local certificate and execs them from a
# fixed path, so their TCC grants survive a rebuild.
#
# Two separate things invalidate a grant, and both have to go:
#
#   * TCC keys a non-bundled executable by absolute path (`client_type = 1`),
#     and every rebuild moves the store path. Hence `trustedDir`: each signed
#     binary moves to `trusted/`, `bin/<name>` becomes a makeWrapper shim
#     exec'ing `${trustedDir}/<name>`, and activation plants the copy there.
#     The exec resolves before the process exists, so TCC sees the stable path
#     -- and nothing referring to `${pkgs.sketchybar}/bin/sketchybar` has to be
#     repointed, which is the point of doing it here and not per launchd job.
#
#   * the recorded `csreq`. An ad-hoc signature (what nixpkgs applies) has no
#     certificate, so the requirement degrades to a `cdhash` and dies on any
#     content change. A real certificate pins it to the certificate hash
#     instead -- `codesign -d -r-` on a signed output reports
#     `identifier "hello" and certificate root = H"7b3f55..."`, which is a hash
#     comparison and so survives any rebuild of the same program. That is why
#     the certificate must never be regenerated: a new one is a new hash, and
#     every grant made against the old one dies with it.
#
# Where the repointing happens: in `bin/`, never in the launchd plist.
#
# Pointing each job's `Program`/`ProgramArguments` straight at
# `${trustedDir}/<name>` looks like the obvious simplification, and it is
# correct for a job whose environment comes from the plist -- kanata,
# kanata_tray and kanata-vk-agent all qualify (../input/kanata/_kanata.nix sets
# PATH in `EnvironmentVariables` and wraps nothing). It is wrong for a job whose
# environment comes from a wrapper script, which is the far more common shape:
# paneru's `finalPackage` is `wrapPaneru (...)`, a symlinkJoin whose `bin/paneru`
# is a wrapProgram script exporting LUA_PATH, and home-manager wraps sketchybar
# the same way to put `extraPackages` on PATH. Exec the signed binary directly
# and those exports never run.
#
# So the plists are left alone and `bin/` carries the repointing, in whichever
# of the two shapes the package turns out to have:
#
#   bin/<name> is a Mach-O          it is signed and planted as
#                                   ${trustedDir}/<name>.
#   bin/<name> is a makeWrapper     the `.<name>-wrapped` payload it execs is
#   script                          signed and planted as
#                                   ${trustedDir}/.<name>-wrapped, and upstream's
#                                   script is planted beside it as
#                                   ${trustedDir}/<name>, kept VERBATIM with only
#                                   its exec target repointed at that payload.
#
# Both halves of a wrapped chain are planted, not just the binary: a script that
# still exec'd a store path would put a rebuild-varying path back in the middle
# of the chain. Either way `bin/<name>` in the package is a shim onto
# ${trustedDir}/<name>, so the entry point is one path regardless of shape.
#
# The second case is the reason this generalises. `bin/<name>` plus a hidden
# `.<name>-wrapped` sibling is a nixpkgs-wide convention, not a paneru quirk, so
# handling it structurally means upstream modules keep working untouched: no
# option is repointed, no `mkForce` is needed, and nothing here has to know what
# any particular wrapper put in the environment. A module that starts wrapping
# something it did not wrap before is absorbed without a change.
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

  # What TCC records as the client for every shimmed program, so it is as
  # permanent as the grants. Shares `/run/wrappers/bin` with
  # `extraModules/darwin/wrappers`'s `security.wrappers` -- the same
  # NixOS-`security.wrapperDir`-flavoured stable bin directory, one fixed path
  # regardless of what happens to land in it.
  #
  # `/run` is a synthetic firmlink macOS clears on boot, and nix-darwin
  # activation only reruns at `darwin-rebuild` time, not at boot
  # (`org.nixos.activate-system`'s `RunAtLoad` job only relinks
  # `/run/current-system` and `/etc`, it does not rerun `activate`) -- so a
  # naive planter leaves every shim missing until the next switch after a
  # reboot. `den.aspects.wrapperd` (`aspects/darwin/wrapperd.nix`) is what
  # closes that gap: it replants at boot from a LaunchDaemon and gates the
  # dependent agents on it. This aspect only produces the signed binaries; it
  # no longer plants them.
  trustedDir = "/run/wrappers/bin";

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
  # signature. What it produces is the same shape the activation script has
  # always planted from:
  #
  #   trusted/<name>  the signed binary, which activation copies to trustedDir
  #   bin/<name>         a makeWrapper shim exec'ing ${trustedDir}/<name>
  #   everything else    symlinked straight through from the original
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

        nativeBuildInputs = [ pkgs.makeWrapper ] ++ lib.optional (signer != null) signer.tool;

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
        # `trusted/` is planted verbatim into ${trustedDir} by activation, and
        # `bin/<name>` is always a shim onto ${trustedDir}/<name>. Everything
        # that has to sit at a stable path lives in `trusted/` -- for a wrapped
        # program that is BOTH the script and the payload it execs, because a
        # script that execs a store path puts a rebuild-varying path back in the
        # middle of the chain, which is the thing this whole aspect exists to
        # avoid.
        mkdir -p "$out/bin" "$out/trusted"

        # Mach-O by magic number; `file` is not in every stdenv. Follows
        # symlinks, which matters: inside a symlinkJoin every bin/ entry is one.
        isMachO() {
        	case "$(od -An -N4 -tx1 -- "$1" 2>/dev/null | tr -d ' \n')" in
        	cffaedfe | cefaedfe | cafebabe | bebafeca) return 0 ;;
        	*) return 1 ;;
        	esac
        }

        # A wrapper adds a signature; it is not a rebuild. Everything that is
        # not bin/ is the original, so it is symlinked rather than copied.
        shopt -s nullglob
        for entry in ${package}/*; do
        	name="$(basename "$entry")"
        	# `if`, not `[ ... ] && continue`: the builder runs under `set -e`, so
        	# a false test at the end of the body would abort the whole build.
        	if [ "$name" = bin ]; then
        		continue
        	fi
        	ln -s "$entry" "$out/$name"
        done

        # Inherited by siblings with no bin/ at all -- paneru's loadable Lua
        # module, for one. Producing no trusted/ is the correct outcome
        # there: activation plants what it finds, so a package with nothing to
        # sign contributes nothing.
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

        	# makeWrapper's hidden payloads are reached through their wrapper
        	# below, never signed under their own name.
        	case "$name" in
        	.*-wrapped) continue ;;
        	esac

        	if [ ! -f "$f" ] || [ ! -x "$f" ]; then
        		ln -s "$f" "$out/bin/$name"
        		continue
        	fi

        	# Find the Mach-O this entry ultimately runs. Either it IS one, or it
        	# is a makeWrapper script and the real binary is the `.<name>-wrapped`
        	# sibling it execs, possibly through several layers of wrapping.
        	target="$f"
        	depth=0
        	while ! isMachO "$target"; do
        		hidden="$(dirname "$target")/.$(basename "$target")-wrapped"
        		if [ ! -e "$hidden" ]; then
        			target=""
        			break
        		fi
        		# Only one layer is handled. The rewrite below substitutes the
        		# path the SCRIPT references, and past one layer that is an
        		# intermediate script rather than the Mach-O -- each layer would
        		# have to be planted and repointed in turn. Nothing here is wrapped
        		# twice; if something starts being, this says so instead of
        		# substituting the wrong path.
        		if [ "$depth" -ge 1 ]; then
        			echo "codesign: $name is wrapped more than one layer deep, which this does not handle" >&2
        			exit 1
        		fi
        		target="$hidden"
        		depth=$((depth + 1))
        	done

        	# Not a Mach-O and not a wrapper around one: a plain script, of which
        	# sketchybar ships several. Carried through untouched -- there is
        	# nothing to sign and nothing to plant.
        	if [ -z "$target" ]; then
        		ln -s "$f" "$out/bin/$name"
        		continue
        	fi

        	# Where the signed payload has to land. A bare binary IS the program,
        	# so it takes the program's own name. A wrapped one keeps
        	# makeWrapper's hidden name, because the script that execs it is
        	# planted alongside under the program name and has to find it there.
        	if [ "$target" = "$f" ]; then
        		payload="$name"
        	else
        		payload=".$name-wrapped"
        	fi

        	# `$target` is a Mach-O by construction -- the loop above only exits
        	# with it set once `isMachO` passed -- so this signs the real binary,
        	# never a wrapper script. A script cannot carry a code signature TCC
        	# would honour anyway.
        	#
        	# The identifier is the stable half of the requirement, so it is the
        	# name rather than anything version-derived.
        	# No timestamp: the requirement TCC stores is the certificate hash.
        	autopen-codesign --remote-key ${if signer == null then "/nonexistent" else signer.remoteKey} \
        		--certificate ${if signer == null then "/nonexistent" else signer.certificate} \
        		--binary-identifier "$name" ${entitlementsArg} \
        		"$target" "$out/trusted/$payload"
        	chmod 755 "$out/trusted/$payload"

        	if [ "$target" != "$f" ]; then
        		# Wrap the wrapper. The script is upstream's and it is what exports
        		# LUA_PATH, PATH and the rest, so it is kept verbatim and ONLY its
        		# exec target is repointed -- from the store copy of the payload to
        		# the planted one. Both halves of the chain then live at fixed
        		# paths, so nothing a rebuild moves is left in it.
        		cp "$f" "$out/trusted/$name"
        		chmod +w "$out/trusted/$name"
        		substituteInPlace "$out/trusted/$name" --replace-fail "$target" "${trustedDir}/$payload"
        		chmod 755 "$out/trusted/$name"
        	fi

        	# Uniform regardless of shape: what the package exposes is a shim onto
        	# the stable path. makeWrapper cannot target ${trustedDir} directly --
        	# assertExecutable rejects a path that only exists after activation --
        	# so it is aimed at the planted file and then re-pointed.
        	makeWrapper "$out/trusted/$name" "$out/bin/$name"
        	substituteInPlace "$out/bin/$name" --replace-fail "$out/trusted/$name" "${trustedDir}/$name"
        done
      '';

  # In this host's TCC database keyed by a store path, which is the whole
  # reason for the list. NOT paneru or sketchybar: both home-manager modules
  # wrap `cfg.package` *again* after this overlay runs -- paneru's own
  # `services.paneru.finalPackage` (`nix/_paneru-common.nix`) and
  # home-manager's `programs.sketchybar.finalPackage`
  # (`modules/programs/sketchybar.nix`) -- via a plain `symlinkJoin` +
  # `wrapProgram` that has never heard of `mkSignedWrapper`, to inject
  # `LUA_PATH`/`LUA_CPATH`/`PATH` (sketchybar's own `extraPackages`/
  # `extraLuaPackages`, e.g. jq/yq/nowplaying-cli and the colours/icon-map Lua
  # modules -- aspects/desktop/sketchybar/sketchybar.nix). Feeding that a
  # pre-signed `pkgs.<name>` produces a *second*, unsigned
  # wrapper-around-a-wrapper whose store path drifts on every rebuild that
  # touches any of those dependencies, which is often -- exactly the
  # instability this whole aspect exists to remove, just one layer further
  # out. Leaving them out of `signed` keeps `pkgs.<name>` (hence `cfg.package`)
  # plain, so `finalPackage` ends up wrapped exactly *once* over the real
  # Mach-O -- the shape `mkSignedWrapper` already handles below. Each is
  # signed explicitly at that point instead: see the `fromUser` entries in
  # `den.aspects.codesign.darwin` and the matching calls in
  # `aspects/wms/paneru/default.nix` and
  # `aspects/desktop/sketchybar/sketchybar.nix`, which sign `finalPackage`
  # itself (same inputs, so the same derivation either way).
  signed = [
    # `kanata-with-cmd`, not `kanata`: `aspects/input/kanata/_kanata.nix`
    # defaults `programs.kanata.package` to it, because the keybinds in the
    # paneru aspect are all `(t! runasuser ...)` and `cmd` is a build feature.
    # Signing plain `kanata` would sign a package nothing runs. Not
    # double-wrapped like paneru/sketchybar above: its plist sets `PATH` via
    # `EnvironmentVariables` directly and wraps nothing
    # (aspects/input/kanata/_kanata.nix).
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
      trustedDir
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

    # Planting -- copying each signed `trusted/<name>` into `trustedDir` -- used
    # to live here in `postActivation`. It moved to `den.aspects.wrapperd`
    # (`aspects/darwin/wrapperd.nix`), which owns the whole
    # `trustedDir`/reboot-gap story: it generates a manifest from the same
    # `enable`-gated `finalPackage` collection this aspect used, plants it on
    # activation AND at boot (via a LaunchDaemon -- activation does not rerun at
    # boot), and gates the launchd agents on that planter over a Mach service.
    # This aspect is now purely the signer: `mkSignedWrapper`, the entitlements
    # map, and the `zzz-codesign` overlay above.
  };
}
