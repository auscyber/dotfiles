{
  lib,
  stdenvNoCC,
  unzip,
  jq,
}:
# The one place that knows how a Zotero addon is assembled.
#
# Zotero identifies a plugin by `manifest.applications.zotero.id`
# (XPIInstall.sys.mjs) and, when scanning <profile>/extensions, derives the id
# from the *filename* (XPIProvider.sys.mjs: id = leafName minus ".xpi"). So the
# xpi must be installed verbatim as `<id>.xpi` — repacking it (as nixpkgs'
# fetchFirefoxAddon does) rewrites `applications` to the gecko form and destroys
# the id Zotero reads. Signing is irrelevant: Zotero sets
# xpinstall.signatures.required = false.
#
# A filename that disagrees with the manifest loads nothing and reports no
# error, so the id is asserted against the manifest inside the finished xpi.
{
  addonId,
  # Shell expression resolving to the finished xpi, evaluated in installPhase.
  # The default is a release xpi handed straight over as `src`; an addon built
  # from source points this at whatever its buildPhase produced.
  xpi ? ''"$src"'',
  # Spliced in once `$xpi` and `$manifestId` are resolved, for per-addon
  # assertions that need the finished xpi.
  extraChecks ? "",
  nativeBuildInputs ? [ ],
  passthru ? { },
  meta ? { },
  ...
}@args:
stdenvNoCC.mkDerivation (
  removeAttrs args [
    "addonId"
    "xpi"
    "extraChecks"
  ]
  // {
    nativeBuildInputs = nativeBuildInputs ++ [
      unzip
      jq
    ];

    installPhase = ''
      runHook preInstall

      xpi=${xpi}
      if [ ! -f "$xpi" ]; then
        echo "buildZoteroAddon: no xpi at '$xpi'" >&2
        exit 1
      fi

      manifestId=$(unzip -p "$xpi" manifest.json | jq -er '.applications.zotero.id')
      if [ "$manifestId" != ${lib.escapeShellArg addonId} ]; then
        echo "buildZoteroAddon: manifest declares id '$manifestId', but addonId is '${addonId}'." >&2
        echo "The file must be named <id>.xpi or Zotero will not load it." >&2
        exit 1
      fi

      ${extraChecks}

      install -Dm444 "$xpi" "$out/share/zotero/extensions/$manifestId.xpi"

      runHook postInstall
    '';

    passthru = passthru // {
      inherit addonId;
    };

    meta = {
      platforms = lib.platforms.all;
    }
    // meta;
  }
)
