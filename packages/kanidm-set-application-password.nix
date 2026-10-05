_:
# kanidm, with a provisioning path for application passwords.
#
# ../patches/kanidm/set-application-password.patch adds a SECOND way to set an
# application password: one where the caller supplies the value. Upstream only
# mints them, returning the cleartext once, which is right for a human pasting a
# password into a mail client and wrong for a fleet that generates secrets
# offline into a store and provisions them when the server is next up.
#
# ── ONE ATTRIBUTE. See ./kanidm-trace-propagation.nix ──
# `kanidmWithSecretProvisioning_1_11` resolves through the OVERLAID
# `kanidm_1_11`, so patching that one is enough -- and
# `kanidm_1_11.withSecretProvisioning` is the same drvPath. Patching both
# applies every patch twice and the build fails on "Reversed (or previously
# applied) patch detected", which is how this was learned.
#
# `overrideAttrs` and not `.override { patches = ... }`: `patches` is an argument
# of the OUTER function in nixpkgs' `servers/kanidm/generic.nix`, applied at the
# `import ./generic.nix { version; hash; cargoHash; }` call site, so it is not in
# the argument set callPackage sees and `.override` cannot reach it.
#
# This is a FROM-SOURCE build of kanidm with no binary cache, and nixpkgs' own
# comment on the derivation says it "can take over 4 hours on 2 cores and needs
# 16GB+ RAM" (`requiredSystemFeatures = [ "big-parallel" ]`). Build it somewhere
# that can, and push it to a cache.
{
  den.aspects.packages.kanidm-set-application-password = {
    overlays = _: {
      kanidm-set-application-password = _self: prev: {
        kanidm_1_11 = prev.kanidm_1_11.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [ ../patches/kanidm/set-application-password.patch ];
        });
      };
    };
  };
}
