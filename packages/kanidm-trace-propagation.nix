{
  # patches/kanidm/trace-propagation.patch -- kanidmd doesn't extract an
  # incoming W3C traceparent (checked directly against its source: the span
  # tower-http builds for every request never looks at the caller's headers),
  # so a request nginx proxies to kanidm always shows as two disconnected
  # traces instead of one. Local patch rather than waiting on upstream: it
  # adds no new Cargo dependency (a hand-rolled Extractor, same shape as the
  # module's existing OpenTelemetrySpanExt usage), so the existing cargoHash
  # stays valid -- only the source changes, not Cargo.lock.
  den.aspects.packages.kanidm-trace-propagation = {
    # The outer `_:` is not decoration. aspects/tooling/overlays.nix walks
    # `den.aspects.packages.*` and treats a FUNCTION under `overlays` as a
    # configurator -- it calls it with `{ sources, system, pkgs }` and expects a
    # *named attrset* of overlays back. Handing it a bare `final: prev: ...`
    # meant `final` was bound to the deps set and the return value was still a
    # function, so the fold did `acc // <lambda>` and every app in the flake died
    # with "expected a set but found a function" (`.#update` included). This
    # takes the deps, ignores them, and returns the named overlay the walker
    # wants -- the same shape as packages/eagle-nvim.nix and packages/ghostty.nix.
    # ── ONE ATTRIBUTE IS ENOUGH, AND TWO IS A BUILD FAILURE ──
    # `kanidmWithSecretProvisioning_1_11` is `kanidm_1_11.override
    # { enableSecretProvisioning = true; }` and resolves through the OVERLAID
    # `kanidm_1_11`, so it inherits whatever is patched here. Measured:
    #
    #   patch kanidm_1_11 only
    #     -> kanidmWithSecretProvisioning_1_11.patches =
    #          [oauth2-basic-secret-modify, recover-account, trace-propagation]
    #     -> and `kanidm_1_11.withSecretProvisioning` is the SAME drvPath
    #
    # Patching both -- which looked necessary, since the passthru is bound to a
    # callPackage argument -- applies every patch TWICE and the build dies on
    # "Reversed (or previously applied) patch detected". That is how this was
    # found, so the comment is here rather than in a commit message.
    overlays = _: {
      kanidm-trace-propagation = _final: prev: {
        kanidm_1_11 = prev.kanidm_1_11.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [ ../patches/kanidm/trace-propagation.patch ];
        });
      };
    };
  };
}
