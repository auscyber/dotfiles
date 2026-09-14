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
    overlays.kanidm-trace-propagation = final: prev: {
      kanidm_1_11 = prev.kanidm_1_11.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [ ../patches/kanidm/trace-propagation.patch ];
      });
    };
  };
}
