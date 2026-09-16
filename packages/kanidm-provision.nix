{
  # Service accounts and their API tokens, which upstream's schema has no entity
  # for -- so a machine caller cannot be given a kanidm identity without this.
  # The NixOS module needs no patch to go with it: `provision.extraJsonFile` is
  # merged into the generated state with yq, which is how the new entities get in.
  #
  # Sent upstream? Not yet. kanidm/kanidm#2627 is the tracking epic for the
  # wider kanidm<->service integration story this belongs to.
  den.aspects.packages.kanidm-provision = {
    overlays = { ... }: {
      kanidm-provision = _self: super: {
        kanidm-provision = super.kanidm-provision.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [ ../patches/kanidm-provision/service-accounts.patch ];
        });
      };
    };
  };
}
