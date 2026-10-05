{
  # Service accounts and their API tokens, which upstream's schema has no entity
  # for -- so a machine caller cannot be given a kanidm identity without this.
  # Plus a person's SSH public keys, so a host with no local account for
  # somebody can still authorise them -- see ssh-keys.patch.
  # Plus applications and application passwords, which pair with
  # ../patches/kanidm/set-application-password.patch: kanidm only MINTS those,
  # returning the cleartext once, so a declared value needs both halves.
  # The NixOS module needs no patch to go with it: `provision.extraJsonFile` is
  # merged into the generated state with yq, which is how the new entities get in.
  #
  # Sent upstream? Not yet. kanidm/kanidm#2627 is the tracking epic for the
  # wider kanidm<->service integration story this belongs to.
  den.aspects.packages.kanidm-provision = {
    overlays = { ... }: {
      kanidm-provision = _self: super: {
        kanidm-provision = super.kanidm-provision.overrideAttrs (old: {
          # ORDER MATTERS. Both touch the same three files at the same anchors,
          # and application-passwords.patch is generated against the tree with
          # service-accounts.patch already applied -- the other way round, five
          # of its hunks fail.
          patches = (old.patches or [ ]) ++ [
            ../patches/kanidm-provision/service-accounts.patch
            ../patches/kanidm-provision/application-passwords.patch
            # A person's SSH public keys, which upstream's `persons.<n>` cannot
            # carry -- so kanidm could SERVE authorized_keys
            # (`kanidm_ssh_authorizedkeys`) but never be told what they are.
            # Generated against the tree with both patches above applied, for
            # the same reason their order matters.
            ../patches/kanidm-provision/ssh-keys.patch
          ];
        });
      };
    };
  };
}
