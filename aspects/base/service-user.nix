{
  den,
  lib,
  config,
  ...
}:
# A SERVICE'S UNIX USER, AND THE GROUPS IT JOINS.
#
# `den.services.<key>.user` names the account a service's processes run as, and
# `.groups` the extra groups that account should be in. This file is the half
# that acts: for every instance placed on this host, it adds `user` to `groups`.
#
# It does NOT create the account. The service's own nixpkgs module already does
# -- `services.nextcloud` makes `nextcloud` -- and a second opinion about uid
# ownership is how two modules end up fighting over one name. So this only ever
# grants membership, and a service whose module does not create its user will
# fail loudly at `users.users.<name>` ("neither isNormalUser nor isSystemUser")
# rather than quietly inventing one.
#
# Shaped exactly like ./service-implement.nix, for the reason that file records
# at length: ONE aspect that iterates the registry, not a scope per service.
# `{ host, ... }` binds, so the body reads `config.den.services` as DATA and
# folds the instances placed here. A per-service scope would re-resolve the same
# aspect once per instance and collide on anything it declares.
let
  # Instances that run HERE and have both halves. A `user` with no `groups` is
  # the common case and contributes nothing; `groups` with no `user` is a
  # declaration error, and it is caught rather than ignored -- silently dropping
  # it would mean a service that believes it has group access and does not.
  placed = lib.filterAttrs (_: s: s.host != null && s.groups != [ ]) config.den.services;
in
{
  den.aspects.service-user.nixos =
    { host, ... }:
    let
      here = lib.filterAttrs (_: s: s.host == (host.name or null)) placed;

      bad = lib.filterAttrs (_: s: s.user == null) here;
    in
    {
      assertions = [
        {
          assertion = bad == { };
          message = "den.services: ${lib.concatStringsSep ", " (lib.attrNames bad)} declare `groups` but no `user`, so there is no account to put in them.";
        }
      ];

      # Merges with whatever the service's own module declares for this user:
      # `extraGroups` is list-valued, so this adds rather than replaces.
      users.users = lib.mkMerge (
        lib.mapAttrsToList (_: s: {
          ${s.user}.extraGroups = s.groups;
        }) (lib.filterAttrs (_: s: s.user != null) here)
      );
    };

  den.schema.host.includes = [ den.aspects.service-user ];
}
