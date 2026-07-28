{ den, lib, ... }:
# `anyUser` — the shared identity of a homeManager environment, whether it is a
# host-managed `user` or a standalone `home`. It is a first-class CONTEXT arg (a
# registered schema kind, like `host`/`user`/`home`): every `user` and `home` binds its
# own `anyUser` projection, so an aspect `{ anyUser, ... }: …` receives it from
# the resolution context. The identity's `hostPublicKey` field is declared by the shared
# `pubKey.nix` schema, which `host`, `user` and `home` all derive from.
#
# It is a PROMOTED FIELD, not a resolved sub-entity: den's parametric dispatch only
# binds `{ anyUser, ... }` in class content when `anyUser` is IN the consuming scope's
# context (den `resolve-entity.nix` `entityDerivedBindings` promotes an entity's own
# schema-kind fields into its ctx). A `policy.resolve[.shared][.to]` instead resolves
# anyUser into a SEPARATE scope, which parent-scope class content can't bind — so those
# consumers (agenix-rekey `homeManager = { anyUser, ... }: …`) go silently inert. anyUser
# also has two possible parents (home OR user), which a single `den.schema.anyUser.parent`
# schema-DAG relation can't express, so the field is the correct mechanism here.
let
  fromHome = home: {
    name = home.userName;
    userName = home.userName;
    hostPublicKey = home.hostPublicKey or null;
    host = {
      name = home.hostName;
      system = home.system;
      class = null; # a standalone home has no OS class
    };
  };
  fromUser = user: {
    inherit (user) name userName;
    hostPublicKey = user.hostPublicKey or null;
    host = user.host; # a full host record: name/class/system
  };
  mkAnyUser =
    projection:
    lib.mkOption {
      type = lib.types.raw;
      default = projection;
    };
in
{
  # Register `anyUser` as an entity schema kind. As an entity kind it is promoted into
  # the resolution context from each entity's own `anyUser` field during the pipeline
  # walk — the same way `host`/`user`/`home` are — so `{ anyUser, ... }` aspects bind it.
  den.schema.anyUser.isEntity = true;

  # Each user and home carries its own `anyUser` projection as a schema-kind field.
  den.schema.home = { config, ... }: { options.anyUser = mkAnyUser (fromHome config); };
  den.schema.user = { config, ... }: { options.anyUser = mkAnyUser (fromUser config); };
}
