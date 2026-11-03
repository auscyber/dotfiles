{ den, lib, ... }:
# A host that claims a household service includes the service's aspects. They
# land in the host's own scope rather than a child `service` scope, so
# `hasAspect` and the capability layer see them as the host's. Reads only the
# declared claim (`serves`), never `hasAspect`, so it cannot loop.
let
  inherit (den.lib.policy) include;
in
{
  den.policies.service-propagate =
    { host, ... }:
    lib.concatMap (s: map include (lib.optional (s.aspect != null) s.aspect ++ s.includes)) (
      lib.attrValues host.serves
    );

  den.schema.host.includes = [ den.policies.service-propagate ];
}
