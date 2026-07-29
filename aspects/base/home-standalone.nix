{
  lib,
  den,
  ...
}:
# Scope-identity fix for `user@host` homes. den forces `home.name = userName`, so two
# standalone homes for the same user (ivy@imflopet, ivy@contabo) share a scope id
# (mkScopeId keys on `.name`) and collide at the flake→home fan-out, resolving as one.
# Give a `user@host` home a name unique per host — but that breaks `lookupAspect`
# (keyed on config.name = den.aspects.<name>), so also pin the aspect to the bare-user
# aspect (what lookupAspect matched before the name carried the host).
#
# Identity for these homes flows through `anyUser` (aspects/base/anyuser.nix), not a
# synthetic user binding.
{
  den.schema.home =
    { config, ... }:
    let
      userName = config.userName;
      hostName = config.hostName;
      nameWithHost = hostName != null;
    in
    {
      config = lib.mkIf nameWithHost {
        name = lib.mkOverride 40 "${userName}@${hostName}";
        aspect = lib.mkOverride 40 (if den.aspects ? ${userName} then den.aspects.${userName} else { });
      };
    };
}
