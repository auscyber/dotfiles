{
  den,
  lib,
  ...
}:
let
  roles =
    (builtins.attrValues den.hosts)
    |> lib.concatMap builtins.attrValues
    |> lib.concatMap (
      host: host.roles or [ ] ++ lib.concatMap (user: user.roles) (builtins.attrValues host.users)
    )
    |> lib.unique;

  # For each role we add a single option `${role}` whose value is an
  # inline aspect (any aspect-shaped attrset — classes, includes, provides,
  # quirks, etc.). We then conditionally include that aspect via
  # `policy.when`, gated on the host+user role intersection.
  #
  # Producers write:
  #
  #   den.aspects.zen.gui = {
  #     includes = [ den.aspects.stylix ];
  #     homeManager = { ... };
  #     darwin      = { ... };
  #     provides.to-users = { ... };
  #   };
  #
  # No per-class declaration is needed here — whatever sub-keys make
  # sense in an aspect work inside `${role}` too, because the content is
  # included as an aspect when the role activates.
  roleAspectModule =
    role:
    { config, lib, ... }:
    {
      options.${role} = lib.mkOption {
        type = lib.types.raw;
        default = { };
        description = ''
          Aspect-shaped content delivered when the '${role}' role is
          active for the consuming host+user. Anything you can put on
          an aspect (classes, includes, provides, …) works here.
        '';
      };

      config.includes = [
        (den.lib.policy.when (
          {
            host ? { },
            user ? { },
            ...
          }:
          lib.elem role (host.roles or [ ]) && lib.elem role (user.roles or [ ])
        ) config.${role})
      ];
    };
in
{
  den.schema.host.options.roles = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      Capabilities this host advertises (e.g. [ "gui" "dev" ]).
      Combined with the matching user's roles, this controls which
      role-tagged aspect content is delivered.
    '';
  };
  den.schema.user.options.roles = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = ''
      Capabilities this user requests. The intersection with the
      host's roles activates per-aspect `<role>` content.
    '';
  };

  den.default.includes = map roleAspectModule roles;
}
