{
  den,
  lib,
  ...
}:
let
  inherit (den.lib.policy) include mkPolicy;

  # The full set of role names in use across every host/user. Was derived by
  # walking den.hosts (all systems -> all hosts -> all users) and collecting
  # `.roles`, unique'd — but that traversal reran on every eval for a set that
  # only changes when someone adds a genuinely new role name. Written out
  # literally instead; update this list when a host/user declares a role not
  # already here (`grep -rn 'roles = \[' aspects/hosts` to check).
  roles = [
    "gui"
    "dev"
    "gaming"
    "study"
  ];

  hasRole = role: entity: lib.elem role (entity.roles or [ ]);
in
{
  den.schema.host.options.roles = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
  };
  den.schema.user.options.roles = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
  };

  # Per-aspect role delivery. `den.schema.aspect` is imported into every aspect
  # submodule (den nix/lib/aspects/types.nix), so this adds an `${role}` option
  # to every aspect holding aspect-shaped content, and delivers it when the
  # consuming host+user both carry that role.
  #
  # Delivery is an include EFFECT emitted by a policy — NOT `policy.when
  # config.${role}`. Handed a raw attrset, `policy.when` takes the
  # conditional-aspect path (den policy-effects.nix `wrapAsConditional`), which
  # delivers only class-local content and never re-runs the aspect FX pipeline,
  # so nested `provides.to-hosts`/`provides.to-users` are silently dropped. An
  # `include` effect is dispatched through `processInclude` → `emitAspectPolicies`
  # (den fx/aspect/provide.nix), so the content's own provides re-emit as
  # cross-policies and fan out to hosts/users natively — the same path
  # `mkCrossPolicy` uses. The role gate lives inside the policy fn (where `host`
  # and `user` are both bound), not on the include, so the content stays
  # registered at the aspect's scope and still reaches its provides-emission phase.
  #
  # The policy name must be unique per aspect: policy identity is name-based
  # (den identity.key = provider ++ name); since this module is applied to every
  # aspect, a shared name would collide and dedup, dropping all but one aspect's
  # delivery. `config.name` scopes it per aspect.
  den.schema.aspect =
    {
      config,
      options,
      ...
    }:
    {
      options = lib.genAttrs roles (
        role:
        lib.mkOption {
          type = lib.types.raw;
          default = { };
        }
      );

      # `den.schema.aspect` is imported into every aspect submodule
      # (den nix/lib/aspects/types.nix), where `includes` is a real option —
      # but it is ALSO introspected standalone (den entry-type.nix reads its
      # options/refs), where `includes` is not declared. `lib.mkIf` still
      # *registers* an `includes` definition (which faults introspection with
      # "option `includes' does not exist"); `lib.optionalAttrs` instead omits
      # the key entirely when the option is absent, so the standalone schema
      # carries no stray definition.
      # Only roles this SPECIFIC aspect actually declares content for get a
      # delivery policy. `den.schema.aspect` applies to all ~159 aspects, but
      # only one (security/1password.nix, "gui") sets any of the 4 role keys —
      # the other ~158 would otherwise get 4 policies apiece created and
      # dispatched at every scope they resolve in, all doing nothing but
      # `include {}`. `config.${role} != { }` forces only that one aspect's own
      # role value (cheap: a single `raw`-typed option, no recursion) rather
      # than the mkPolicy closure + its later dispatch.
      config = lib.optionalAttrs (options ? includes) {
        includes = map (
          role:
          mkPolicy "${config.name or "anon"}-role-${role}" (
            {
              host ? { },
              user ? { },
              ...
            }:
            lib.optional (hasRole role host && hasRole role user) (include config.${role})
          )
        ) (lib.filter (role: config.${role} != { }) roles);
      };
    };
}
