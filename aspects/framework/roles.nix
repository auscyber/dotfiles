{
  den,
  lib,
  ...
}:
let
  inherit (den.lib.policy) include mkPolicy;

  roles =
    (builtins.attrValues den.hosts)
    |> lib.concatMap builtins.attrValues
    |> lib.concatMap (
      host: host.roles or [ ] ++ lib.concatMap (user: user.roles) (builtins.attrValues host.users)
    )
    |> lib.unique;

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
        ) roles;
      };
    };
}
