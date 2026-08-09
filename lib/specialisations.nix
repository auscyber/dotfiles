# Build one specialisation of a host (or a standalone home) as a COMPOSITION OF
# ASPECTS.
#
# A specialisation here is not a pile of `mkForce` overrides layered on top of
# the running configuration -- it is the entity's own aspect tree, re-resolved
# with extra aspects included and unwanted ones subtracted, evaluated from
# scratch:
#
#   composed root = <entity's own aspect> ++ <every contribution>
#                   minus every aspect the contributions exclude
#
# Subtraction is den's `excludes` constraint (den fx/aspect/children.nix
# `registerConstraints`); addition is an ordinary include, so a contribution can
# carry includes, class content and nested provides like any other aspect.
#
# HOW IT IS BUILT. The obvious route -- `resolveEntity` + `resolveWithPaths`,
# which is how `<entity>.mainModule` is produced -- resolves an entity ROOTED AT
# ITSELF, and that path is measurably lower fidelity than the walk den really
# uses for a host: `provide`-delivered modules from `den.default` policies never
# reach a user's home-manager subtree, because that subtree comes from a spawned
# sub-pipeline (den nix/lib/home-env.nix `userForward`) and only the fleet
# materializer (`applyInstantiates` -> `mkInstantiateArgs` -> `materializeUnified`)
# threads provides into it. Concretely: `programs.nixvim` is written by the nvim
# forward but `inputs.nixvim.homeModules.nixvim` is never imported, so the
# specialisation dies on "option `programs.nixvim' does not exist" while the
# parent host evaluates fine.
#
# So a specialisation is built by running den's REAL walk over a one-entity
# fleet: a flake-system root whose only includes are `resolve.to <kind>` and
# `instantiate` for the composed entity -- exactly what den's own
# `system-to-os-outputs` policy emits per host (den modules/policies/flake.nix) --
# evaluated the way den evaluates its flake outputs (den modules/outputs.nix).
# The result is the same shape as `darwinConfigurations.<host>`, built by the
# same machinery, so nothing is quietly missing.
{
  lib,
  den,
  inputs,
}:
let
  inherit (den.lib.policy) resolve instantiate mkPolicy;

  # Where the private one-entity walk parks its output: the entity's own output
  # path with a qualified leaf. It has to be a real flake output attribute --
  # den's flake output type declares those and nothing else -- but this
  # evalModules is ours, not the repo's outputs, so nothing leaks.
  specIntoAttr = entity: name: lib.init entity.intoAttr ++ [ "${lib.last entity.intoAttr}@${name}" ];
in
rec {
  # kind          : den entity kind -- "host" or "home"
  # entity        : the den entity (needs .aspect, .class, .name, .system)
  # name          : specialisation name
  # contributions : list of aspect-shaped attrsets, i.e. the values every aspect
  #                 wrote under `specialisations.<name>`
  composedEntity =
    {
      kind,
      entity,
      name,
      contributions,
    }:
    let
      excludes = lib.concatMap (c: c.excludes or [ ]) contributions;

      # A contribution read out of the routed class content is content-wrapped:
      # it carries its provenance in `__provider` but no `name`, so its identity
      # collapses to "<anon>" (den fx/identity.nix `aspectPath`). Reconstruct the
      # name den's own `unwrapContent` would give it, so each contribution is a
      # distinct, named node in the composed tree.
      named = lib.imap0 (
        i: c:
        let
          prov = c.__provider or [ ];
        in
        if builtins.isAttrs c && prov != [ ] && !(c ? name) then
          c
          // {
            name = "${lib.last prov}@${name}";
            meta = (c.meta or { }) // {
              provider = lib.init prov;
            };
          }
        else if builtins.isAttrs c && !(c ? name) then
          c // { name = "contribution-${toString i}@${name}"; }
        else
          c
      ) contributions;

      # A host's home-manager content is resolved in a SPAWNED sub-pipeline
      # rooted at `resolveEntity "user" { host; user; }` (den nix/lib/home-env.nix
      # `userForward`). That sub-pipeline is threaded with the parent's scope
      # contexts and routes but NOT its constraint registry, so an exclude
      # registered on the host root never reaches a user-scoped aspect --
      # excluding `den.aspects.kanata` would silently leave kanata enabled in
      # `home-manager.users.<u>`. Re-root each user on a composed aspect carrying
      # the same excludes, which is where that sub-pipeline does look.
      # Only when there is something to subtract: re-rooting a user perturbs the
      # scope tree den walks, and there is no reason to pay for that in a profile
      # that only ADDS content.
      specUsers = lib.mapAttrs (
        userName: user:
        user
        // {
          aspect = {
            name = "${userName}@${name}";
            includes = [ user.aspect ];
            inherit excludes;
          };
        }
      ) (entity.users or { });
    in
    entity
    // lib.optionalAttrs (entity ? users) { users = specUsers; }
    // {
      # Recursion guard, the den-level analogue of upstream's
      # `specialisation = lib.mkOverride 0 { }`: the delivery aspects read this
      # off their `{ host, ... }` / `{ home, ... }` args and stay inert, so a
      # specialisation never builds a specialisation of itself.
      __isSpecialisation = true;
      __specialisationName = name;

      intoAttr = specIntoAttr entity name;

      aspect = {
        name = "${entity.name or kind}@${name}";
        # `den.default` needs no mention: den injects it as a schema include for
        # host/user/home (den modules/aspects/defaults.nix), and `resolveEntity`
        # seeds schema includes on the root it builds.
        includes = [ entity.aspect ] ++ named;
        inherit excludes;
      };
    };

  # The arguments den would hand the platform builder for this composition:
  # `{ modules }` for a host, `{ pkgs, modules }` for a home. Obtained by running
  # the real walk with `instantiate` swapped for the identity function, so the
  # module list can be consumed directly (NixOS's own `specialisation.<name>`
  # wants modules, not a built system) as well as built.
  #
  # Not `builtConfig.config._module.args.modules`: den calls `nixosSystem`
  # through a path that leaves that empty here, and an empty list is a SILENT
  # failure -- a specialisation with no aspects in it at all, which only shows up
  # as something like "neither nixpkgs.hostPlatform nor nixpkgs.system has been
  # set" much further along.
  instantiateArgsFor =
    args@{
      kind,
      entity,
      ...
    }:
    let
      specEntity = composedEntity args // {
        instantiate = lib.id;
      };

      # den's own root for the scope a host is reached from, so `system` is in
      # context exactly as it is in the real walk -- then its fleet-wide includes
      # (every host of the system, every output route) are replaced by this one
      # entity's pair of effects.
      root = (den.lib.resolveEntity "flake-system" { inherit (entity) system; }) // {
        # A policy, not two bare effects: `includes` takes aspects, and the
        # effect descriptors `resolve.to` / `instantiate` return are dispatched by
        # the policy machinery. This is the same pair den's own
        # `system-to-os-outputs` emits per host (den modules/policies/flake.nix).
        includes = [
          (mkPolicy "specialisation:${lib.concatStringsSep "." specEntity.intoAttr}" (_: [
            (resolve.to kind { ${kind} = specEntity; })
            (instantiate specEntity)
          ]))
        ];
      };

      flake =
        (lib.evalModules {
          modules = [
            (den.lib.aspects.resolve "flake" root)
            inputs.den.flakeOutputs.flake
          ];
          specialArgs.inputs = inputs;
        }).config.flake;

      captured = lib.getAttrFromPath specEntity.intoAttr flake;
    in
    if (captured.modules or [ ]) == [ ] then
      throw "specialisations: composing '${args.name}' for ${entity.name or kind} produced no modules — den's walk did not reach the entity"
    else
      captured;

  # The built specialisation, as den would have built it had this composition
  # been a declared entity: `.config`, `.system.build.toplevel` /
  # `.activationPackage`, everything.
  buildFor =
    args@{
      kind,
      entity,
      ...
    }:
    let
      instantiateArgs = instantiateArgsFor args;
      built = entity.instantiate instantiateArgs;
    in
    {
      configuration = built;
      # What gets linked into `$out/specialisation/<name>`: a system toplevel for
      # a host, an activation package for a home -- the same things nixpkgs' and
      # home-manager's own specialisation modules link.
      package = if kind == "home" then built.activationPackage else built.config.system.build.toplevel;
      inherit (instantiateArgs) modules;
    };
}
