# Aspect-presence guards, injected into `den.lib` (a freeform
# `lazyAttrsOf unspecified` submodule — den nix/nixModule/lib.nix — so it takes
# our own additions the same way den's own modules/context/perHost-perUser.nix
# adds `perHost`/`perUser`/`perHome`).
#
# Inject content only when some *other* aspect resolved onto the same entity.
# Both the probed aspect and the injected content are parameters:
#
#   den.aspects.onepassword.includes = [
#     (den.lib.whenAspect den.aspects.browsers.zen {
#       homeManager = { pkgs, ... }: {
#         programs.zen-browser._internalProfile.extensions.packages =
#           [ pkgs.firefox-addons.onepassword-password-manager ];
#       };
#     })
#   ];
#
# Built on `den.lib.policy.when` with a NON-policy payload, which takes den's
# conditional-aspect path (`wrapAsConditional`, den nix/lib/policy-effects.nix).
# That is why the predicate reads `hasAspect` off its own ctx rather than off
# `host`/`user`: the compile-conditional handler (den
# nix/lib/aspects/fx/handlers/compile-conditional.nix, `mkGuardCtx`) evaluates
# the guard against the *in-flight* pathSet, so it never touches
# `config.resolved` and never closes the `includes` → `resolved` → `includes`
# cycle that a bare `host.hasAspect` call from a policy body would. It is also
# exclude-aware: an aspect removed by a `meta.handleWith` constraint reads as
# absent. Membership is scoped to the consuming entity's own subtree plus
# ancestors, not the fleet-wide walk, so a sibling host that included the aspect
# earlier in the walk does not leak in (den #613).
#
# CAVEAT — the payload should be plain class content. `wrapAsConditional`
# delivers class-local content and does not re-run the aspect FX pipeline, so a
# nested `provides.to-hosts` / `provides.to-users` inside the payload is dropped
# rather than fanned out. That is the same trap documented at length in
# core/roles.nix; for provides-bearing content, gate at the class body with the
# entity's own `hasAspect` instead (see `whenAspect` vs `hasAspect` note below).
{ den, lib, ... }:
let
  toList = x: if builtins.isList x then x else [ x ];

  # `policy.when` dispatches on payload shape: policies and bare functions take
  # `wrapAsPolicy`, where the predicate is handed the ordinary policy ctx, which
  # carries no `hasAspect`. Our predicates destructure `{ hasAspect, ... }`, so
  # `when`'s required-arg precheck would find the key missing and the guard
  # would silently never fire. Coerce a functor payload into an attrset aspect
  # (functors are legal `includes` entries) so we always land on the conditional
  # path, and reject policies loudly rather than no-op.
  asAspect =
    content:
    if content.__isPolicy or false then
      throw "den.lib.whenAspect: payload must be aspect-shaped content, not a policy — policies are dispatched before guards run and would never see `hasAspect`"
    else if builtins.isFunction content then
      { includes = [ content ]; }
    else
      content;

  guard =
    pred: targets: content:
    den.lib.policy.when ({ hasAspect, ... }: pred hasAspect (toList targets)) (asAspect content);
in
{
  # Present: every listed aspect resolved onto this entity.
  den.lib.whenAspect = guard (hasAspect: lib.all hasAspect);

  # Present: at least one of the listed aspects resolved onto this entity.
  den.lib.whenAnyAspect = guard (hasAspect: lib.any hasAspect);

  # Absent: none of the listed aspects resolved onto this entity.
  den.lib.unlessAspect = guard (hasAspect: targets: !(lib.any hasAspect targets));
}
