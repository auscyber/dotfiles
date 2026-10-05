{
  den,
  lib,
  checkLib,
  ...
}:
let
  extraOptions = {
    options = {
      hostPublicKey = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.unique { message = "a host public key must have a single value"; } (
            lib.types.either lib.types.str lib.types.path
          )
        );
        default = null;
        apply =
          v:
          if v == null then
            builtins.warn "hostPublicKey is required: set it on the host/user (e.g. `den.hosts.<sys>.<name>.hostPublicKey = \"ssh-ed25519 …\";`)." null
          else if builtins.isPath v then
            builtins.readFile v
          else
            v;
        description = "Public key (literal string or path to file) of the entity for which to generate rekey secrets.";
      };
    };
  };
in
{
  den.schema.host = extraOptions;
  den.schema.user = extraOptions;
  # A standalone home carries its own hostPublicKey too (agenix rekey via `anyUser`).
  den.schema.home = extraOptions;

  # ── A HOST THAT HOLDS SECRETS MUST HAVE A KEY, LOUDLY ──
  #
  # The option's `apply` only WARNS when this is unset, and agenix-rekey then
  # rekeys that host's secrets to a dummy key -- in its own words, "resulting
  # in an activation failure". A warning is the wrong severity for that: it
  # scrolls past in a rekey run and the failure arrives at activation instead.
  #
  # `rekeyable` excludes a class agenix cannot rekey for (the ios phone has no
  # `age` module at all), and "holds secrets" is read from the ASPECT
  # declarations rather than the host's `age.secrets`, so this answers without
  # a host fixpoint.
  den.checks.hosts-holding-secrets-have-a-key = {
    description = "A host whose aspects declare secrets has a `hostPublicKey`, so rekeying cannot silently use a dummy key.";
    severity = "error";
    findings =
      let
        secretBearing = lib.filter (a: den.aspects.${a} ? secrets) (lib.attrNames den.aspects);
        rekeyable =
          h:
          lib.elem (h.class or "") [
            "nixos"
            "darwin"
          ];
        holdsSecrets = h: lib.any (a: h.hasAspect den.aspects.${a}) secretBearing;
      in
      lib.concatMap (
        h:
        lib.optional (rekeyable h && holdsSecrets h && (h.hostPublicKey or null) == null)
          "${h.name or "?"} declares secrets and has no `hostPublicKey`, so `agenix rekey` encrypts them to a dummy key and activation fails on that host. Set `den.hosts.<sys>.${h.name or "<n>"}.hostPublicKey` from the box's own `/etc/ssh/ssh_host_ed25519_key.pub`."
      ) checkLib.hostEntities;
  };
}
