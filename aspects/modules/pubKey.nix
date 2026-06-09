{ den, lib, ... }:
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

}
