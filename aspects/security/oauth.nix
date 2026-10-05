_:
# The OAuth 2.0 token generator's one package dependency.
#
# ./oauth/_lib.nix mints tokens with `oauth2c`, which already implements the
# authorization-code grant, PKCE S256, and the RFC 8252 section 7.3 loopback
# redirect listener -- and, crucially, sends every diagnostic to stderr
# (`pterm.SetDefaultOutput(os.Stderr)` in its cmd/log.go) while printing the
# token response to stdout. A generator's stdout IS the secret, so that split is
# the whole reason it can be driven from inside `agenix generate` at all.
#
# What it cannot do is send a non-standard authorization parameter.
# `Request.AuthorizeRequest` builds the query from a fixed allowlist of fields
# and then overwrites `URL.RawQuery` wholesale, so there is no passthrough and
# none can be smuggled in through the endpoint URL either. Google issues a
# refresh token ONLY when `access_type=offline` is on the authorization request,
# which made Google unreachable with the stock tool.
#
# ../../patches/oauth2c/authorize-params.patch adds a repeatable
# `--authorize-param key=value` that is merged into the form last. Declared
# under `den.aspects.packages` so ../tooling/overlays.nix collects it without
# anything having to include this aspect -- the generator runs against the
# perSystem package set, not a host's.
{
  den.aspects.packages.oauth2c = {
    overlays = _: {
      oauth2c = _self: super: {
        oauth2c = super.oauth2c.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [ ../../patches/oauth2c/authorize-params.patch ];
        });
      };
    };
  };
}
