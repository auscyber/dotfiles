{ den, ... }:
# The random-string generators, registered once.
#
# Most generated secrets in this repo are "N bytes of randomness, newline
# stripped" -- an OAuth2 client secret, a session-signing key, a database role
# password. Written inline that is four lines of `openssl` and `tr` per secret,
# repeated per service, and every copy is a chance to forget the `tr` (the
# values are compared byte for byte, so a trailing newline is a bug that only
# shows at runtime) or to pick a length the consumer rejects.
#
# `generator.script` takes the NAME of a registered generator, which is how
# ../network/vpn.nix's `wireguard_pair` and ../network/cloudflare.nix's token
# scripts already work. These are the generic ones:
#
#   secrets.oidc-client-secret.generator.script = "random-hex-32";
#
# Registered on `den.default` rather than on an aspect, so any scope can name
# one without first including something.
let
  # `tr -d '\n'`, always. openssl appends a newline and the consumers here
  # compare the value byte for byte -- kanidm against what a client presents,
  # postgres against what it was told, NextAuth against its own cookie.
  strip = gen: { pkgs, ... }: ''
    ${gen pkgs} | ${pkgs.coreutils}/bin/tr -d '\n'
  '';

  openssl = args: pkgs: "${pkgs.openssl}/bin/openssl ${args}";
in
{
  den.default.age.generators = {
    # 32 bytes as 64 hex characters. The default choice for a client secret or
    # an API token: hex survives every config format here unquoted.
    random-hex-32 = strip (openssl "rand -hex 32");

    # 16 bytes as 32 hex characters, for the consumers that demand exactly
    # 16/24/32 CHARACTERS rather than bytes -- oauth2-proxy's cookie AES key is
    # the one in-tree.
    random-hex-16 = strip (openssl "rand -hex 16");

    # 32 bytes base64. For a value a human may have to type or paste once (a
    # Nextcloud admin password), and for the consumers that specify base64
    # themselves (NextAuth's `NEXTAUTH_SECRET`).
    random-base64-32 = strip (openssl "rand -base64 32");
  };
}
