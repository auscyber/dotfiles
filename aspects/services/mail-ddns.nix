{ den, ... }:
# THE ONE RECORD NOBODY CAN WRITE AT DEPLOY: where this box actually is.
#
# Every other record this household publishes follows from a declaration --
# an MX names the mail host, an SPF names the MX, a CNAME names the tunnel.
# The A record under them names an IP on a residential connection, and that
# is not a fact about the configuration: it changes when the ISP decides it
# does, with no rebuild in between.
#
# So the record's EXISTENCE is declared the same way as every other
# (`takes.dns.dynamic`, ../base/capability-types.nix), and only its VALUE is
# deferred to something running on the box. ddclient is that something.
#
# ── WHY NOT THE SYNC APP ──
#
# `nix run .#sync-dns` writes records from the admin machine with the account
# token, which is right for a record that changes when the config does and
# useless for one that changes while nobody is looking. The alternative to
# ddclient is a timer running the sync app against a host, which means the
# ACCOUNT token on a target -- and the whole design of ../network/cloudflare.nix
# is that the account token never reaches one (its header says so). ddclient
# instead holds a ZONE-SCOPED token that can edit DNS in one zone and nothing
# else: the same scope lego already gets on this host, minted by the same
# generator.
#
# ── WHAT IT CANNOT DO, STATED SO NOBODY LOOKS FOR IT ──
#
# ddclient updates address records. It has no notion of MX, TXT, SRV or
# priorities, so the mail records proper stay with the sync app; this is only
# the A record they all point at. That split is also why `dns.dynamic` is a
# list of NAMES rather than of records -- there is nothing else about them to
# say.
{
  den.aspects.mail-ddns = {
    includes = [ den.aspects.agenix-rekey ];

    # The name whose address moves. `mail.ivymect.in` and not the zone apex:
    # the apex is a proxied CNAME to the tunnel (../network/cloudflare.nix),
    # and the mail name has to stay a direct, unproxied A record -- Cloudflare's
    # HTTP edge answers nothing on 25.
    takes.dns.dynamic = [ "mail.ivymect.in" ];

    # ── A ZONE-SCOPED TOKEN OF ITS OWN ──
    #
    # `cloudflare-dns-token` mints `DNS Write` + `Zone Read` for one zone
    # (../network/cloudflare.nix), which is exactly what Cloudflare's ddns API
    # needs and no more. `format = "bare"` because ddclient reads a PASSWORD
    # FILE -- one token, no `KEY=` prefix -- where lego reads an environment
    # file; same generator, two shapes, so the scope is stated once.
    #
    # NOT lego's secret reused: that one is an env file, and a second reader
    # wanting a different shape is what `format` is for rather than a second
    # mint of the same scope.
    secrets = {
      # THE ACCOUNT TOKEN THIS MINTS FROM, under the name the generator looks
      # for. Not a duplicate of ../network/cloudflare.nix's: agenix-rekey keys
      # on the FILE, so this is the same secret, and a generator finds its
      # dependency by looking in its OWN scope -- which is why
      # ../hosts/celler2.nix declares it again for the same reason.
      api = {
        rekeyFile = ../../secrets/static/cloudflare_api.age;
        intermediary = true;
      };

      # `den.lib.cloudflare.dnsTokenSecret` and not a hand-written
      # `generator.script`: this declared the script and NOT
      # `dependencies.api`, and because agenix-rekey's `dependencies` defaults
      # to `[ ]` and may legitimately be a list, nothing caught it as a missing
      # option -- `dnsTokenScript` selected `api` on the empty list and the
      # whole `agenix generate` run failed with `expected a set but found a
      # list: [ ]`, naming neither this secret nor this file.
      ddns-token = den.lib.cloudflare.dnsTokenSecret {
        zone = "ivymect.in";
        format = "bare";
      };
    };

    nixos =
      {
        config,
        lib,
        scoped,
        host ? { },
        ...
      }:
      let
        # The names this host declared dynamic, read back off the publisher's
        # own option rather than restated here -- so adding a second dynamic
        # name is a `takes.dns.dynamic` on whichever aspect needs it, and this
        # file does not change.
        names = config.dns.dynamic;
      in
      lib.mkIf (names != [ ]) {
        services.ddclient = {
          enable = true;

          protocol = "cloudflare";
          zone = "ivymect.in";

          # `token` is Cloudflare's own sentinel for "the password is an API
          # token, not an account password with an email beside it".
          username = "token";
          passwordFile = scoped.mail-ddns.secrets.ddns-token.path;

          domains = names;

          # WHAT THE ADDRESS IS, and it has to be asked of the outside: this
          # box sits behind NAT, so its own interfaces hold a private address
          # and `webv4` is the only source that knows the public one.
          usev4 = "webv4, webv4=cloudflare-auto";

          # Five minutes. The cost of a stale A record is bounced mail for the
          # length of the window, and the cost of asking more often is a
          # request to Cloudflare -- so this is as low as it is worth being.
          interval = "5min";

          ssl = true;
        };
      };
  };
}
