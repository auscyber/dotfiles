{
  den,
  capability,
  ...
}:
# Vaultwarden, as its own aspect.
#
# Was `den.aspects.secondpc.nixos` here with the vhost in ./web.nix -- one
# service across two aspects, neither of them about it. A take is keyed by the
# taking ASPECT, so a `takes.proxy` in the host's catch-all would have been the
# HOST's and would have collided with the next service that wanted one.
let
  # ── WHERE ITS MAIL GOES, ASKED FOR RATHER THAN DESCRIBED ──
  #
  # Four facts lived in the `config` block below as literals: `127.0.0.1`, port
  # 25, "no TLS", and `admin@bitwarden.ivymect.in`. Every one of them is the
  # MAILSERVER's (../../services/mail.nix), and the last was wrong in a way
  # nothing here could have caught -- `bitwarden.ivymect.in` is not a domain
  # that server hosts, so rspamd had no key for it and no `v=spf1` record
  # authorised it. Those mails were unsigned and unaligned.
  #
  # Now this file states only what is genuinely its own: that it sends mail at
  # all, the local part the household recognises it by, and the display name.
  # The domain, the relay and the port follow.
  #
  # ── `linkFor`, WITH BOTH ENDS NAMED ──
  #
  # Not `capability.dial`: the From address is composed from THIS aspect's own
  # local part, and `dial` binds the giver's `peer` to the asking host -- so a
  # dialled link would come back addressed as `secondpc@`. The same reason
  # ../../services/mail.nix resolves its own directory link this way.
  #
  # `host = { }` is honest rather than lazy, as ../../services/tandoor-recipes.nix
  # records: `linkFor` needs a host only to hand the giver's outer scope, and
  # mail's `smtp` body reads `self`, `peer` and `resolve` -- so this is
  # answerable at FILE level, with no host fixpoint behind it.
  mail =
    let
      l = capability.linkFor den.aspects.mail "smtp" {
        host = { };
        peer = den.aspects.vaultwarden;
      };
    in
    if l != null then
      l
    else
      throw "den.aspects.vaultwarden: nothing furnishes an `smtp` endpoint, so there is no relay to send password resets through. `den.capabilities.smtp` is declared in ../../services/mail.nix beside the `gives` that answers it.";

  # ── VAULTWARDEN'S OWN SPELLING OF THE SAME THREE STATES ──
  #
  # `SMTP_SECURITY`, not the `SMTP_SSL` boolean that was here: that one is
  # deprecated upstream and cannot say "STARTTLS" at all, which is one of the
  # three values the capability furnishes. Mapped here because the NAMES are
  # vaultwarden's -- the capability says what the hop is, not what this app
  # calls it.
  security =
    {
      none = "off";
      starttls = "starttls";
      tls = "force_tls";
    }
    .${mail.security};
in
{
  den.aspects.secondpc.includes = [ den.aspects.vaultwarden ];

  den.aspects.vaultwarden = {
    includes = [ den.aspects.gateway ];

    # WAS A RAW VHOST in ./web.nix: `bitwarden.ivymect.in` -> 127.0.0.1:8222.
    # `proxy` and not `gated`: vaultwarden has its own accounts and was never
    # behind oauth2-proxy, so gating it now would put a second login in front
    # of one that already exists.
    takes.proxy = {
      subdomain = "bitwarden";
      port = 8222;
    };

    # ── IT SENDS PASSWORD RESETS AND INVITES ──
    #
    # `bitwarden` and not the aspect's own name, which is the default: this is
    # the only service in the house whose brand differs from its package, and
    # `vaultwarden@` in a From line would read as something nobody recognises.
    takes.smtp = {
      localPart = "bitwarden";
      fromName = "ivymect.in Bitwarden server";
    };

    nixos.services.vaultwarden = {
      enable = true;
      backupDir = "/var/local/vaultwarden/backup";
      config = {
        DOMAIN = "https://bitwarden.ivymect.in";
        SIGNUPS_ALLOWED = false;
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;
        ROCKET_LOG = "critical";

        SMTP_HOST = mail.relayHost;
        SMTP_PORT = mail.relayPort;
        SMTP_SECURITY = security;
        SMTP_FROM = mail.from;
        SMTP_FROM_NAME = mail.fromName;
      };
    };
  };
}
