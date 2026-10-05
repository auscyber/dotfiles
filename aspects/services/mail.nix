{
  den,
  lib,
  inputs,
  capability,
  caps,
  ...
}:
# THE HOUSEHOLD'S MAILSERVER -- simple-nixos-mailserver, declared as an aspect.
#
# ── WHO GETS A MAILBOX IS PUSHED, NOT SCANNED ──
#
# The obvious shape is a walk: read `den.persons`, filter `mailAddresses` by
# the domains this server hosts, emit an account each. It is also the wrong
# shape, and not only on taste -- a host-scope body that reads the whole person
# registry forces every person's options for every host that merely INCLUDES
# this aspect, and the answer it computes is the same one the person already
# knows about themselves.
#
# So it goes the other way, the way ./samba-users.nix sends SMB authentication
# and ./shares.nix sends folders: a person INCLUDES `den.aspects.mailbox`, the
# emission is stamped at the USER scope -- where that person is already bound,
# so nothing is searched -- and `pipe.expose` pushes it up to the host that
# serves the mail. The host sees addresses and never sees the registry.
#
# ── AND THE SECOND CHANNEL: THE HOUSEHOLD'S MEMBERS ──
#
# That push has a limit this header used to state and accept: a person's
# `includes` reach their ACCOUNTS, so a mailbox only arrived on a host where
# that person had a unix login. Lifting it was described as "a change in
# ../base/person.nix's fan-out, not here".
#
# It went to the MEMBERSHIP instead (../base/household.nix): a member is an
# aspect-resolving entity carrying its person, so "does this person in this
# house want a mailbox" is answerable without a login. So mailboxes come from
# BOTH -- the account push above, and the household walk in the `nixos` body
# below -- and a person who is both is one mailbox, because the set is keyed by
# address.
#
# Both, rather than replacing the push, because they answer for different
# people: the membership covers everyone in the house, and the push still
# covers an account on a host that serves no household at all.
#
# ── WHICH ADDRESSES ARE LOCAL ──
#
# `hostedDomains` is the whole filter. An address whose domain is in it becomes
# a mailbox; every other address a person declares stays an external identity
# and provisions nothing -- which is why `ivy@ivymect.in` is hosted and
# `ivyp@outlook.com.au` (kanidm's primary, ../base/person.nix) is not. There is
# deliberately no second list of "local addresses" on the person: that would be
# the same fact twice, able to disagree.
#
# ── WHAT CANNOT GO THROUGH THE TUNNEL, AND WHAT CAN ──
#
# cloudflared is not HTTP-only -- its ingress takes `tcp://`, `ssh://` and
# `rdp://`, and `warp-routing` carries whole subnets. What it cannot do is
# accept a connection from a client that is not running Cloudflare software:
# a published TCP hostname is reached with `cloudflared access tcp` or through
# WARP, and raw public TCP is Spectrum, which is an Enterprise product for
# SMTP. A sending mail server is exactly an arbitrary internet client, so the
# MX has to resolve to an address the world can dial directly.
#
# That is what ./mail-ddns.nix is for, and it is why `mail.dnsRecords` below
# carries `proxied = false` on every record. The tunnel remains the right
# carrier for a webmail vhost the day one exists -- that is plain HTTPS.
let
  inherit (den.lib.policy) pipe;

  # ── THE DOMAINS THIS SERVER IS AUTHORITATIVE FOR ──
  #
  # One list, read by three things: the mailbox filter below, `mailserver.
  # domains`, and the derived DNS records. Adding a domain here is what makes
  # an address in it local.
  # ── THE SERVICE SUBDOMAIN IS A HOSTED DOMAIN, NOT AN EXCEPTION TO ONE ──
  #
  # `bitwarden.ivymect.in` is here because ../hosts/secondpc/vaultwarden.nix
  # was sending `admin@bitwarden.ivymect.in` and that address was NEITHER
  # signed NOR aligned: rspamd signs only the domains in `dkim.keyDirectory`
  # with `try_fallback = false` and `use_esld = false` (upstream
  # `mail-server/rspamd.nix`), so a subdomain gets no signature from the
  # parent's key, and `v=spf1 mx -all` was published on `ivymect.in` and
  # nowhere else.
  #
  # LISTING IT IS WHAT FIXES THAT, because everything in this file derives per
  # entry: the MX, the SPF and DMARC records, the autoconfiguration SRVs, and
  # a DKIM keypair of its own. Nothing is special-cased for a subdomain.
  #
  # ── WHY NOT WILDCARDS ALONE ──
  #
  # A wildcard SPF record is published below and does cover any other
  # subdomain, and DMARC needs no wildcard at all -- an absent `sp=` makes
  # subdomains inherit the organisational domain's policy. What a wildcard
  # CANNOT do is DKIM: `use_esld = false` is hardcoded into the generated
  # `dkim_signing.conf`, so rspamd will not sign `x@sub.example` with the
  # `example` key, and overriding that one line means replacing a whole
  # generated rspamd section. So a domain that needs a SIGNATURE is listed; a
  # domain that only needs to survive an SPF check is covered by the wildcard.
  # ── ⚠ DEPLOY ORDER, AND IT MATTERS FOR THIS ONE ──
  #
  # Adding a domain declares a DKIM secret for it (`secrets` below), and that
  # secret is NOT minted -- `nix run .#gen-secrets` is frozen while the master
  # key is mid-recovery (../../CLAUDE.md). An unminted key is normally harmless
  # here: `pathExists` guards the `.dns` sidecar, so the host evaluates and
  # simply publishes no DKIM record.
  #
  # It is not harmless at ACTIVATION. Upstream's rspamd service generates a
  # keypair for any configured domain whose key file is absent, guarded by
  # `if [ ! -f "$privkey" ]` -- which is exactly what makes our own key work
  # when it IS placed (see the `dkim_pair` generator's note). Deploy this
  # before minting and rspamd creates a key at that path itself; ours can then
  # never land without deleting the one it made, and in the meantime it signs
  # with a key no DNS record matches.
  #
  # So: `nix run .#gen-secrets` (and `.#rekey`) BEFORE the first deploy that
  # carries this domain. Both are blocked today, which is why
  # ../hosts/secondpc/vaultwarden.nix still sends as `bitwarden@ivymect.in` --
  # signable with the key that already exists -- rather than being switched to
  # `admin@bitwarden.ivymect.in` in the same change.
  hostedDomains = [
    "ivymect.in"
    "bitwarden.ivymect.in"
  ];

  # The server's own name, which is what every MX points AT and what its
  # certificate is for. A subdomain of a hosted zone rather than a bare one, so
  # ../network/cloudflare.nix's ACME path already covers it.
  fqdn = "mail.ivymect.in";

  # rspamd's DKIM selector. `mail` is upstream's default
  # (`mailserver.dkim.selector`) and the selector is part of the DNS name
  # (`<selector>._domainkey.<domain>`), so changing it is a DNS change too.
  selector = "mail";

  # Where rspamd looks for the keys. Upstream's default; stated because the
  # agenix secrets below have to land at exactly `<dir>/<domain>.<selector>.key`
  # or rspamd generates its own and signs with a key no DNS record matches.
  dkimKeyDirectory = "/var/dkim";

  # ── THE PORTS, ONCE ──
  #
  # `gives.tcp` below is what ../base/firewall.nix opens, so these numbers are
  # declared here and nowhere else -- not in a `networking.firewall` literal
  # and not in the household file (../../lib/service-type.nix reads them back
  # off this aspect).
  #
  # `expose` per port, because they are not all the same question:
  #
  #   smtp 25         THE MX PORT, and the only one a stranger dials. On the
  #                   LAN wire because that is where the router forwards from
  #                   -- which is also the honest limit of what this flake can
  #                   say about "public": it opens the wire, and whether the
  #                   router forwards 25 to it is a fact about hardware this
  #                   repo does not manage.
  #   submissions 465 implicit TLS, what every modern client uses.
  #   submission 587  STARTTLS, for the one client that still needs it.
  #   imaps 993       reading mail. LAN and tailnet both: a phone at home and
  #                   the same phone on the tailnet are the same person.
  #   sieve 4190      managesieve. NOT exposed at all: filters are edited by a
  #                   client that is already inside.
  ports = {
    smtp = {
      port = 25;
      meta.description = "The MX port: the only one a stranger dials, and the only door a service holding no credential can relay through.";
      expose = [ "lan-home" ];
    };
    submissions = {
      port = 465;
      meta.description = "Submission under implicit TLS -- how every modern client hands this server its outgoing mail.";
      expose = [
        "lan-home"
        "tailscale"
      ];
    };
    submission = {
      port = 587;
      meta.description = "Submission over STARTTLS, kept for the one client that cannot do implicit TLS.";
      expose = [
        "lan-home"
        "tailscale"
      ];
    };
    imaps = {
      port = 993;
      meta.description = "IMAP over TLS: reading mail, from a phone at home on the lan or the same phone on the tailnet.";
      expose = [
        "lan-home"
        "tailscale"
      ];
    };
    sieve = {
      port = 4190;
      meta.description = "managesieve: editing filters, from a client that is already inside.";
      expose = [ ];
    };
  };

  domainOf = address: lib.toLower (lib.last (lib.splitString "@" address));
  isHosted = address: lib.elem (domainOf address) hostedDomains;

  # The hosted domains that are not themselves under another hosted domain --
  # the zones a wildcard record belongs at. `ivymect.in` is one;
  # `bitwarden.ivymect.in` is not, because it sits under one.
  zoneApexes = lib.filter (
    d: !(lib.any (other: other != d && lib.hasSuffix ".${other}" d) hostedDomains)
  ) hostedDomains;

  # ── WHICH PORT A SERVICE RELAYS THROUGH, AND WHY IT IS THE MX PORT ──
  #
  # 25, NOT 587 OR 465, and that is forced rather than chosen. Both submission
  # ports carry upstream's `submissionOptions`
  # (`mail-server/postfix.nix`), which is
  #
  #   smtpd_client_restrictions = permit_sasl_authenticated,reject
  #   smtpd_sender_restrictions = reject_sender_login_mismatch
  #
  # -- so submission needs a SASL login AND that login must own the From
  # address. Port 25's `smtpd_relay_restrictions` is
  # `permit_mynetworks,permit_sasl_authenticated,reject_unauth_destination`,
  # which is the only door a service with no credential can walk through. See
  # `credentialFile` on the capability for why there is no credential to mint.
  relayPortName = "smtp";

  # ── THE DOMAIN A SERVICE SENDS AS ──
  #
  # The first hosted domain, which is the same choice `systemContact` below
  # makes and for the same reason: there is one house and `hostedDomains` is
  # ordered. It matters more than it looks -- rspamd signs only the domains in
  # `dkim.keyDirectory` with `try_fallback = false` and `use_esld = false`
  # (upstream's `mail-server/rspamd.nix`), so a From in anything else is
  # UNSIGNED, and `v=spf1 mx -all` is published on these names and nowhere
  # else. An address outside this list is mail that fails both checks.
  senderDomain = lib.head hostedDomains;

  # ── WHO SENDS AS WHOM, READ OFF THE TAKER'S OWN DECLARATION ──
  #
  # A `gives` body is handed the PEER and not its take, so the local part is
  # read back off the asker here -- the same shape ../services/kanidm.nix's
  # `applicationFor` has.
  #
  # `or null` FOLLOWED BY AN EXPLICIT NULL TEST, never `or <default>`: a take's
  # freeform key is MISSING when unwritten (so `or` fires) and present-and-null
  # when written as null (so it does not). ../../CLAUDE.md records five bugs
  # from that distinction.
  #
  # DEFAULTS TO THE ASPECT'S OWN NAME, which is the whole point: a service that
  # just wants to send mail declares `takes.smtp = { }` and gets
  # `<aspect>@ivymect.in` without naming a domain, a host or a port.
  # `or`, not a null test: a RAW take carries only the keys the taker set, so
  # the attribute is genuinely ABSENT when nothing stated it (../../CLAUDE.md).
  # And no `@` check here -- `taker.options.localPart` is `strMatching` below, so
  # the module system refuses an address where a local part belongs, with a
  # better message than a throw written here.
  localPartFor = aspect: lib.toLower (aspect.takes.smtp.localPart or aspect.name);

  # ── WHICH HOSTED DOMAIN, WHERE A SERVICE HAS A REASON TO DIFFER ──
  #
  # No validation here and no fallback chain: `taker.options.domain` below is
  # `types.enum hostedDomains`, so an unhosted domain is refused by the module
  # system naming the valid ones, and `senderDomain` is the option's own
  # default. This was an `if stated == null then … else if lib.elem … then …
  # else throw` -- three hand-rolled behaviours the type does better.
  domainFor = aspect: aspect.takes.smtp.domain or senderDomain;

  senderFor = aspect: "${localPartFor aspect}@${domainFor aspect}";
  fromNameFor = aspect: aspect.takes.smtp.fromName or null;

  # Who may send through this server, as far as it reaches. `takersReaching`
  # and not `takersOn` for the reason ../services/kanidm.nix gives: this aspect
  # is `reach = "household"`, so `takersOn` would see only the co-located
  # senders -- and the assertion below, whose whole job is to catch a sender on
  # another box, would have had nothing to catch.
  #
  # It is also the one query form that is SAFE FROM THE PROJECTION TRAP: it
  # walks `den.hosts` entities itself, where a `takersOn host` from a class
  # body asks a projected `hasAspect` and silently comes back empty
  # (../base/firewall.nix's header, ../services/shares.nix before it).
  smtpTakers = host: capability.takersReaching den.aspects.mail "smtp" host;

  # ── THE STAMP, COMPUTED WHERE THE PERSON IS ALREADY BOUND ──
  #
  # Everything the host needs about one human, worked out at their own user
  # scope: which of their addresses are local, which is the mailbox, and the
  # handful of facts LDAP cannot carry (aliases, a catch-all, a quota).
  #
  # `addresses` is LOWERCASED, and that is not tidying: the mailbox key IS the
  # maildir path (`<storage>/<domain>/<localpart>`), so two addresses differing
  # only in case would be two mailboxes for one person.
  mailboxFor =
    person:
    let
      hosted = lib.unique (map lib.toLower (lib.filter isHosted person.mailAddresses));

      # WHICH ONE IS THE MAILBOX. Not `lib.head mailAddresses`: that list's
      # order means something else already -- kanidm's primary is the first
      # entry (../base/person.nix) and today that is an external address. And
      # because the key is the maildir path, re-ordering the list for a kanidm
      # reason would rename the mailbox and orphan every message in it. So the
      # choice is its own field, and ambiguity is a throw rather than a guess.
      primary =
        if person.mail.primary != null then
          lib.toLower person.mail.primary
        else if lib.length hosted <= 1 then
          lib.head (hosted ++ [ null ])
        else
          throw "den.persons.${person.name}: has ${toString (lib.length hosted)} addresses in a domain this mailserver hosts (${lib.concatStringsSep ", " hosted}), so `mail.primary` has to say which one is the mailbox. The others become aliases.";
    in
    lib.optional (person.mail.enable && hosted != [ ]) {
      inherit (person) name;
      address = primary;
      # Every other local address routes to the mailbox rather than becoming a
      # second one.
      aliases = lib.unique (lib.filter (a: a != primary) hosted ++ person.mail.extraAliases);
      inherit (person.mail) catchAll quota sendOnly;
    };
in
{
  # ── THE OPT-IN ──
  #
  # Carries nothing but its emission, exactly as `den.aspects.samba-user` does
  # and for the same reason: a person's `includes` are delivered to EVERY
  # account they have, so anything else in here would land on machines that
  # serve no mail. The person comes from the stamp below.
  den.quirks.mailbox.description = "mailboxes a person asks the household's mailserver for, stamped with the person whose scope it resolved at";

  den.aspects.mailbox.mailbox = { };

  # ── SENDING MAIL, AS A CAPABILITY ──
  #
  # Declared HERE, in the aspect that serves it, per the rule
  # ../services/kanidm.nix's `den.capabilities.ldap` follows: a capability
  # belongs to its owner, and ../base/capability-types.nix keeps only the
  # vocabulary nothing owns.
  #
  # WHAT IT REPLACES. ../hosts/secondpc/vaultwarden.nix wrote out an SMTP host,
  # a port, a TLS flag and a From address -- four facts about THIS server,
  # agreed with this file by nobody -- and got two of them wrong in a way no
  # eval could see: the From was `admin@bitwarden.ivymect.in`, which is not a
  # domain `hostedDomains` covers, so rspamd signed nothing (`try_fallback =
  # false`, `use_esld = false`) and no `v=spf1` record authorised it. Every
  # service that wants to send a password reset would have copied the same four
  # lines and had the same chance of that.
  #
  # A NARROWING OF TCP, like `http`, `mqtt` and `ldap`. Two things follow and
  # both are wanted: ../base/firewall.nix still sees the port claim (so the
  # relay port is governed by the same `expose` as everything else), and a
  # consumer asking for a submission endpoint cannot be answered by some other
  # tcp giver that happens to be nearer.
  #
  # ── IT IS RESOLVED WITH BOTH ENDS NAMED ──
  #
  # `capability.linkFor <mail> "smtp" { host; peer = <me>; }`, not
  # `capability.dial`. Half of what this furnishes is PER CONSUMER -- the From
  # address is composed from the asker's own local part -- and `dial` binds the
  # giver's `peer` to the asking HOST (../base/capability.nix's `reachEndpoint`:
  # `peer = from`), so a dialled link comes back describing the machine rather
  # than the service. Same reason `dir` below uses `linkFor` for the directory.
  den.capabilities.smtp = {
    meta.description = "Permission to send mail through the household's mailserver: the relay, the From address, and whatever credential it needs. A narrowing of tcp.";
    side = "endpoint";
    includes = [ caps.tcp ];

    options = {
      # ── WHERE TO CONNECT, FROM THE LOOPBACK CARRIER RATHER THAN A LITERAL ──
      #
      # NO CARRIER COMPOSITION, and none is wanted -- the same call
      # ../services/kanidm.nix's `uris` makes, for a different reason. There it
      # is because LDAPS verifies a name; here it is because the only door a
      # service can walk through is `permit_mynetworks`, and `mynetworks` is
      # pinned to the loopback below. So the relay address is a fact about this
      # server's own box, and the constraint that the sender is on it is
      # enforced by the assertion in the `nixos` body rather than by resolution.
      relayHost = lib.mkOption {
        type = lib.types.str;
        description = "Where to open the SMTP connection. The loopback, because that is the only client `mynetworks` admits.";
      };

      relayPort = lib.mkOption {
        type = lib.types.int;
        description = "Which of the server's ports a service relays through. NOT a submission port -- see `relayPortName`.";
      };

      security = lib.mkOption {
        type = lib.types.enum [
          "none"
          "starttls"
          "tls"
        ];
        description = ''
          What the consumer should negotiate. `none` over the loopback: the hop
          does not leave the kernel, and STARTTLS on it would be a certificate
          check against a name for the sake of a wire that has no listener but
          this host.

          The other two exist because the day there is a relay on another box
          this is the field that changes, in one place, rather than a TLS flag
          per consumer.
        '';
      };

      # ── THE FROM ADDRESS, WHOLE AND IN PARTS ──
      #
      # Three fields for one fact, because the consumers genuinely disagree
      # about the shape: vaultwarden's `SMTP_FROM` is the whole address, and
      # nextcloud's `mail_from_address` is the LOCAL PART with `mail_domain`
      # beside it. Furnishing only `from` would have each of them split it back
      # up, which is the sort of thing one of them gets wrong.
      from = lib.mkOption {
        type = lib.types.str;
        description = "The envelope/From address, `<local>@<hosted domain>`. Composed here so no service writes a domain.";
      };

      fromLocalPart = lib.mkOption {
        type = lib.types.str;
        description = "Its local part alone, for a consumer that takes the two halves separately.";
      };

      fromDomain = lib.mkOption {
        type = lib.types.str;
        description = "Its domain alone. Always one of `hostedDomains` -- the taker may choose among them, which is what keeps the mail signable.";
      };

      fromName = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "The display name, echoed back from the take. Null where the consumer never stated one.";
      };

      # ── THE CREDENTIAL, WHICH IS NULL AND NOT BY OVERSIGHT ──
      #
      # There is no per-service SMTP login to hand out, and the reason is
      # structural rather than unfinished work:
      #
      #   * `mailserver.accounts` is EMPTY here on purpose (see the note on it
      #     below) -- dovecot authenticates against kanidm over LDAP, so there
      #     is no password file this repo could write an account into.
      #   * kanidm's gateway serves AUTHENTICATION BINDS and no password
      #     material, so a sender would need a kanidm APPLICATION PASSWORD --
      #     and kanidm only ever MINTS one, interactively, per person. Nothing
      #     declares one, which is the same wall ../services/grocy.nix walked
      #     into and walked back from.
      #   * a kanidm SERVICE ACCOUNT's API token is not an SMTP credential:
      #     dovecot's auth socket checks a password, and `dn=token` is an LDAP
      #     bind form.
      #   * and even with a credential, submission enforces
      #     `reject_sender_login_mismatch`, so the login would have to OWN
      #     `<service>@ivymect.in` -- i.e. the service would need a mailbox in
      #     the directory, which means a person-shaped kanidm entity per
      #     service.
      #
      # So the loopback case is modelled properly and the remote case is
      # refused loudly (the assertion in the `nixos` body) rather than answered
      # with a credential that cannot exist. This field is what a consumer
      # branches on, and it is the field that stops being null the day any of
      # the above changes.
      credentialFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to the relay credential, or null when the relay admits this consumer unauthenticated.";
      };
    };

    taker.options = {
      # ── THE TAKER STATES A LOCAL PART, OR NOTHING ──
      #
      # Nothing is the usual case and gets `<aspect>@<hosted domain>`. Stated
      # where the service is known to the household by another name --
      # vaultwarden is "bitwarden" on its vhost and in everybody's password
      # manager, and `vaultwarden@` in the From line would read as a different
      # service.
      #
      # A LOCAL PART AND NOT AN ADDRESS, which is the whole design decision
      # here: the domain is this server's fact, it is what makes the mail
      # DKIM-signable and SPF-aligned, and a consumer that could write one
      # could write one that is neither.
      localPart = lib.mkOption {
        # A LOCAL PART, and the type is what says so: `strMatching` refuses
        # anything containing an `@`, which is the whole constraint this field
        # has.
        #
        # NULL IS THE ABSENCE, and it has to be: the default was `""`, which
        # its own `strMatching` rejects. nixpkgs pushes a default in as a
        # definition at `mkOptionDefault` priority and definitions ARE
        # type-checked, so collecting this contribution for a taker that
        # states no local part threw -- latent for as long as nothing asked
        # for it, since `localPartFor` reads the raw take with `or`.
        type = lib.types.nullOr (lib.types.strMatching "[^@]+");
        default = null;
      };

      # ── AND WHICH HOSTED DOMAIN, OPTIONALLY ──
      #
      # Null means the first hosted domain, which is the right answer for
      # almost everything. Stated where a service has its own name in the
      # house, and checked against `hostedDomains` -- see `domainFor`. This is
      # NOT the loophole the `localPart`-only note above refuses: a taker picks
      # among domains this server can sign for, and still cannot write an
      # address whose domain is neither signable nor aligned.
      domain = lib.mkOption {
        # THE TYPE IS THE CHECK. An unhosted domain is unsigned and unaligned
        # (see `hostedDomains`), and `enum` refuses it while naming the ones
        # that work -- where the throw this replaces had to be written, kept in
        # step with the list, and read before it helped.
        type = lib.types.enum hostedDomains;
        default = senderDomain;
      };

      # The display name beside the address. Per consumer and not derivable:
      # "ivymect.in Bitwarden server" is the string the household recognises.
      fromName = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
      };
    };
  };

  # Stamp with the human, then push to the host that will serve it.
  #
  # `{ user, ... }` is BARE on purpose: a bare entity arg is the gate, so this
  # is dispatched only where `user` binds -- which is the only scope where
  # `user.person` exists to be read.
  den.policies.mailbox-owner =
    { user, ... }:
    let
      person =
        if user.person == null then
          throw "den.aspects.mailbox: the account '${user.name}' includes it but names no person (`users.${user.name}.person`). A mailbox belongs to a human, not to a login -- ../base/person.nix."
        else
          user.person;
    in
    [
      (pipe.from "mailbox" [
        (pipe.transform (m: m // { boxes = m.boxes or (mailboxFor person); }))
        pipe.expose
      ])
    ];

  den.schema.user.includes = [ den.policies.mailbox-owner ];

  # ── THE DKIM KEYPAIR, FROM A GENERATOR ──
  #
  # rspamd generates its own on first start (`createDkimKeypair` in the
  # upstream `mail-server/rspamd.nix`) -- and it is guarded by
  # `if [ ! -f "$privkey" ]`, which is the whole reason this works: a key put
  # there first is never regenerated, so the key can be OURS, encrypted at
  # rest, restored with the host rather than re-minted on a rebuild.
  #
  # The public half is written beside the `.age` file as a COMMITTED `.dns`
  # sidecar. That is what makes the DKIM TXT record derivable at EVALUATION --
  # the alternative is reading it back off a running host, which no DNS tool in
  # this repo can do. Same pattern as ../network/vpn.nix's `wireguard_pair` and
  # its `.pub`, and the same reason it is safe: the public half of a keypair is
  # not a secret.
  #
  # `secret.settings` carries the per-domain parameters, agenix's own channel
  # for exactly this -- so one generator serves every hosted domain and nothing
  # closes over a call-site argument. ../network/cloudflare.nix's
  # `cloudflared-token` does the same.
  den.aspects.agenix-rekey.age.generators.dkim_pair =
    {
      secret,
      file,
      lib,
      pkgs,
      ...
    }:
    ''
      tmp="$(mktemp -d)"
      trap 'rm -rf "$tmp"' EXIT

      # ── openssl, NOT `rspamadm dkim_keygen` ──
      #
      # A generator script is built for the machine running `agenix generate`,
      # which here is a Mac, and `rspamd.meta.platforms` is Linux only. So
      # referencing it did not merely fail for this secret -- it made the whole
      # `agenix-rekey.aarch64-darwin.generate` app refuse to evaluate, so NO
      # secret in the fleet could be minted from this admin host:
      #
      #   error: Refusing to evaluate package 'rspamd-4.2.2' ... because it is
      #   not available on the requested hostPlatform
      #
      # openssl is on both platforms and does the same two jobs. `-traditional`
      # asks for PKCS#1 ("BEGIN RSA PRIVATE KEY"), which is the shape
      # `rspamadm` wrote and rspamd's DKIM module reads; OpenSSL 3 defaults to
      # PKCS#8 without it. Verified against openssl 3.6.3.
      #
      # `selector` and `domain` are no longer read here -- they only ever
      # formatted rspamadm's zone fragment, and the record NAME is the
      # consumer's business. They stay in `settings`, so `derivedFrom` still
      # moves the filename when either changes.
      ${pkgs.openssl}/bin/openssl genrsa -traditional -out "$tmp/key" 2048 2>/dev/null

      # One bare string, which is what a Cloudflare API body takes: the base64
      # SPKI with the PEM header, footer and newlines stripped.
      pub="$(
        ${pkgs.openssl}/bin/openssl rsa -in "$tmp/key" -pubout -outform PEM 2>/dev/null \
          | ${pkgs.gnugrep}/bin/grep -v '^-----' \
          | tr -d '\n'
      )"
      printf 'v=DKIM1; k=rsa; p=%s' "$pub" \
        > ${lib.escapeShellArg (lib.removeSuffix ".age" file + ".dns")}

      # STDOUT IS THE SECRET. Everything above went to stderr or to a file --
      # ../../CLAUDE.md: an agenix generator's stdout IS the secret, so every
      # diagnostic goes to stderr.
      cat "$tmp/key"
    '';

  den.aspects.mail = {
    includes = [
      den.aspects.agenix-rekey
      # The certificate. `mail.ivymect.in` is in the same zone as every other
      # vhost on this box, so the existing ACME setup already covers the name;
      # the aspect is included for the cert and NOT for a vhost -- nothing here
      # is published through nginx.
      den.aspects.secondpc-web
      # The dynamic A record (./mail-ddns.nix). Included rather than left to
      # the host, because an MX whose A record is stale is the failure mode
      # this whole aspect is pointless without.
      den.aspects.mail-ddns
    ];

    # ── THE RECORDS THIS MAILSERVER NEEDS, AS A TAKE ──
    #
    # Was a `mail.dnsRecords` option that one shell script happened to read --
    # a parallel channel with no taker identity in it, which is what `share`
    # and `gated` both stopped being. Now it is an ask: whichever aspect can
    # write the zone (../network/cloudflare.nix's `publish-ivymect-in`) folds
    # every taker's `records` into one set, so this file names no zone, holds
    # no credential and knows nothing about Cloudflare.
    #
    # EVERY ONE IS UNPROXIED. An orange-clouded record resolves to Cloudflare's
    # HTTP edge, which answers nothing on 25, 465, 587 or 993 -- so an MX
    # pointing at one is an MX pointing at silence. The A record itself is not
    # here at all: ./mail-ddns.nix declares it `dynamic`, because a residential
    # address is not a fact a deploy can write.
    # ── THE DIRECTORY, ASKED FOR RATHER THAN DESCRIBED ──
    #
    # Everything dovecot needs to bind -- the URI, the base DN, the bind form,
    # the attribute names and the path its own token is minted to -- came from
    # this file, written out by hand against a server configured in
    # ../services/kanidm.nix and ../hosts/secondpc/sso.nix. Four of those five
    # facts are the directory's, so they are furnished now
    # (`den.capabilities.ldap`, declared in the aspect that serves it) and this
    # file states only the two things that are genuinely its own.
    #
    # The second one is the APPLICATION: a mail client cannot bind with a
    # person's primary credential (kanidm's gateway serves authentication binds
    # and no password material), so it binds with an application password, and
    # an application password is per application. `mail` is that application,
    # and naming it here is what makes the DN template resolvable at all.
    takes.ldap = {
      application = "mail";

      # Who may hold a mail password. The household's people, which is what
      # `media-users` is today (../hosts/secondpc/default.nix's `humanGroups`).
      # The honest source is this service's own access character once the
      # household drives it -- `den.capabilities.service` -- at which point this
      # line goes away rather than being edited.
      applicationGroup = "media-users";
    };

    takes.dns.records =
      lib.concatMap (domain: [
        {
          type = "MX";
          name = domain;
          content = fqdn;
          priority = 10;
          proxied = false;
        }
        {
          # `mx` rather than an address literal: the mechanism follows whatever
          # the MX says, so this never needs changing when the server moves.
          # `-all` because nothing else sends as this domain.
          type = "TXT";
          name = domain;
          content = "v=spf1 mx -all";
          proxied = false;
        }
        {
          type = "TXT";
          name = "_dmarc.${domain}";
          content = "v=DMARC1; p=none; rua=mailto:postmaster@${lib.head hostedDomains}";
          proxied = false;
        }
        # RFC 6186 autoconfiguration, which is what a mail client actually
        # reads. Deliberately not `autoconfig.`/`autodiscover.` CNAMEs: those
        # mean something only if a vhost serves Mozilla's and Microsoft's XML,
        # and this server serves neither. That vhost CAN go through the tunnel
        # the day it exists -- it is plain HTTPS.
        {
          type = "SRV";
          name = "_imaps._tcp.${domain}";
          data = {
            priority = 0;
            weight = 1;
            inherit (ports.imaps) port;
            target = fqdn;
          };
          proxied = false;
        }
        {
          type = "SRV";
          name = "_submissions._tcp.${domain}";
          data = {
            priority = 0;
            weight = 1;
            inherit (ports.submissions) port;
            target = fqdn;
          };
          proxied = false;
        }
      ]) hostedDomains
      # ── ONE WILDCARD SPF PER ZONE, FOR THE SUBDOMAINS NOT LISTED ABOVE ──
      #
      # `*.ivymect.in` and not a record per service: a subdomain with no SPF
      # record of its own fails an SPF check outright, and every service vhost in
      # this house is a subdomain of this zone. `mx` is the same mechanism the
      # apex publishes -- mail leaves from the box the MX points at -- so this
      # says no more than the apex does, for names that would otherwise say
      # nothing.
      #
      # ONLY the zone apexes, which is why this filters: a wildcard under
      # `bitwarden.ivymect.in` would be `*.bitwarden.ivymect.in`, covering names
      # nothing serves. `zoneApexes` is the hosted domains that are not
      # themselves a subdomain of another hosted domain.
      #
      # DMARC needs no equivalent. An absent `sp=` makes a subdomain inherit the
      # organisational domain's policy, so `_dmarc.ivymect.in` already covers
      # every name under it.
      ++ map (zone: {
        type = "TXT";
        name = "*.${zone}";
        content = "v=spf1 mx -all";
        proxied = false;
      }) zoneApexes;

    # A HOUSE AMENITY: every member reads their mail from it, and nothing
    # outside the household does. `fleet` would be wrong -- a second household
    # runs its own -- and `host` would be a lie the moment a phone on the lan
    # fetches mail.
    reach = "household";

    # WHAT IT LISTENS ON, as capabilities it gives. ../base/firewall.nix turns
    # the `expose` flags into the firewall, so `mailserver.openFirewall` stays
    # FALSE below: two openers for one port means the derived firewall is no
    # longer the only one, and `derived-port-exclusion` stops being able to see
    # a clash.
    gives =
      { self, ... }:
      let
        # ── ONE ENDPOINT HALF, FURNISHED UNDER TWO NAMES ──
        #
        # `tcp` and `smtp` carry the SAME ports, deliberately, and it is not
        # the duplication it looks like: ../base/capability.nix's `linkRaw`
        # picks exactly ONE furnished key per (aspect, capability) query -- an
        # exact match, otherwise the alphabetically first whose closure
        # contains what was asked -- so one aspect never presents two endpoint
        # halves to be compared against each other.
        #
        # Which key gets picked matters, and this is why the ports are on both.
        # ../base/firewall.nix asks for `tcp` and gets an exact match.
        # `exclusionsIn` asks for `endpoint`, which NEITHER key matches
        # exactly, so it takes the first narrowing by name -- and `smtp` sorts
        # before `tcp`. A `gives.smtp` with no ports would therefore have
        # SILENTLY WITHDRAWN this server's five port claims from the clash
        # check, which is exactly what has happened to ../services/kanidm.nix's
        # 3636: its `ldap` sorts before its `tcp` and furnishes no port, so the
        # claim its comment describes is not in fact being compared.
        endpoint = {
          # So a consumer can ask for THIS one by name -- a service wanting to
          # send mail declares `takes.smtp = { name = "mail"; }` rather than a
          # port number.
          name = self.name;

          # ── THE NAME IT IS PUBLISHED UNDER, which is how it is reached ──
          #
          # The endpoint half furnishes the SUBDOMAIN and a zone-publishing
          # carrier furnishes the domain; ../base/capability.nix's
          # `reachEndpoint` composes the two into `mail.ivymect.in` and, where
          # that carrier has no address of its own, dials the name. So this
          # file holds `mail` and ../network/cloudflare.nix holds `ivymect.in`,
          # and neither holds `mail.ivymect.in` -- the same split every gated
          # service already uses for its vhost.
          #
          # NOTE, because it is half-done: that composition needs the zone
          # aspect to furnish a `tcp` carrier half, and today it furnishes only
          # `http` -- so a tcp take still resolves over the lan or the tailnet
          # and not over this name. Giving the zone a tcp half is a real
          # decision rather than an omission: the http route is PROXIED
          # (100MB cap, Cloudflare's edge) while a mail record must be a direct
          # unproxied A, so they are two carriers with different facts and not
          # one with two protocols.
          subdomain = "mail";

          inherit ports;
          carrier = self.name;
        };
      in
      {
        # A SERVICE THE HOUSE OFFERS. No `access`: a mailbox is not a level in
        # a service, it is an address a person holds, and who holds one is
        # pushed from the person (`den.aspects.mailbox`) rather than granted
        # here.
        service = _: { };

        tcp = _: endpoint;

        # ── AND THE SENDING HALF, WHICH IS PER CONSUMER ──
        #
        # `{ peer, resolve, ... }` on the INNER level, which is where the peer
        # belongs: putting it on the outer `{ self, ... }` makes the aspect
        # furnish nothing at all, silently, because `den.lib.canTake.atLeast`
        # then refuses to apply the body.
        smtp =
          { peer, ... }:
          endpoint
          // {
            # ASKED OF THE LOOPBACK CARRIER, not written as `127.0.0.1`. The
            # address of a carrier is the carrier's fact
            # (../network/segments.nix furnishes it beside `lo`), and nothing
            # else in this tree writes that string any more.
            # ── THE SERVER'S OWN NAME ──
            #
            # `fqdn` is what every MX points at and what its certificate is
            # for, so the endpoint owns it the way kanidm's `ldap` owns
            # `auth.<zone>` -- and furnishing it is also what makes a whole
            # `smtp` link forceable: `hostname` is a required `str` carried
            # through the `tcp` narrowing, and a link that furnishes none
            # throws the moment anything forces it rather than reading named
            # fields off it.
            #
            # NOT the relay host below. A sender connects to the loopback and
            # the name is what the server CALLS itself -- the HELO name, the
            # one in the certificate -- which is a different fact from the
            # address a local client dials.
            hostname = fqdn;

            # `resolve` is the inner `gives` argument -- this same resolution
            # one level down -- so no `127.0.0.1` and no hand-written throw: a
            # host with no loopback carrier is not a state this file should be
            # describing in prose.
            # ── THE LOOPBACK'S DECLARED DIAL ADDRESS, NOT A RESOLVED LINK ──
            #
            # This asked the loopback CARRIER for a link, which is the question
            # "is A connected to B over loopback" and needs both ends. The
            # relay address is the other question -- "what is the loopback
            # address on this box" -- which is host-independent, and
            # ../../aspects/network/segments.nix already declares the constant
            # once in the roster (`dialAt`). Resolving a link for it meant
            # ../hosts/secondpc/vaultwarden.nix's deliberate `host = { }` (no
            # fixpoint at file level) produced a null address the moment the
            # loopback giver started answering `peer` honestly, and secondpc
            # stopped evaluating against this option's `types.str`.
            relayHost = den.carrierAttachments.loopback.dialAt { };

            relayPort = ports.${relayPortName}.port;

            # See `security` on the capability: the hop does not leave the
            # kernel.
            security = "none";

            from = senderFor peer;
            fromLocalPart = localPartFor peer;
            fromDomain = domainFor peer;
            fromName = fromNameFor peer;

            # NOT A CREDENTIAL THIS REPO DECLINED TO MINT -- one it cannot.
            # The reasoning is on the capability's own `credentialFile`.
            credentialFile = null;
          };
      };

    # The maildir, and only the maildir. rspamd's learned-spam and redis state
    # are deliberately not here: they rebuild, slowly, where a lost mailbox
    # does not come back at all.
    backup = _: {
      name = "mail";
      paths = [ "/var/vmail" ];
    };

    # ── THE DKIM KEYS, ONE PER HOSTED DOMAIN ──
    #
    # `path` puts each one exactly where rspamd looks, owned by rspamd, which
    # is what makes the guard described above fire.
    secrets = lib.listToAttrs (
      map (
        domain:
        lib.nameValuePair "dkim-${domain}" {
          path = "${dkimKeyDirectory}/${domain}.${selector}.key";
          owner = "rspamd";
          group = "rspamd";
          mode = "0400";
          settings = { inherit domain selector; };
          generator.script = "dkim_pair";
        }
      ) hostedDomains
    );

    nixos =
      {
        config,
        lib,
        mailbox,
        host ? { },
        ...
      }:
      let
        # ── THE DIRECTORY, RESOLVED WITH BOTH ENDS NAMED ──
        #
        # `linkFor` and not `dial`, which was the first attempt and is wrong
        # here: `dial` answers "how do I reach that from this HOST" and binds
        # the giver's `peer` to the host itself (../base/capability.nix's
        # `reachEndpoint`: `peer = from`). Half of what this link carries is
        # per CONSUMER -- the bind token is minted per asker, and the person DN
        # template names the asker's application -- so a dialled link would
        # have come back describing secondpc rather than the mailserver.
        #
        # Both ends named, as ./postgres.nix does for `user`, so kanidm's
        # `gives.ldap` is handed the mail aspect and answers about it.
        #
        # NO CARRIER COMPOSITION, and none is wanted: LDAPS verifies the NAME,
        # so the URI is the same whichever route reaches it (which is the whole
        # of `uris`' note in ../services/kanidm.nix). What a route would decide
        # is the address, and the gateway is loopback-only -- so the real
        # constraint is that the mailserver runs beside kanidm, which is what
        # `includes` on this aspect already makes true.
        # ── A DIRECTORY, NOT kanidm ──
        #
        # `linkTo` resolves the capability to whichever aspect provides it, so
        # this file names no provider. It said `linkFor den.aspects.kanidm
        # "ldap"` until now, which is the thing the capability layer exists to
        # stop: mail wants A DIRECTORY, and the day there is a second one -- or
        # kanidm is renamed -- a consumer naming it is wrong rather than
        # re-resolved.
        #
        # `linkTo` and not `dial`, because half of this link is per CONSUMER:
        # the bind token is minted per asker and the person-DN template names
        # the asker's own application. `dial` binds the giver's `peer` to the
        # asking HOST and would come back describing secondpc.
        #
        # The throw when nothing answers is the resolver's, derived from the
        # capability, this host and the aspects that could give it -- so it
        # stays true as the tree changes, which a message written here would
        # not.
        dir = capability.linkTo "ldap" {
          inherit host;
          peer = den.aspects.mail;
        };

        # Who sends through this server, from as far as it reaches. See
        # `smtpTakers` at file level for why `takersReaching`.
        senders = smtpTakers host;

        # The pooled emissions, flattened. One entry per (account, person) that
        # included `den.aspects.mailbox` and had a local address; `boxes` is
        # already the resolved shape, computed at the user scope.
        pushed = lib.concatMap (m: m.boxes or [ ]) (lib.flatten mailbox);

        # ── AND EVERY MEMBER OF THE HOUSEHOLD THIS HOST SERVES ──
        #
        # Asked rather than pushed, and it is a NARROW walk: `host.household`
        # names one household, so this forces that household's members and
        # nothing else. The objection in this file's header is to reading the
        # whole person REGISTRY from a host body -- which would force every
        # person's options on every host merely including this aspect -- and
        # this is not that.
        #
        # `hasAspect` on the member, so the opt-in is the same one an account
        # uses: a person who includes `den.aspects.mailbox` gets a mailbox,
        # whether they have a login here or not. A member who does not include
        # it is in the house and has no mail, which is the point of it being an
        # opt-in.
        members = lib.attrValues ((host.household or null).members or { });
        fromMembers = lib.concatMap (
          m: if m.hasAspect den.aspects.mailbox then mailboxFor m.person else [ ]
        ) members;

        # ONE SET. A person who is both a member here and has an account here
        # appears in both lists with the same address and the same name, and
        # `byAddress` below groups by address -- so the duplicate collapses,
        # and only two DIFFERENT people claiming one address is a collision.
        boxes = pushed ++ fromMembers;

        # ONE MAILBOX PER ADDRESS. Two people claiming one address would
        # otherwise merge into a single mailbox with both their aliases, which
        # is a privacy failure delivered as a merge.
        byAddress = lib.groupBy (b: b.address) boxes;

        collisions = lib.filterAttrs (_: bs: lib.length (lib.unique (map (b: b.name) bs)) > 1) byAddress;

        checked =
          if collisions == { } then
            byAddress
          else
            throw "den.aspects.mail: ${
              lib.concatMapStringsSep "; " (
                addr: "${addr} is claimed by ${lib.concatStringsSep " and " (map (b: b.name) byAddress.${addr})}"
              ) (lib.attrNames collisions)
            }. An address is one mailbox, so two people cannot share one -- use `mail.extraAliases` on whichever of them should receive it.";

        # ── THE PUBLIC HALF OF EACH DKIM KEY, READ BACK AT EVAL ──
        #
        # `secrets/generated/<host>/mail/dkim-<domain>.dns`, the sidecar the
        # generator wrote. `pathExists` guards it exactly as
        # ../network/_lib.nix's `hasKey` does: a host whose keys have not been
        # minted yet still evaluates, and simply publishes no DKIM record --
        # which is the honest answer, since there is no key to publish.
        dkimRecordFile = domain: ../../secrets/generated + "/${host.name or "?"}/mail/dkim-${domain}.dns";

        # A VALUE, NOT A FUNCTION. As `domain: ...` this read as non-null to
        # the `lib.optional` below, and the record went out with a LAMBDA as
        # its content -- which `builtins.toJSON` in the sync app would have
        # died on, after the record had already been accepted everywhere else.
        dkimRecordFor =
          domain:
          let
            f = dkimRecordFile domain;
          in
          if builtins.pathExists f then lib.removeSuffix "\n" (builtins.readFile f) else null;
      in
      {
        mailserver = {
          # ── A PERSON ASKING FOR A MAILBOX IS WHAT TURNS IT ON ──
          #
          # Not a hand-flipped boolean: the pipe above already answers "does
          # anybody here have mail", so a host that serves the service but has
          # nobody's mailbox on it runs no postfix. That is also what keeps
          # this harmless to include before the first mailbox exists.
          #
          # ── OR SOMETHING WANTS TO SEND ──
          #
          # A SERVICE needing the relay is the second reason to run postfix,
          # and it is independent of the first: `takes.smtp` wants a relay and
          # not a mailbox. Without this clause a box whose only mail user was
          # vaultwarden would have had the capability resolve cleanly to a
          # server that was not running -- the failure this derivation exists
          # to make impossible.
          #
          # `||` and not `or`: both operands are booleans here, which is the
          # one place that operator means what it looks like (../../CLAUDE.md).
          enable = checked != { } || senders != { };

          inherit fqdn;
          domains = hostedDomains;

          # NEW INSTALL, so the newest. Every assertion keyed on this exists to
          # force a MIGRATION on a server that already holds mail -- uuid-based
          # home directories, the sieve script move -- and there is nothing here
          # to migrate. Starting lower would mean doing those migrations later
          # for no reason.
          stateVersion = 5;

          # Where postmaster mail and DMARC reports go.
          systemContact = "postmaster@${lib.head hostedDomains}";

          # ── NO `accounts`, BECAUSE KANIDM IS THE ACCOUNT SOURCE ──
          #
          # With `ldap` on, dovecot resolves a mailbox out of the directory --
          # every address in a person's kanidm `mail` attribute reaches them,
          # which is also the alias mechanism (upstream's own `aliases` option
          # says it "does not support LDAP"). Declaring accounts here as well
          # would be a second, divergent answer to "who has mail", and each one
          # would need a password this repo cannot mint while the master key is
          # mid-recovery.
          #
          # So the pipe above is not the account list. It is what says anybody
          # wants mail at all -- `enable` above reads it -- and it is where the
          # per-person facts a directory cannot carry will go the day they are
          # needed (a quota, a catch-all): those are `mailserver` options that
          # name an address, not an account.
          accounts = { };

          # ── WHERE A SERVICE'S BOUNCES GO ──
          #
          # DERIVED FROM WHO TAKES `smtp`, so one declaration gives a service
          # its From address AND makes that address deliverable. This is the
          # second half the capability would otherwise be missing: `from` above
          # is a sending identity, and an address nothing routes collects a
          # `550 user unknown` for every bounce, every DMARC report and every
          # human who hits reply.
          #
          # `forwards` AND NOT `aliases`, and the distinction is load-bearing
          # twice over. Upstream's own note on `aliases` is that the target may
          # SEND as the alias -- which is the one thing a service address must
          # not grant, since it is in no mailbox and no person should acquire
          # it. And `aliases` "does not support LDAP" (the note on `accounts`
          # above), where `forwards` is folded into `services.postfix.virtual`
          # as a plain hash map and so works beside an LDAP directory rather
          # than instead of it.
          #
          # ⚠ THE LAST HOP DOES NOT EXIST YET. `systemContact` is
          # `postmaster@ivymect.in`, and with `accounts = { }` that address is
          # deliverable only if some person's kanidm `mail` attribute carries
          # it -- which none does today (../persons). So a bounce is currently
          # rejected one step later than it used to be. That is deliberately
          # not papered over here: the fix is a person claiming the address in
          # `mail.extraAliases` (../base/person.nix), which is where "who reads
          # the household's postmaster mail" belongs, and this forward becomes
          # correct the moment they do.
          forwards = lib.listToAttrs (
            map (t: lib.nameValuePair (senderFor t.aspect) "postmaster@${senderDomain}") (
              lib.attrValues senders
            )
          );

          # ── AUTHENTICATION IS KANIDM'S, OVER LDAP ──
          #
          # `attributes.password = null` selects AUTHENTICATION BINDS: dovecot
          # finds the entry, then binds AS that person to check the password,
          # rather than reading a hash out of the directory. That is the only
          # mode kanidm can serve -- its LDAP gateway is read-only and does not
          # hand out password material at all -- and it is why a mail client
          # ends up using a kanidm APPLICATION PASSWORD, which
          # ../../patches/kanidm-provision/application-passwords.patch exists
          # to provision.
          #
          # `dn=token` is kanidm's own bind form for a service account's API
          # token, which is what the search bind uses: a token scoped to
          # reading people, not a person's own credential.
          #
          # ⚠ PHASE B. Three things have to land before this can be turned on,
          # and each one is a deploy of its own:
          #   1. kanidm's LDAP listener (`server.settings.ldapbindaddress`) is
          #      NOT set today -- ../hosts/secondpc/sso.nix configures no LDAP
          #      at all, so there is nothing to connect to yet.
          #   2. the service account and its token, which needs the frozen
          #      generator (../../CLAUDE.md).
          #   3. the TLS question: kanidm serves LDAPS with its own ACME
          #      certificate, so the URI has to be a name that certificate
          #      covers rather than a loopback literal.
          # Until then this is `false` and no mailbox can authenticate -- which
          # is correct, because there is no credential for one either.
          ldap = {
            enable = true;

            # ALL FOUR FROM THE LINK. See `dir` above and the take below:
            # kanidm owns its URI, its base DN and its bind form, and this file
            # held copies of all three.
            inherit (dir) uris;
            base = dir.baseDn;
            scope = "sub";

            # ── THE SEARCH BIND IS A SERVICE ACCOUNT, NOT A PERSON ──
            #
            # `dn=token` is kanidm's form for binding with a service account's
            # API token. The account and the path are DERIVED from this take
            # (../services/kanidm.nix mints one per consumer), so neither end
            # names the other's file -- which is what the two of them used to
            # agree on by hand.
            bind = {
              dn = dir.bindDn;
              passwordFile = dir.tokenFile;
            };

            attributes = {
              # Kanidm's own spelling, furnished rather than assumed: it calls
              # the account name `name` where most LDAP documentation says
              # `uid`, and a filter on the wrong one finds nobody while looking
              # entirely correct.
              username = dir.usernameAttribute;
              mail = dir.mailAttribute;

              # ── AUTHENTICATION BINDS ──
              #
              # Null, and the DIRECTORY says so rather than this file: kanidm's
              # gateway is read-only and hands out no password material at all,
              # so dovecot finds the entry and then BINDS as that person to
              # check the credential. A person binds with an application
              # password (`dir.userDnTemplate`, composed from the application
              # named in the take), which is what
              # ../../patches/kanidm-provision/application-passwords.patch
              # provisions and the reason that patch exists.
              password = dir.passwordAttribute;
            };
          };

          # ── THE THINGS THAT WOULD BREAK THIS BOX ──
          #
          # `localDnsResolver` defaults to TRUE upstream and would put kresd on
          # :53. secondpc already answers on 53 (its firewall opens it and it
          # runs its own resolver), so this would be an EADDRINUSE at boot that
          # ../base/capability.nix CANNOT catch -- kresd's port is inside the
          # upstream module and in no aspect's `gives`. rspamd's RBL lookups
          # then go through the host's resolver, which is slightly worse for
          # spam scoring and much better for the box coming up.
          localDnsResolver = false;

          # clamd is ~2GB resident and this host already runs plex, jellyfin,
          # postgres, kanidm, Home Assistant, Music Assistant, Nextcloud,
          # immich, the observability stack, samba and six *arr apps.
          virusScanning = false;

          # The firewall is DERIVED from `gives` above. See the note there.
          openFirewall = false;

          # The keys are ours (see `secrets`), placed where rspamd looks.
          dkim = {
            enable = true;
            # `defaults.selector`, not `selector`: at this rev the selector is
            # per-domain with a default beside the key type and size, and the
            # flat name was removed. The option set here is the one the locked
            # input actually has -- most of the first page of search results
            # is written against the older naming.
            defaults.selector = selector;
            keyDirectory = dkimKeyDirectory;
          };

          # `x509.useACMEHost`, not `certificateScheme`: the latter is REMOVED at
          # this rev and evaluating it only earns "no longer has any effect".
          # The cert is the wildcard this box already renews, so there is no
          # second ACME account and no second challenge.
          x509.useACMEHost = "ivymect.in";
        };

        # ── `mynetworks` IS THE LOOPBACK, AND NOTHING ELSE ──
        #
        # THIS IS WHAT MAKES `credentialFile = null` TRUE. Port 25's
        # `smtpd_relay_restrictions` begins `permit_mynetworks`, and postfix's
        # built-in default is `mynetworks_style = subnet` -- the host's whole
        # /24. Neither upstream nor this file set `mynetworks`, so until now
        # every machine on the house lan could relay through 25 to ANY
        # destination (`permit_mynetworks` comes before
        # `reject_unauth_destination`), and the capability's claim that a
        # credential-free sender must be on this box was simply false.
        #
        # Pinned here rather than left implicit, because it is the only thing
        # standing between "a service on this host may send" and "an open relay
        # for the LAN". Nothing in this repo relied on the wider set: real mail
        # clients use 465/587 with SASL, inbound mail for a hosted domain is
        # admitted by `reject_unauth_destination`, and dovecot's LMTP and
        # rspamd's milter do not pass through smtpd at all.
        #
        # `services.postfix.networks`, the nixpkgs option, and not a
        # `settings.main` literal: that module folds it into `mynetworks` and
        # already owns the key.
        services.postfix.networks = [
          "127.0.0.0/8"
          "[::1]/128"
        ];

        # ── A SENDER HAS TO BE ON THIS BOX ──
        #
        # The honest enforcement of the above, and it is an ASSERTION rather
        # than a resolution clause on purpose: the capability's `rules` are
        # applied by `judge` to every candidate including the giver's own
        # uncomposed furnish, where there is no carrier to test -- so a rule
        # saying "loopback only" would reject the link at the stage that
        # chooses the provider, silently, which is the failure mode
        # ../base/capability.nix's own note on judging a take in two halves
        # describes.
        #
        # Here instead, where both ends are known and the message can name
        # them. Same shape as ../services/kanidm.nix's `applicationGroup`
        # assertion: a provisioning fact the giver can check and the taker
        # cannot.
        assertions = lib.mapAttrsToList (_: t: {
          assertion = (t.host.name or null) == (host.name or null);
          message = "den.aspects.${t.aspect.name}: takes `smtp` on ${t.host.name or "?"} while the mailserver runs on ${host.name or "?"}. A sender with no credential is admitted by `permit_mynetworks`, which is this host's loopback and nothing else -- and there is no credential to give it (`den.capabilities.smtp.credentialFile` in ../services/mail.nix says why). Either co-locate the service with the mailserver or route its mail through something that holds a mailbox credential.";
        }) senders;

        # ── THE ONE RECORD A TAKE CANNOT CARRY ──
        #
        # A take is a plain attrset with no host in scope, and this value lives
        # in a file whose path contains the host's name. So it is appended to
        # the publisher's list here, where `host` is bound -- the same option,
        # the same sync app, and no second channel.
        dns.records = lib.concatMap (
          domain:
          let
            record = dkimRecordFor domain;
          in
          lib.optional (record != null) {
            type = "TXT";
            name = "${selector}._domainkey.${domain}";
            content = record;
            proxied = false;
          }
        ) hostedDomains;

        imports = [ inputs.nixos-mailserver.nixosModules.default ];
      };
  };
}
