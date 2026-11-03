{
  den,
  lib,
  caps,
  ...
}:
# Services. A household declares a service (`den.households.<h>.services.<n>`,
# a den entity, ../../lib/service-type.nix); a host claims it with
# `services.<n>.enable`, and ./service-propagate.nix includes the service's
# aspects on every host that claims it. References point host -> household ->
# service, never back.
let
  inherit (lib) mkOption types;
in
{
  config = {
    den.schema.service = {
      parent = "household";
      inherits = [ den.schema.conf ];
    };

    den.schema.host =
      { config, ... }:
      {
        options.services = mkOption {
          type = types.attrsOf (
            types.submodule {
              options.enable = mkOption {
                type = types.bool;
                default = false;
                description = "This host serves its household's service of this name.";
              };
            }
          );
          default = { };
          example = lib.literalExpression "{ audiobookshelf.enable = true; postgres.enable = true; }";
          description = ''
            This host's CLAIMS on its household's services -- the manifest of
            what the machine runs, so one glance says what it serves. The
            service itself is declared on the household; a claim on a name the
            household does not declare is an evaluation error rather than a
            silent no-op, which is the typo-safety `attrsOf` otherwise loses.
          '';
        };

        # Claimed AND declared, with the throw. Read this, not `services`, when
        # the question is "what does this host actually serve".
        options.serves = mkOption {
          type = types.raw;
          readOnly = true;
          default =
            let
              claimed = lib.attrNames (lib.filterAttrs (_: s: s.enable) config.services);
              declared = if config.household == null then { } else config.household.services;
              missing = lib.subtractLists (lib.attrNames declared) claimed;
            in
            if missing == [ ] then
              lib.getAttrs claimed declared
            else
              throw "den.hosts.*.${config.name}.services claims ${lib.concatStringsSep ", " missing}, which ${
                if config.household == null then
                  "cannot exist: this host sets no `household`, so it has no services to claim"
                else
                  "den.households.${config.household.name} does not declare. It declares: ${
                    let
                      names = lib.attrNames declared;
                    in
                    if names == [ ] then "nothing" else lib.concatStringsSep ", " names
                  }"
              }";
          description = "The household services this host both claims and that exist, keyed by name.";
        };
      };
  };

  # ── THE CAPABILITY, DECLARED WHERE SERVICES ARE MODELLED ──
  #
  # Last of the moves out of ./capability-types.nix. "households services is
  # more of an abstraction over a capability that i think aspects should
  # provide which is a service" -- so what a service IS belongs in the file
  # that holds that abstraction, next to the two collections above, rather
  # than in the capability registry.
  # ── SOMETHING A HOUSEHOLD OFFERS ITS MEMBERS ──
  #
  # "households services is more of an abstraction over a capability that
  # aspects should provide, which is a service" -- and "the service
  # capability has character to it that household consumes".
  #
  # So the ASPECT gives it and the HOUSEHOLD consumes the character. What a
  # grant in a service MEANS is the service's own fact: vikunja's levels are
  # vikunja's, donetick maps `admin_groups` in its own config, splitpro has
  # no role model at all. ../../lib/service-type.nix's `access` option
  # already says it is "filled in by the service's own aspect" -- it just
  # was not, it was written out in ../households/ivymectin.nix, which is the
  # same fact in two places with one reader.
  #
  # WHAT IS NOT HERE, deliberately:
  #
  #   reach      already on the giver (`den.schema.aspect.reach`), and it is
  #              what resolution honours. Restating it here would be the
  #              third copy of a fact that has already had two.
  #   endpoints  the ports are `gives.tcp`/`gives.http`; a service is not a
  #              second place for them.
  #   who serves it   inclusion. A host that includes the aspect gives the
  #              service, exactly as a host that includes `tailscale` is on
  #              the tailnet -- "nothing to declare and nothing that can
  #              disagree with reality".
  # `config.den.capabilities`, not a bare `den`: this file carries explicit
  # `options.` and `config.` attributes, and mixing the two forms is "has an
  # unsupported attribute `den'" -- the same thing ./capability-types.nix's
  # `caps` export hit.
  config.den.capabilities.service = {
    meta.description = "Something a household offers its members, and what a grant in it means.";
    includes = [ caps.carried ];

    options = {
      displayName = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Human-facing name, where it differs from the aspect's own.";
      };

      # RAW, and that is not laziness: there is deliberately no central
      # `levels` enum, because Home Assistant maps groups to roles, donetick
      # has `admin_groups`, splitpro has no role model and audiobookshelf's
      # mapping is not declarative at all. Null for a service with nothing
      # to say about permissions -- nextcloud and garage both mean it.
      access = mkOption {
        type = types.nullOr types.raw;
        default = null;
        example = lib.literalExpression ''{ levels = [ "user" "admin" ]; defaultLevel = "user"; }'';
        description = "What a grant in this service means, or null where a grant would mean nothing.";
      };

      # ── WHERE A GRANT CANNOT BE HONOURED FROM NIX AT ALL ──
      #
      # plex authenticates with an X-Plex-Token minted at plex.tv and shares a
      # library in its own UI; immich and audiobookshelf keep their own
      # accounts. For those a grant is the household's REGISTER of who uses
      # what -- which ../services/homepage.nix reads to decide whose dashboard
      # shows it -- and nothing more.
      #
      # Declared rather than left silent, because the two are
      # indistinguishable otherwise: ../services/access.nix's
      # `access-grants-land` check reports a grant that realises nothing, and
      # this is how a service says the answer is "by hand, elsewhere" instead
      # of "somebody forgot".
      externallyManaged = mkOption {
        type = types.bool;
        default = false;
        description = "A grant here is a record, not an instruction: the accounts are this app's own and nothing in nix can open them.";
      };

      # A TAG, and the one field whose home is arguable: sonarr does not
      # know it is part of this house's media stack. It is furnished here
      # for provenance -- the grant that covers six services has to name
      # them from somewhere -- while the LEVELS that grant carries stay on
      # the household's `serviceGroups`, because what a grant means to
      # PEOPLE is the house's fact and not the app's.
      group = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "arr";
        description = "Grants this service alongside others carrying the same tag.";
      };
    };
  };
}
