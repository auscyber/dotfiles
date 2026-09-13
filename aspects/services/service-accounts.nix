{ den, lib, ... }:
# Bare gateway callers -- a caller with no gated API of its own, so it would
# otherwise need its own `gateway.serviceAccounts.<name>` and `api.clients`
# edits hand-written into some other aspect's file. `scripts/mint-service-
# account.sh` is the CLI that appends to ./service-accounts.json; this file is
# what turns each entry into the two things gateway.nix actually wants:
# `gateway.serviceAccounts.<name>` (so the account exists and gets a key) and
# `<name>` added to `gateway.services.<target>.api.clients` for every service
# it was minted against (so that key actually opens something).
#
# One aspect covering every registry entry, not one aspect per entry: an
# aspect only takes effect once a host's own `includes` names it, and the
# point of the registry is minting a new account without hand-editing a host
# file each time.
let
  registry = (builtins.fromJSON (builtins.readFile ./service-accounts.json)).accounts;

  # target service -> every account name minted against it, so a service that
  # gets called by three minted accounts still ends up as one contribution to
  # gateway.services.<target>.api.clients rather than three, each clobbering
  # the last.
  targets = lib.foldlAttrs (
    acc: name: entry: lib.foldl' (acc': svc: acc' // { ${svc} = (acc'.${svc} or [ ]) ++ [ name ]; }) acc entry.clientOf
  ) { } registry;
in
{
  den.aspects.service-accounts = {
    includes = [ den.aspects.gateway ];
    nixos =
      { lib, ... }:
      {
        gateway.serviceAccounts = lib.mapAttrs (_: entry: { description = entry.description; }) registry;
        gateway.services = lib.mapAttrs (_: clients: { api.clients = clients; }) targets;
      };
  };
}
