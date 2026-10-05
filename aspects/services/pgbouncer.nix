{ den, ... }:
# A CONNECTION POOLER THAT GIVES WHAT IT USES.
#
# ./postgres.nix's own header names this as the case that settles why a
# service EDGE is worth having over a quirk:
#
#   "pgbouncer is a thing that GIVES `postgres` and USES `postgres` --
#    structurally identical to `http` giving http and using tcp ... a consumer
#    still writes `uses.postgres` and reads the link; whether it terminates at
#    pgbouncer or at the server is resolution's business, so pgbouncer can be
#    inserted with NO change at any of the five consumers."
#
# So that is exactly what this is, and the test of the claim is that no
# consumer file changes. The only thing that decides which one a service gets
# is a characteristic it may filter on -- `pooled`, declared beside `over` in
# ../base/capability-types.nix.
#
# ── WHO SHOULD ACTUALLY USE IT, which is not "everyone" ──
#
# Transaction pooling multiplexes one server connection across clients between
# transactions, which is what makes it cheap and what breaks session state:
# LISTEN/NOTIFY, session advisory locks, server-side prepared statements and
# any `SET` meant to outlive a transaction. Vikunja and Donetick open a
# connection per request and are the obvious wins; Home Assistant's recorder
# holds a long-lived session and should keep the server. Nothing here decides
# that -- each consumer says `takes.postgres.pooled` when it has a reason, and
# says nothing when either will do.
#
# ── WHY IT IS NOT IN FRONT OF THE SOCKET CONSUMERS ──
#
# Five of the six consumers reach postgres over the unix socket with PEER
# authentication: the socket authenticates the connecting process's uid and
# the role is named after it, so there is no password anywhere. A pooler in
# front of that would have to re-authenticate as somebody, which means
# inventing a credential for a connection that currently needs none. So this
# gives `over = "tcp"` only, and the socket path stays exactly as it was.
let
  # The pooler's own listener. 6432 is pgbouncer's conventional port, next to
  # postgres' 5432 -- loopback only, like everything else on this box.
  port = 6432;

  # The role pgbouncer authenticates its clients AS. It needs to read password
  # hashes for whoever connects, which is what `auth_query` below does; the
  # role itself has no database of its own.
  authRole = "pgbouncer";
in
{
  den.aspects.pgbouncer = {
    includes = [
      # The server it pools for. Including it is also what guarantees they are
      # on one host, which `reach = "host"` on both then makes unarguable.
      den.aspects.postgres
    ];

    # ── THE SAME CAPABILITY THE SERVER GIVES ──
    #
    # Not `pgbouncer`, not `pooled-postgres`: a consumer asks for `postgres`
    # and resolution decides. A second capability name would mean every
    # consumer choosing a provider by name, which is the thing the edge model
    # exists to stop.
    # ── `peer` AND `resolve` AT THE INNER LEVEL, NOT THE OUTER ONE ──
    #
    # ../base/capability.nix's `linkRaw` calls `givenBy { inherit host; }` --
    # the outer body is handed the HOST only -- and passes `peer`/`resolve` to
    # the value beneath it. An outer `{ peer, ... }` is not an error:
    # `canTake` skips a body whose arguments the context cannot supply, so the
    # aspect furnishes NOTHING and the only symptom is a provider that quietly
    # is not one. ../services/postgres.nix takes them inner for this reason;
    # this file did not, and the first probe of it returned null.
    gives =
      { self, ... }:
      {
        service = _: { };

        postgres =
          { peer, resolve, ... }:
          {
            # NAMED AFTER THE ASKER, exactly as ../services/postgres.nix
            # derives it -- the database and role are the server's, and this
            # only forwards to them. Deriving it the same way is what keeps
            # the two providers interchangeable.
            database = (resolve "user" peer).name;
            user = (resolve "user" peer).name;

            # TCP ONLY. See the header: a pooler in front of peer auth would
            # need a credential the socket consumers do not have.
            over = "tcp";

            # THE CHARACTERISTIC THAT DISTINGUISHES THE TWO PROVIDERS.
            pooled = true;

            carrier = self.name;
          };

        tcp = _: {
          name = self.name;
          ports.postgres = {
            inherit port;
          };
        };
      };

    # Its own box's server, and nobody else's -- the same answer
    # ../services/postgres.nix gives, for the same reason.
    reach = "host";

    # ── WHAT IT USES, which is the other half of the claim ──
    #
    # Over the socket, as the `pgbouncer` system user, so the pooler itself
    # needs no password: peer authentication does for it exactly what it does
    # for the five socket consumers.
    takes.postgres = {
      services = [ "pgbouncer.service" ];
    };

    nixos =
      { config, lib, ... }:
      {
        services.pgbouncer = {
          enable = true;

          settings = {
            pgbouncer = {
              listen_addr = "127.0.0.1";
              listen_port = port;

              # TRANSACTION POOLING is the mode worth having; `session` mode
              # pools nothing a web app notices. The cost is the session state
              # listed in the header, which is why a consumer that needs it
              # filters on `pooled = false` rather than this being changed.
              pool_mode = "transaction";

              # ── AUTHENTICATION, WITHOUT A SECOND COPY OF EVERY PASSWORD ──
              #
              # `auth_file` would mean pgbouncer holding a hash for every role
              # -- a second credential store to keep in step with the one
              # ../base/capability-types.nix's `credential` already owns.
              # `auth_query` asks postgres instead, as `auth_user`, over the
              # socket it is peer-authenticated on.
              #
              # The query reads a SECURITY DEFINER function rather than
              # `pg_shadow` directly: a plain select there needs superuser,
              # and giving the pooler superuser to read hashes is the opposite
              # of the point. The function is created below.
              auth_type = "scram-sha-256";
              auth_user = authRole;
              auth_dbname = "postgres";
              auth_query = "SELECT * FROM pgbouncer.get_auth($1)";

              # Nothing administers it over the wire; the admin console is for
              # `SHOW POOLS` on the box.
              admin_users = authRole;

              ignore_startup_parameters = "extra_float_digits";
            };

            # EVERY DATABASE, FORWARDED TO THE LOCAL SOCKET. `*` is
            # pgbouncer's catch-all: a client connecting for database `vikunja`
            # gets a pooled connection to `vikunja` on the socket, so nothing
            # here enumerates what ../services/postgres.nix already provisions.
            databases."*" = "host=/run/postgresql auth_user=${authRole}";
          };
        };

        # ── THE SERVER SIDE OF `auth_query` ──
        #
        # A role that can read hashes through one function and nothing else.
        # `ensureUsers` creates it with no password (peer auth over the
        # socket, as everywhere here); the function and its grant are an
        # initial script, because nixpkgs' `ensure*` options cannot express
        # either.
        services.postgresql = {
          ensureUsers = [ { name = authRole; } ];

          # SECURITY DEFINER so the function runs as its owner (postgres) and
          # the pooler does not need the privilege itself. `search_path` is
          # pinned inside the function: a SECURITY DEFINER function with a
          # caller-controlled search_path is the classic privilege-escalation
          # shape.
          initialScript = lib.mkDefault (
            builtins.toFile "pgbouncer-auth.sql" ''
              CREATE SCHEMA IF NOT EXISTS pgbouncer;

              CREATE OR REPLACE FUNCTION pgbouncer.get_auth(p_usename TEXT)
              RETURNS TABLE(usename NAME, passwd TEXT)
              LANGUAGE sql
              SECURITY DEFINER
              SET search_path = pg_catalog
              AS $$
                SELECT usename, passwd FROM pg_shadow WHERE usename = p_usename;
              $$;

              REVOKE ALL ON FUNCTION pgbouncer.get_auth(TEXT) FROM PUBLIC;
              GRANT EXECUTE ON FUNCTION pgbouncer.get_auth(TEXT) TO ${authRole};
              GRANT USAGE ON SCHEMA pgbouncer TO ${authRole};
            ''
          );
        };

        # It cannot start before the server it pools for, and it has nothing
        # to pool if provisioning has not run.
        systemd.services.pgbouncer = {
          after = [ "postgresql-setup.service" ];
          requires = [ "postgresql.service" ];
        };
      };
  };
}
