# dendritic

A flake-parts "dendritic" repo on the `den` framework.

## No path imports — declare in lib scope

**Do not write `import ./foo.nix` or `import ../../lib/foo.nix`.** Put the thing
in a lib scope and read it from there.

Two reasons, and the second is the one that bites:

- **Unnecessary re-evaluation.** A path import is evaluated per call site and
  does not share a thunk with anything else that imports the same file, so the
  same work is redone. Something reached through a lib scope is evaluated once
  for the whole evaluation.
- **Files stop being relocatable.** `import ../../lib/foo.nix` hard-codes this
  file's position relative to that one, so moving either breaks the other. A
  lib-scope name does not care where either file lives, which is what lets the
  tree be reorganised — splitting a large aspect into a directory, for
  instance — without touching unrelated files.

This applies to our own helpers. Reading another flake's `lib` output is
unaffected.

## Jujutsu, not git

`jj`, never `git`. **New files must be `jj file track`ed** or Nix cannot see
them at all: a `git+file://` flake only sees tracked paths, so an untracked
`.nix` file is silently not loaded and every probe written in it silently does
nothing.

## Evaluation cost — SERIALISE FULL-HOST EVALS

**A full `nixosConfigurations.<host>.config` eval needs several GB.** This box
has 16GB. Run two or three at once and they are **OOM-killed**, which looks
like nothing at all: the process exits 0, writes no output, and logs no error.
That signature has been misread as a deadlock, as a loop, and as a stale lock,
and working code was reverted twice chasing it.

So: **one full-host eval at a time**, behind a lock.

```
flock /tmp/dendritic-eval.lock nix eval .#nixosConfigurations.secondpc.config --apply ...
```

And ask ONE question per eval rather than three — the combined form is cheaper
in peak memory, not just in wall clock:

```
nix eval --json .#nixosConfigurations.secondpc.config --apply 'c: {
  units = builtins.length (builtins.attrNames c.systemd.services);
  oauth2 = builtins.attrNames c.services.kanidm.provision.systems.oauth2;
  dbs = c.services.postgresql.ensureDatabases;
}'
```

Other things that are true but were NOT the main cause:

- **A long eval is not evidence of a loop, and 0% CPU is not evidence of a
  deadlock.** Measure the baseline before interpreting silence.
- A `kill -9`'d eval can leave a stale `~/.cache/nix/fetcher-lock-*` that blocks
  later evals. Delete it if you kill one. Real, but secondary to memory.
- Orphaned evals outlive the agent that started them. Check for and clear them
  before blaming your own change.

### The fast feedback loop

For anything in the service model's resolution layer (`lib/service-scope.nix`,
`lib/service-link.nix`), drive the libraries directly against a fixture registry
of plain attrsets instead of going through a host eval: seconds rather than
minutes, no den, no import-from-derivation, no host entities.

**The capability layer has its own version of this, and it is much faster than a
host eval.** `aspects/base/capability.nix` exports through `_module.args`, and
`debug = true` puts the whole flake config in the `debug` output, so every
resolver query can be driven against real host ENTITIES without building a
single NixOS config:

```
nix eval --impure --json .#debug.config --apply 'cfg:
  let cap = cfg._module.args.capability; h = cfg.den.hosts.x86_64-linux.secondpc;
  in { msgs = cap.exclusionsIn h; bind = cap.bindAddressFor h cfg.den.aspects.grocy "tcp" "http"; }'
```

Seconds, not minutes, because `hasAspect` is a projected lookup and a giver's
`gives` is a plain function — neither needs the host's fixpoint. Use this for
anything that only reads entities and givers (`resolveChain`, `exclusionsIn`,
`bindAddressFor`, `takersOn`, `linksBetween`); a full host eval is only needed
when the question is about rendered config.

Six full-host evals were spent chasing a check that returned `[ ]`, and the
flake-level form answered it on the first try.

## Let the module system check it

**A constraint belongs in the option's `type`. A fallback belongs in its
`default`. Neither belongs in an `if`/`throw` inside a helper function.**

This tree kept growing hand-rolled versions of checks the module system already
does, and they are worse in every way: the error says less, it fires later, and
nothing stops the next writer adding a second copy somewhere else.

| hand-rolled | what to write instead |
| --- | --- |
| `if x == null then d else x` | a non-null `default` on the option, or `lib.defaultTo d x` |
| `if lib.elem x set then x else throw …` | `types.enum set` |
| `if lib.hasInfix "@" x then throw … else x` | `types.strMatching "[^@]+"` |
| a `throw` on an unexpected attribute | `mkInstanceType { strict = true; }` / `den.schema.<kind>.isStrict` |
| folding a list to catch duplicates | the capability's own `exclusive` rule (../base/capability-types.nix) |

den's strict types already produce better messages than anything written by
hand. Measured, on a real mistake this session:

```
error: STRICT MODE: "classes" is not declared on member (instance at
den.households.ivymectin.members.ivy, …).
Fix: schema.member.options.classes = mkOption { ... };
```

For a capability, the place to put the type is `options.<n>` (what the giver
furnishes) or `taker.options.<n>` (what a consumer may state, a real
`mkOption`) in the capability definition itself — which now lives in the aspect that owns it, so
the constraint sits beside the thing it constrains.

### The two traps that make people reach for a hand-rolled check

Both of these have cost real bugs here, so the rule above is not only about
tidiness.

**`x.y or default` fires on a MISSING attribute, never on a null one.** A
declared option with `default = null` EXISTS, so `or` never runs and the null
flows on. This has caused at least six bugs in this repo — a DNS record with a
lambda as its content, a samba account named `null`, an mqtt login named
`null`, `cannot coerce null to a string` from an OIDC issuer, and an `||` that
was really a boolean or. If the attribute can be null, test it (`lib.defaultTo`)
rather than `or`.

**A RAW take carries only the keys the taker actually set.**
`aspect.takes.<cap>` is not the typed-with-defaults view — measured: a probe
aspect setting only `callback` produced a take with `callback` and no
`displayName`, although both are declared in the capability's `taker`. Defaults
materialise through `capability.contributions`. So on a raw take `or` is the
CORRECT operator (the attribute is genuinely absent), and a `default` on the
taker option does not reach it. Both readings therefore appear in
`aspects/services/kanidm.nix`, with the reason written at each site.

## Secrets

**Never run `nix run .#rekey` or `nix run .#gen-secrets`** while the master key
is mid-recovery — roughly ten secrets are currently unrecoverable, and
regenerating one replaces a value that cannot currently be decrypted.

The `secrets` workflow (`.github/workflows/secrets.yml`) is the same
operation run as the CI master; it falls under the same rule.

Never print secret plaintext. An agenix generator's stdout IS the secret, so
every diagnostic goes to stderr.

## Formatting

`nixfmt` every `.nix` file touched; `nixfmt --check` must pass.
