# `aspects/`

This directory is auto-imported wholesale by `flake.nix`: every `*.nix` file
anywhere under here (any depth) whose filename doesn't start with `_` becomes
a flake-parts module. There is no manual list and no per-directory
`default.nix` aggregator to maintain — directory layout is purely
organizational, nothing wires up because of *where* a file lives.

```nix
./aspects |> fileset.fileFilter (file: file.hasExt "nix" && !hasPrefix "_" file.name)
```

## What's an "aspect"?

An aspect is a named, composable unit of config declared via
`den.aspects.<name>`, provided by the `den` flake input's dendritic
flakeModule (bootstrapped in [`framework/den.nix`](framework/den.nix)). Each
aspect can target one or more "classes" — the module system it produces
config for:

- `nixos` — a NixOS module body
- `darwin` — a nix-darwin module body
- `homeManager` — a home-manager module body
- `standaloneHome` — a standalone (non-NixOS-attached) home-manager config
- `os` — den's cross-platform battery: forwards the same body into *both*
  `nixos` and `darwin`, as long as every option it touches exists on both
  module systems (see [`dev/ccache.nix`](dev/ccache.nix) for a worked
  example)
- bare flake-parts attrs (`perSystem`, `packages`, `checks`, ...) for aspects
  that aren't host config at all

Which class(es) a file targets is **not** encoded in its path or filename —
only in the attribute keys inside the file body. The one deliberate
exception is the `nixos/` and `darwin/` top-level categories themselves,
where OS is the whole point of the grouping.

Aspects are wired to hosts via `den.schema.host.includes` (declared either in
the aspect itself or in a host file under `hosts/`), and can fan out
host-specific overrides via `provides.<hostname>.<class>` (see
[`framework/roles.nix`](framework/roles.nix) for why `provides` rather than
`policy.when` is required for that fan-out). Conditional inclusion uses
`den.lib.whenAspect` / `whenAnyAspect` / `unlessAspect`, documented in
[`framework/when-aspect.nix`](framework/when-aspect.nix).

## The `_` prefix convention

A file whose name starts with `_` (e.g. `_lib.nix`, `_internal.nix`) is a
private helper, excluded from auto-import, and manually `import`ed by name
from its sibling aspect file. Use this for shared logic that isn't itself a
standalone aspect — vendored data, internal implementation detail, a small
library a directory's other files pull in.

## Category list

Non-feature categories (special-cased, not "just another feature"):

| Dir | Contents |
|---|---|
| `hosts/` | Per-machine host definitions (`den.hosts.<system>.<name>`, host-specific `den.aspects.<hostname>` blocks, disko layouts, host-coupled checks) |
| `framework/` | Pure den/dendritic bootstrap plumbing — not a feature a host turns on, but the machinery aspects run on |
| `tooling/` | Flake-parts dev/CI/build/packaging glue: devshells, formatter, checks-that-aren't-host-specific, overlay collection, deploy, templates |
| `docs/` | README/diagram generation tooling |
| `nixos/`, `darwin/` | The one deliberate OS-axis exception — kernel, bootloader, hardware-quirk, and other OS-primitive glue that's genuinely organized by "which OS," not by feature |

Everything else is a feature/technology category (`security/`, `shell/`,
`editors/`, `desktop/`, `services/`, `apps/`, `wms/`, `input/`, `network/`,
`dev/`, `base/`, `programs/`, `nixvim/`) — this is the primary organizing
axis, because that's how you actually search ("where's my ssh config", not
"where's my nixos config").

## Nesting rule

A category is a flat file (`aspects/foo.nix`) if it holds exactly one file.
It becomes a directory (`aspects/foo/`) only once it holds 2+ files. Don't
introduce a subdirectory pre-emptively for a single file "because it might
grow later" — flatten it back if it shrinks to one.

## Where do I add a new aspect?

- **Matches an existing feature category** (ssh, a new editor, a new
  service) → add a file there, following the flat-vs-directory rule above.
- **Framework-internal** (touches `den.schema`, `den.lib`, the dendritic
  bootstrap itself) → `framework/`.
- **CI/build/packaging/dev-tooling glue, not itself a feature** →
  `tooling/`.
- **Host-specific** (only makes sense for one machine) → `hosts/<name>.nix`,
  splitting into `hosts/<name>/` only once that file is unwieldy (rough
  150–200 line guideline) *and* has genuinely separable concerns.
- **A raw NixOS/darwin/home-manager option that doesn't exist upstream**
  (like `auscybernix.tmpfiles` or `auscybernix.nix.ccache`) isn't an aspect
  at all — that's a module belonging in `extraModules/<class>/`, one level
  up from `aspects/`. An aspect then *enables* that option via
  `den.aspects.<name>`.
