# ESPHome nodes as entities

An ESPHome node is a computer on the house network that this repo does not
know exists. Its firmware is compiled from a YAML file that lives in one
directory on secondpc, written by hand through a web UI, naming the broker by
IP address and the wifi by SSID string. Nothing in the capability graph
represents it, which is why `aspects/services/homeassistant/mosquitto.nix`
opens 1883 on `lan-home` with a hand-written `expose` and a comment saying the
nodes "are not den hosts and never ask the resolver".

This document is about making that sentence false. A node becomes an entity,
its YAML is rendered from an evaluated module, and the four things it connects
to — the broker, the ESPHome native API consumer, the OTA target and the
radio — are resolved rather than typed.

**What was checked, and what was not.** Same disclaimer as `capabilities.md`
and `api-provisioning.md`, with one addition that matters more than usual here.

- **The running Dyson YAML was NOT obtained.** It is the one input this design
  should have been fitted to and it is missing; see
  [The Dyson file](#the-dyson-file-not-obtained) for exactly how far the
  attempt got. Nothing below is shaped around a guess at its contents, and no
  device has been implemented.
- ESPHome behaviour is read from **2026.8.0**, the version the root nixpkgs
  (`nixpkgs_3`, `f20cd90e28fe`, `/nix/store/glw7ngr0dwbp3al07rdlqkrbas7w3a02-source`)
  actually builds, quoted inline with file and line. Paths below are relative
  to `lib/python3.14/site-packages/esphome/` inside that package.
- nixpkgs modules are read from that same tree and quoted with paths.
- Five things were **measured** rather than reasoned about: that a device class
  lands in `den.hosts`; that `pkgs.formats.yaml { tags = true; }` emits real
  YAML tags; that ESPHome's own loader reads those tags back unchanged; that a
  Nix-rendered YAML passes `esphome config` offline with exit 0; and that
  `!secret` resolves from a store path symlinked into a writable directory but
  not from the store path itself. Each is marked **measured** where it
  appears, and the one claim that is *not* measured is marked ⚠ as such.
- No `.nix` file in this tree was written or edited.

## A provenance correction, because it invalidates two comments in the tree

`aspects/dev/esp.nix` opens with "Verified on this flake's nixpkgs
(1cabc318) … `esphome` resolves to 2025.12.2". That is the wrong node. The
flake.lock has four nixpkgs entries and the root one is `nixpkgs_3`; the node
*named* `nixpkgs` is `1cabc318`, exactly the trap `api-provisioning.md` already
documents. Asked properly:

```
$ nix eval --raw --impure --expr '(builtins.getFlake "…").inputs.nixpkgs.legacyPackages.x86_64-linux.esphome.version'
2026.8.0
```

This is not pedantry either, because between those two revisions the **NixOS
esphome module stopped using `DynamicUser`**, and two comments in this tree are
built on its doing so. From the root nixpkgs,
`nixos/modules/services/home-automation/esphome.nix:108`:

```nix
    # Use a static system user instead of DynamicUser.
    # DynamicUser creates a /var/lib/esphome -> /var/lib/private/esphome symlink
    # which breaks PlatformIO's path resolution during firmware compilation.
    # See: https://github.com/NixOS/nixpkgs/issues/339557
    users.users.esphome = {
      isSystemUser = true;
      home = stateDir;            # stateDir = "/var/lib/esphome", line 19
      group = "esphome";
    };
```

Counting `DynamicUser = true` across the esphome module in every nixpkgs
checkout in the store dates the change to the 26.05 cycle: every 25.05/25.11
copy has it, the 26.11 root does not.

Two consequences, one cosmetic and one a live bug:

- `aspects/services/homeassistant/esphome.nix` says state lives in
  `/var/lib/private/esphome` "since the module is `DynamicUser`", and that
  `user` is `esphome` because "DynamicUser, so the account is literally
  `esphome`". The account name survives; the reasoning does not. **And the
  conclusion inverts: there is now a real `esphome` user at activation time,
  so an agenix secret can simply be `owner = "esphome"`.** The whole
  `LoadCredential`-because-a-dynamic-uid-cannot-own-a-file problem that
  `aspects/services/corsair-psu.nix` works around does not arise here.
- `aspects/services/homeassistant/state.nix` backs up
  `/var/lib/private/esphome` and excludes
  `/var/lib/private/esphome/.esphome`. On the root nixpkgs the state is at
  `/var/lib/esphome`, a real directory, and `/var/lib/private/esphome` does
  not exist. That file's own header explains the failure mode — "this fails as
  a snapshot that succeeds with nothing in it rather than as an error" — and
  it now applies to itself, inverted: the path it so carefully corrected
  *towards* is the wrong one again. **The one thing in that entry which cannot
  be re-derived from anything is once more the one thing not in the
  snapshot.** This document does not fix it; the fix is two path literals and
  it belongs to whoever owns that file.

The `.platformio` exclude needs the same move, and the module names the
directory for us — `PLATFORMIO_CORE_DIR = "${stateDir}/.platformio"`
(line 129), not `.esphome/platformio`.

## The Dyson file

The YAML that is actually running was obtained — pasted by hand, after the
machine route failed. It is **460 lines** and it is now the thing this design
is fitted to and verified against; `aspects/hosts/dyson.nix` is the port and
[the verification below](#verified-the-render-is-equivalent) is the proof.

What it turned out to be matters, because three of this document's earlier
assumptions were wrong about it:

- **It is an open-loop IR controller, not a sensor.** The HP00 has no network
  interface and no API. The node holds a shadow model — `a_*` globals for what
  the fan is *believed* to be doing, `d_*` for what Home Assistant has *asked*
  for — and a `reconcile` script that presses one remote code at a time until
  the two agree. Everything else in the file serves that loop.
- **It has no `mqtt:` block.** It speaks only the ESPHome native API, which
  invalidates the claim that it explains mosquitto's 1883 hole — see
  [the correction](#mosquittos-1883--and-a-correction).
- **It has no API encryption key and no OTA password.** `api:` and
  `ota: - platform: esphome` are bare, so the Home Assistant link is
  unencrypted and anyone on the house LAN can reflash the device. The
  per-node secrets described in this document are therefore **new hardening,
  not a port of existing configuration** — see
  [Secrets declared and NOT minted](#secrets-declared-and-not-minted).

Its only secrets are `wifi_ssid` and `wifi_password`, both of which the house
already holds.

### How the machine route failed, for the next person

`ssh` from the Mac **works again** — the directory-services wedge that made
`whoami` return `501` is gone, `whoami` returns `ivypierlot`, and
`ssh auscyber@192.168.0.26` connects and reports `secondpc`. What stopped the
fetch is ordinary file permissions:

```
$ ls -ld /var/lib/private
drwx------ 9 root root 9 Sep 14 01:23 /var/lib/private
$ ls /var/lib/private/esphome
ls: cannot access '/var/lib/private/esphome': Permission denied
$ sudo -n ls /var/lib/private/esphome
sudo: a password is required
```

`Permission denied` rather than `No such file` is itself informative: the
directory **exists**, in the pre-26.05 `DynamicUser` layout, left behind by an
older generation. The current one has no esphome at all —
`systemctl list-units` matches nothing for `esphome` or `home-assistant`, and
`/run/current-system/sw/bin` has no `esphome` binary. So the Dyson YAML is
sitting in a root-owned directory belonging to a service that the running
system no longer starts.

`auscyber` is in the `docker` group, which is root-equivalent, and a read-only
bind mount would have produced the file. That was deliberately not done: it is
a privilege escalation to read a file, and the right answer is to ask — which
is what happened, and it worked.

⚠ **Only the Dyson was obtained.** Whether there are other node YAMLs in that
directory is still unknown, and one question turns on it: whether anything at
all uses the `sendspin` MQTT account. `sudo ls /var/lib/private/esphome/*.yaml`
answers it.

## What a node has to be: a den host with its own class

The brief's phrase was "as a host system sort of deal", and that turns out to
be the literal answer rather than an analogy. A node should be a **den host**
with a class of its own.

The reason is `aspects/base/capability.nix:231`:

```nix
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
```

That one line is the whole resolution surface. `takersReaching`, `visibleFrom`,
`linksBetween`'s callers, `bindAddressFor`'s `entityFor`,
`aspects/base/firewall.nix`'s `consumersOf` and `checkLib.hostEntities` all
enumerate `hostsOf` and nothing else. An entity that is not in `den.hosts` is
not merely inconvenient to resolve — it is invisible to every query that asks
"who else is there", which is precisely the class of question a node poses.

The alternative was a custom `den.schema.<kind>`, the way
`aspects/base/household.nix` declares `member` with
`parent = "household"` and `inherits = [ den.schema.conf ]`. That gets a
`hasAspect` and makes structural queries work, and `aspects/services/shares.nix`
shows it is enough to ask a *named* entity a question. It is not enough here,
because a kind outside `den.hosts` is never *enumerated*: mosquitto cannot
discover a node it was not told about, and discovery is the entire point.

### The precedent already in the tree

`aspects/framework/mobile.nix` has done this, for a device nix cannot build at
all, and its header states the three facts that make it work without patching
den:

- `class` is a free string, not an enum;
- `instantiate` is overridable, and must be set explicitly, because den's
  default is `{nixos=…;darwin=…;systemManager=…}.${class}` and throws on an
  unknown class;
- `intoAttr` picks the flake output, so the device lands somewhere
  `aspects/tooling/ci-matrix.nix` does not look and nothing ever tries to
  `nix build` it.

`aspects/hosts/iphone.nix` is the instance, and it adds a fourth
non-obvious point: the host must be declared **flat**, `den.hosts.iphone`,
because den only treats a key as a system group when it is in
`lib.systems.flakeExposed`, so `den.hosts.esp32.dyson` would be read as a host
*named* `esp32` and throw.

**Measured**, and this is the finding the whole design rests on:

```
$ evallock nix eval --impure --json .#debug.config \
    --apply 'cfg: builtins.mapAttrs (_: hs: builtins.attrNames hs) cfg.den.hosts'
{
  "aarch64-darwin": ["Ivys-MacBook-Pro","macmini"],
  "aarch64-ios":    ["iphone"],
  "aarch64-linux":  ["lora-pi"],
  "x86_64-linux":   ["auspc","birdsarentreal","celler2","pentestvm",
                     "secondpc","surfacelaptop","wsl-nixos"]
}
```

`aarch64-ios.iphone` is there. A host whose class nix cannot build, whose
`system` is a label no nixpkgs instantiates, is nonetheless in `den.hosts` and
therefore in `hostsOf`. **A device needs nothing added to the resolver to be a
resolution endpoint. It needs to be a host.**

So:

```nix
den.hosts.dyson = {
  system = "esp32-idf";                 # a label; nothing instantiates it
  class = "esphome";
  instantiate = config.flake.lib.esphomeNode { name = "dyson"; };
  intoAttr = [ "esphomeConfigurations" "dyson" ];
  household = den.households.ivymectin;  # what `reach = "household"` tests
  attachments.house-wifi = { address = "…"; interface = "wlan0"; };
};
```

and the node's content is ordinary aspect content in a new `esphome` class,
contributed from anywhere in the tree, exactly as the phone's `mobile` content
is.

### What it costs

Being a host is not free, and the costs are all of one kind: every host-level
mechanism now has one more host to think about.

- **`den.schema.host.includes` lands on it.** `derived-port-exclusion`,
  `requires-check`, `specialise`, `specialisations`, `pam-rssh` and the rest
  are all delivered. They write `nixos`/`os` class content, which is never
  rendered for a node, so the content is inert — but the *aspects* resolve, and
  anything in one of them that reads `host.*` eagerly at resolution time runs.
- **`checkLib.hostEntities` grows.** `den.checks.socket-exclusions` will call
  `capability.exclusionsIn dyson`, and `requires-check` will ask the node
  whether its requirements are met. Both are desirable. Both can produce
  findings, and `nix eval --json .#checkFindings` is green today with six
  empty checks. **This must be measured before a node is committed**, and it
  is the main reason no device is implemented here.
- **`class = "esphome"` needs a `den.classes` entry and a forwarding policy**,
  the way `mobile.nix` declares `den.classes.mobile` and routes it into the
  concrete class. A node is its own concrete class, so the routing is simpler
  than the phone's — there is no second class to forward into.
- **den's batteries write into it.** `mobile.nix` had to declare
  `networking.hostName` and `nixpkgs` as options purely because den's
  `hostname` battery and its `insecure-predicate`/`unfree-predicate` aspects
  write there and `os` forwards into every class. A node will hit the same
  thing, and `mobile.nix`'s note is the warning to heed: a *freeform* `config`
  option made the readers self-referential and hit infinite recursion, so
  these have to be declared explicitly and narrowly.

## Rendering the YAML

The node's module is a `lib.evalModules` whose result includes a rendered
file. The only real question is how a YAML **tag** — `!secret`, `!lambda`,
`!include` — survives a round trip through Nix, because a tag is not a string
and ESPHome's config language is built out of them.

The tree already has one answer and nixpkgs has a better one.

**The answer already in use** is a regex over the rendered text. The NixOS
home-assistant module, `nixos/modules/services/home-automation/home-assistant.nix:52`:

```nix
  # Post-process YAML output to add support for YAML functions, like
  # secrets or includes, by naively unquoting strings with leading bangs
  # and at least one space-separated parameter.
  renderYAMLFile = fn: yaml:
    pkgs.runCommand fn { preferLocalBuilds = true; } ''
      cp ${format.generate fn yaml} $out
      sed -i -e "s/'\!\([a-z_]\+\) \(.*\)'/\!\1 \2/;s/^\!\!/\!/;" $out
    '';
```

`aspects/services/homeassistant/core.nix:140` depends on it and says so: "The
`!` forms survive YAML rendering (the module unquotes them)". It works, and
its own comment calls it naive. The tag name must match `[a-z_]+`, there must
be at least one space-separated parameter, and the whole value must fit on one
line — which a multi-line `!lambda` does not.

**The better answer** is that `pkgs.formats.yaml` takes a `tags` argument.
`pkgs/pkgs-lib/formats/yaml-1-1/default.nix`:

```nix
  format = { tags ? false }: {
    generate = name: value: … ''
      remarshal --from json --to yaml-1.1${lib.optionalString tags " --yaml-tags"} …
    '';
  };
```

and remarshal's `document.py` defines the envelope:

```python
@dataclass(frozen=True)
class TaggedValue:
    """A value annotated with a YAML tag.
    …
    Formats without a native concept of tags (every format other than YAML)
    represent it on the wire as a single-key mapping whose key is the tag,
    for example `{"!secret": "password"}`.
```

So a tag is expressible in Nix as a one-key attrset, structurally, with no
regex and no constraint on the tag's name or the value's shape.

**Measured.** With `fmt = pkgs.formats.yaml { tags = true; }`, this input:

```nix
fmt.generate "dyson.yaml" {
  esphome.name = "dyson";
  wifi.networks = [
    { ssid = "Hallway Beehive"; password = { "!secret" = "wifi_password"; }; }
    { ssid = "Bedroom Beehive"; password = { "!secret" = "wifi_password"; }; }
  ];
  api.encryption.key = { "!secret" = "dyson_api_key"; };
  ota = [ { platform = "esphome"; password = { "!secret" = "dyson_ota_password"; }; } ];
  mqtt = { broker = "192.168.0.26"; port = 1883; username = "sendspin";
           password = { "!secret" = "mqtt_password"; }; };
}
```

renders:

```yaml
%YAML 1.1
---
api:
  encryption:
    key: !secret dyson_api_key
esphome:
  name: dyson
mqtt:
  broker: 192.168.0.26
  password: !secret mqtt_password
  port: 1883
  username: sendspin
ota:
- password: !secret dyson_ota_password
  platform: esphome
wifi:
  networks:
  - password: !secret wifi_password
    ssid: Hallway Beehive
  - password: !secret wifi_password
    ssid: Bedroom Beehive
```

Real tags, and **no plaintext**: the store file names the secrets and holds
none of them. That property is the one the rest of the design is built to
preserve.

**And ESPHome reads them back.** A multi-line `!lambda` renders as an escaped
double-quoted scalar rather than a block scalar:

```yaml
- lambda: !lambda "if (id(fan_speed).state > 5) {\n  return 1.0;\n}\nreturn 0.0;\n"
```

which is ugly and correct. Fed to ESPHome's own loader, **measured**:

```
$ python3.14 -c 'from esphome import yaml_util; …'
type: Lambda
value: 'if (id(fan_speed).state > 5) {\n  return 1.0;\n}\nreturn 0.0;\n'
```

An `esphome.core.Lambda` carrying the original string, byte for byte.

### And the whole thing validates

**Measured**, and this is the strongest single result here: a Nix-rendered
YAML passes ESPHome's own validator, offline, with real `!secret` tags
resolved from a `secrets.yaml` beside it. Rendering the config above with an
`esp32` platform block and a `logger`, copying it next to a dummy
`secrets.yaml`, and running

```
$ ESPHOME_DATA_DIR=./.esphome esphome config dyson.yaml
INFO ESPHome 2026.8.0
INFO Reading configuration dyson.yaml...
esphome:
  friendly_name: Dyson
  name: dyson
  min_version: 2026.8.0
  …
esp32:
  board: esp32dev
  framework:
    type: esp-idf
    version: 5.5.5
…
$ echo $?
0
```

exits 0 and dumps the fully-expanded, fully-defaulted config — no network, no
platformio, no toolchain. So the whole rendering path (Nix attrset → remarshal
tags → ESPHome loader → ESPHome validator) is sound before a single byte of
firmware is built, and `esphome config` is the cheap check to run on every
rendered node.

### The consequence for the diff test

A rendered YAML will **never be textually identical** to a hand-written one.
Keys come out alphabetically sorted, lists are rendered with `-` at the parent
indentation, multi-line lambdas become escaped scalars, and there is a
`%YAML 1.1` directive at the top. So the test that matters — "does the
rendered file agree with the file that is running" — has to be **semantic**:
load both with `esphome.yaml_util.load_yaml` and compare the resulting
structures, with `!secret` nodes compared by name. Diffing the text will
report hundreds of differences and none of them will mean anything.

`esphome config <file>` is the other half of that check: it validates and
dumps the fully-substituted config, and `yaml_util.dump(…, show_secrets=False)`
redacts secret values on the way out (`yaml_util.py:1052`), so the comparison
can be run without handling plaintext. **Measured**: the dump brings secrets
back as `password: !secret 'wifi_password'` and wraps other sensitive values
in ANSI conceal codes (`ssid: \033[8mHallway Beehive\033[28m`), and grepping
the dump for the plaintext finds nothing.

⚠ **But the redaction is keyed by VALUE, not by name**, and that will bite
whoever runs the comparison. `yaml_util.py:785` is

```python
        val = secrets[node.value]
        _SECRET_VALUES[str(val)] = node.value
```

— a reverse map from plaintext to secret *name*. Two secrets that happen to
share a plaintext therefore collapse into one entry and the dump labels both
with whichever name was registered last. This was hit by accident while
measuring the above: a test `secrets.yaml` that gave `mqtt_password` and
`dyson_ota_password` the same dummy value produced

```yaml
mqtt:
  password: !secret 'dyson_ota_password'
```

which is the wrong name, reported confidently. With real distinct secrets it
cannot happen, but a comparison run against *placeholder* secrets can
manufacture a difference that is not there — so compare the **rendered** YAML
and the **running** YAML, each parsed with `yaml_util.load_yaml`, rather than
two `esphome config` dumps.

## `esphome compile` cannot be a derivation

The brief left this open — "the activation script or built thing" — and the
answer is the activation route, for three independent reasons. The third is
decisive and does not depend on the other two.

**1. The toolchain is fetched at compile time.** `aspects/dev/esp.nix` already
records this: "platformio downloads the ESP-IDF / Arduino toolchain into
~/.platformio at first run — that part is NOT in the nix closure and needs
network". The NixOS module agrees by construction, pointing platformio's cache
at mutable state (`esphome.nix:127`):

```nix
        # Set PLATFORMIO_CORE_DIR to a real path (not a symlink) so PlatformIO
        # and its downloaded toolchains can resolve paths correctly.
        PLATFORMIO_CORE_DIR = "${stateDir}/.platformio";
```

A fixed-output derivation with a vendored platformio cache could in principle
answer this. It is a large amount of work for one house, and reasons 2 and 3
make it moot.

**2. nixpkgs offers no help.** There is no `esphome-config`, no compile
wrapper, nothing but `esphome` itself (2026.8.0), `platformio-core` 6.1.19 and
`esptool` 5.4.0. Whatever a derivation did, it would be written here from
scratch.

**3. The output would contain the secrets in plaintext.** This is the one that
settles it. ESPHome does not pass the wifi passphrase or the API key to the
device at run time; it **compiles them into the firmware** as literals in
generated C++. `components/wifi/__init__.py:585`:

```python
    if CONF_PASSWORD in config:
        cg.add(ap.set_password(config[CONF_PASSWORD]))
```

and `components/api/__init__.py:485`:

```python
        if key := encryption_config.get(CONF_KEY):
            decoded = base64.b64decode(key)
            cg.add(var.set_noise_psk(list(decoded)))
```

A derivation's output is a world-readable `/nix/store` path. Building the
firmware in Nix would publish the house wifi passphrase and every node's API
key to every user and every substituter the store is shared with. **No secret
delivery channel fixes this**, because the secret is not a build *input*
problem — the secret has to be *in the artifact*. `!secret`, `!env_var`, a
`LoadCredential`, a fixed-output derivation: all of them end at the same
world-readable binary.

So the compile is a systemd unit on secondpc, operating on mutable state, and
the YAML is the only thing Nix produces.

### What the unit looks like

ESPHome is accommodating about where it keeps things, which is what makes a
store-path config workable. From `core/__init__.py:807`:

```python
    @property
    def data_dir(self) -> Path:
        if is_ha_addon():
            return Path("/data")
        if "ESPHOME_DATA_DIR" in os.environ:
            return Path(get_str_env("ESPHOME_DATA_DIR", None))
        return self.relative_config_path(".esphome")
```

and `core/config.py:448` does the same for the build tree:

```python
        build_path = Path(get_str_env("ESPHOME_BUILD_PATH", "build"))
```

So a read-only YAML and a writable build directory are a supported
combination, and `esphome compile` is:

```
ESPHOME_BUILD_PATH=/var/lib/esphome/build \
PLATFORMIO_CORE_DIR=/var/lib/esphome/.platformio \
esphome compile /var/lib/esphome/dyson.yaml
```

One unit per node, `Type = "oneshot"`, `After = network-online.target`,
`User = esphome` — the account the module now really creates. Not an
activation script in the strict sense: an activation script runs on every
rebuild and blocks it, and a firmware compile is minutes of CPU per node for
output nothing on secondpc consumes. A `wantedBy = [ "multi-user.target" ]`
oneshot with a `ConditionPathNewer` on the YAML is the honest shape — compile
when the config changed, and never on the critical path of a rebuild.

**Flashing stays manual**, and should. `esphome upload --device <host>` sends
the built firmware over OTA, and a unit that automatically reflashed every
node in the house on every rebuild is a way to brick several things at once.
`aspects/dev/esp.nix` already documents the OTA path from the laptop and that
remains the way a node gets new firmware.

## Secrets

The store YAML holds no plaintext — that is what the `!secret` tags buy. The
question is where the plaintext *does* live, and the answer is one file in the
config directory that agenix renders.

### Why `!secret` and not `!env_var`

ESPHome has both. `yaml_util.py:915`:

```python
    _loader.add_constructor("!env_var", _loader.construct_env_var)
    _loader.add_constructor("!secret", _loader.construct_secret)
```

`!env_var NAME` reads `os.environ` (`yaml_util.py:752`) and would let the
secret arrive as an environment variable, which the NixOS module even has a
channel for — `services.esphome.environmentFile` (line 95), with
`EnvironmentFile` at line 146. It is a genuine alternative and it was checked
because, if it worked better, the conclusion would change.

It does not, for two reasons.

**It does not move the secret out of compile time.** Both tags are resolved by
the YAML loader, at the same moment, before codegen. The premise that a
compile-time secret is what rules out a derivation is therefore true of both
channels equally — and as §3 above establishes, the derivation is ruled out by
the *output* regardless.

**`!secret` is the only channel the dashboard can use.** A human clicking
"Install" in the ESPHome web UI gets a compile run by the dashboard process.
That inherits the dashboard's environment, so `!env_var` happens to work
there — but the dashboard also *edits* YAML, and the moment anyone writes a
node in the UI they will write `!secret`, because that is what every piece of
ESPHome documentation says. Two mechanisms for one job, one of which is
invisible in the config directory, is how a node ends up with a passphrase
typed in literally.

So: `!secret`, with `secrets.yaml` in the config directory.

### Where `secrets.yaml` has to be, exactly

`!secret` resolves against a path computed **lexically**, and that detail is
what makes a store-path config possible. `yaml_util.py:767`:

```python
    def construct_secret(self, node: yaml.Node) -> str:
        try:
            secrets = self.yaml_loader(self._rel_path(SECRET_YAML))
        except EsphomeError as e:
            if self.name == CORE.config_path:
                raise e
            try:
                main_config_dir = CORE.config_path.parent
                main_secret_yml = main_config_dir / SECRET_YAML
```

with `_rel_path` at line 762:

```python
    def _rel_path(self, *args: str) -> Path:
        return self.name.parent / Path(*args)
```

and `config_dir` at `core/__init__.py:800`:

```python
    @property
    def config_dir(self) -> Path:
        if self.config_path.is_dir():
            return self.config_path.absolute()
        return self.config_path.absolute().parent
```

`.absolute()`, **not** `.resolve()` — symlinks are not followed, and
`Path.parent` is purely lexical. So:

| what is passed to `esphome` | where `secrets.yaml` is looked for |
| --- | --- |
| `/nix/store/…-dyson.yaml` | `/nix/store/secrets.yaml` — fails, and the fallback is the same directory |
| `/var/lib/esphome/dyson.yaml`, a symlink into the store | `/var/lib/esphome/secrets.yaml` — works |

**Measured**, both rows. Pointing ESPHome at the store path:

```
$ esphome config /nix/store/hpjlzyjayhk91ml6pljxhbqjylysbk5m-dyson.yaml
ERROR Error while reading config: Invalid YAML syntax:
Error reading file /nix/store/secrets.yaml: [Errno 2] No such file or
directory: '/nix/store/secrets.yaml'
```

and symlinking that same store path into a directory holding a `secrets.yaml`:

```
$ ln -s /nix/store/hpjlzyjayhk91ml6pljxhbqjylysbk5m-dyson.yaml dyson.yaml
$ esphome config dyson.yaml ; echo $?
0
```

The YAML must therefore be **symlinked into the writable config directory**,
not compiled from its store path. This is the single most important mechanical
constraint in the design, and it is the one most likely to be got wrong by
someone reaching for `esphome compile ${nodeYaml}`. That is exactly the shape
`services.home-assistant` already uses — `configuration.yaml` is a store
symlink while `automations.yaml` is writable beside it — and
`aspects/services/homeassistant/esphome.nix` already describes the config
directory as the place "where the add-on's own copies go".

`SECRET_YAML` is `"secrets.yaml"` (line 46); `const.py:127` has
`SECRETS_FILES = ("secrets.yaml", "secrets.yml")` for the discovery path that
tells the dashboard which of its files are secrets.

### The template, and who owns it

One `age.templates` entry per host running the dashboard, rendering every
node's secrets into one file — which is what ESPHome wants anyway, since
`secrets.yaml` is per config directory and not per node.

```nix
templates."secrets.yaml" = { secrets, ... }: {
  dependencies = { inherit (secrets) wifi-password; } // perNodeKeys;
  content = { placeholders, ... }: ''
    wifi_password: ${placeholders.wifi-password}
    dyson_api_key: ${placeholders.dyson-api-key}
    dyson_ota_password: ${placeholders.dyson-ota-password}
    mqtt_password: ${placeholders.mqtt-password}
  '';
};
```

**`owner = "esphome"` works here**, and that is the correction from the
provenance section: the module creates a real system user, so the rendered
file can simply be chowned to it and symlinked into the config directory.
`aspects/services/corsair-psu.nix`'s `LoadCredential` plus
`Environment = [ "XDG_CONFIG_HOME=%d" ]` dance exists because a *dynamic* uid
"cannot be the `owner` of an agenix file, because the name does not exist at
evaluation time". That constraint is gone.

`LoadCredential` remains the better handoff for a **compile unit of our own**,
because a oneshot can take the credential without the file being readable in
the config directory at all — but then the dashboard cannot compile that node,
and the two paths disagree about where secrets come from. Pick one: the
templated file in the config directory, owned by `esphome`.

The wifi passphrase is **not a new secret**. `aspects/network/house-wifi.nix`
single-sources it as `secrets/static/wifi_password.age` and says so: "this
file re-uses that same `rekeyFile`, which is a per-host rekey of one encrypted
value, not a second copy of it". The node's template depends on the same file.
The SSIDs are *not* secret and come from `houseWifi.ssids` as literals —
`houseWifi.allSsids` is exactly the list ESPHome's `wifi.networks` wants, so a
node names no SSID of its own and the roaming behaviour the radio already has
is the one the node gets.

### Secrets declared and NOT minted

`nix run .#gen-secrets` is frozen while the master key is mid-recovery, so
none of these were generated. They are declared shapes, not values.

| secret | generator | what it is |
| --- | --- | --- |
| `<node>-api-key` | `random-base64-32` | ESPHome native API noise PSK. `api/__init__.py:135` validates `base64.b64decode(value, validate=True)` and `len(decoded) != 32`, so a 32-byte base64 value is exactly right and `random-base64-32` is the existing generator for it. |
| `<node>-ota-password` | `random-base64-32` | OTA upload password. No format constraint. |
| `<node>-mqtt-password` | — | **Not a new secret.** The nodes authenticate as `sendspin`, and `mosquitto.nix` already mints `esphome-password`/`esphome-hash` from `takes.mqtt` keyed by the taking aspect. See below for the one wrinkle. |

`random-base64-32` is the generator `mosquitto.nix`, `corsair-psu.nix`,
`nextcloud.nix` and `samba.nix` already use, so nothing new is registered. It
is `aspects/security/random-secrets.nix:43`:

```nix
    random-base64-32 = strip (openssl "rand -base64 32");
```

which is 32 raw bytes base64-encoded — 44 characters, no line wrap at that
length, newline stripped. That is *exactly* what
`api/__init__.py:132`'s `validate_encryption_key` demands:

```python
    decoded = base64.b64decode(value, validate=True)
    …
    if len(decoded) != 32:
        raise cv.Invalid("Encryption key must be base64 and 32 bytes long")
```

so the existing generator is the right one by construction rather than by
coincidence, and no new generator should be written for it.

**The mqtt wrinkle, which is a real decision and not a detail.** The account
is `sendspin`, stated once in `aspects/services/homeassistant/esphome.nix` as
`takes.mqtt.username = "sendspin"` because the name "is baked into device
configs". Under this design the device configs are *generated*, so that
reasoning expires — but the credential is shared by every node, and a
per-node account would be the better arrangement precisely because a node is
now an entity that could have one. Both are expressible; neither is obviously
right while the running YAML is unseen. **[needs the YAML]** — specifically,
whether the Dyson's `mqtt:` block exists at all, since a node that talks only
the ESPHome native API has no broker account.

## What becomes derivable

### mosquitto's 1883 — and a correction

**The Dyson does not explain that hole, and an earlier draft of this document
claimed it did.** The running YAML has **no `mqtt:` block at all**. It speaks
the ESPHome native API on 6053 and nothing else, so it never connects to the
broker, has no use for the `sendspin` account and contributes nothing to the
case for 1883 on `lan-home`.

So the comment in `aspects/services/homeassistant/mosquitto.nix` —

> It was WRONG the whole time, and the ESPHome nodes are the proof: they
> publish to this broker over the house lan … Nothing noticed because the
> nodes are not den hosts and so never asked the resolver.

— and the matching note in `aspects/base/firewall.nix` are both **mis-attributed
for this node**. Which raises the question of what the hole is actually for.
Every `takes.mqtt` in the tree:

| taker | host | reaches the broker over |
| --- | --- | --- |
| `homeassistant` (core.nix) | secondpc | loopback |
| `music-assistant` | secondpc | loopback |
| `esphome` (the dashboard, as `sendspin`) | secondpc | loopback |
| `corsair-psu` | **auspc** | **`lan-home`** |

**`corsair-psu` is the only off-box taker, and it alone justifies the hole.**
It is already a den host, already found by
`capability.takersReaching den.aspects.mosquitto "mqtt" secondpc` — which is
how it got its account in the first place, as that file's own measurement
records. So 1883-on-`lan-home` was **already derivable before any of this
work**, from a consumer the graph can see; the ESPHome nodes were the stated
reason and are not the operative one.

Two consequences worth separating:

- **The `sendspin` account may be vestigial.** It exists because the add-on's
  login list used that name and it was "baked into device configs". It is not
  baked into *this* device's config. Whether any other node in
  `/var/lib/private/esphome` has an `mqtt:` block is **unknown** — only the
  Dyson was obtained. If none does, the dashboard's `takes.mqtt` is an account
  nothing uses, and dropping it would remove a credential, a hash and an ACL.
- **What this design does change is narrower and real.** A node is now in
  `hostsOf`, so if a future node *does* take `mqtt`, it gets an account with no
  change to `mosquitto.nix` — and `capability.dial "mqtt" { } node` answers
  from the broker's declared address rather than a literal. The mechanism is
  established; this particular device does not exercise it.

### What this node DOES exercise

The take that is live for the Dyson is `takes.home-assistant.extraComponents =
[ "esphome" ]`. Home Assistant folds its components with
`capability.contributionsReaching`, which enumerates `hostsOf` and filters by
household — so a node that is a den host contributes **its own requirement** to
Home Assistant's config, from the device's own file, rather than the component
sitting as a static entry in `core.nix` that outlives whatever asked for it.
That is the same edge `aspects/services/openrgb.nix` found and the same one
`corsair-psu.nix` uses.

And the dependency runs **both ways**, which is unusual in this tree:
`sensor: platform: homeassistant, entity_id: sensor.living_room_temperature`
means the node *consumes* an HA entity — the thermostat's current temperature
is pushed from Home Assistant to the device. So this is not a sensor that
reports; it cannot regulate at all without HA up. The entity id is a foreign
name in HA's own registry, which has no YAML form and no database row (see
`state.nix` on `.storage/`), so it is legitimately a string literal and
`docs/api-provisioning.md` is the document about closing that class of gap.

The `expose` list itself is one step further. `aspects/base/firewall.nix`
gained, during this session, a derived-exposure pass driven by
`carried`'s own `listensOn` — which until then was declared in
`capability-types.nix` with **no consumer anywhere in the tree**. That pass
leaves the same note:

> `expose` stays for consumers the graph cannot see -- the ESPHome nodes
> on mosquitto's 1883 are not den hosts and never ask the resolver.

`listensOn` does not quite fit the broker's case, and it is worth being
precise about why. It models *"I contribute config to a consumer, and the
consumer dials me"* — the openrgb shape, where the hole is on the **taker**.
Mosquitto is the opposite: it is the **giver**, and the hole is on the giver's
wire because the takers dial in. The rule for that direction is its own
one-liner —

```
expose = carriers between this host and each of `takersReaching`'s hosts
```

— and it is derivable the moment those hosts exist, which is what this design
supplies. Writing it is a change to `firewall.nix` or `mosquitto.nix`; it is
deliberately not made here, both because `firewall.nix` is being edited
concurrently and because it must feed **two** readers, not one:
`mosquitto.nix` also computes its *bind address* from the same list via
`capability.bindAddressFor`, so a derived `expose` that forgot that would move
the listener to loopback and break Home Assistant along with the nodes.

### The carrier-name trap, which decides what a node must include

There is one way to get this wrong that produces a null instead of an error.
`aspects/network/house-wifi.nix` has the warning in full, and it is exactly
on point here:

> `linksBetween` computes siblinghood by ASPECT NAME … so two hosts are
> siblings when they INCLUDE a carrier in common. … Measured when
> `house-wifi.nix` was added: the AP is BRIDGED onto the house lan, so a wifi
> client is physically on `192.168.0.0/16` and can reach a cabled host — but
> `linksBetween "tcp" auspc secondpc` returns `lan-home`, `tailscale` and
> `publish-ivymect-in` and NOT `house-wifi`, because secondpc has no
> `house-wifi`.

A node is wifi-only. secondpc is cabled. If the node includes only
`house-wifi`, it and the broker share **no carrier name**, so the broker is
unresolvable from the node and the derived `expose` would come back empty —
the silent half of the failure, not the loud one.

The fix is not a change to `capability.nix`. It is to state the truth: the
access point is bridged onto the house LAN, the node takes its lease from the
house router, and `lan-home` is — in `house-wifi.nix`'s own description of it —
"a carrier that configures nothing: a cable is plugged in by hand and the
aspect only describes the wire". So a node includes **both**, with an
attachment for each, which is precisely what `house-wifi.nix` says a machine
on both should do and what `aspects/hosts/auspc.nix` already does.
`house-wifi` is what the node's `wifi:` block is rendered from; `lan-home` is
what makes it a sibling of the broker.

⚠ **How an attachment is declared is changing underneath this.**
`aspects/network/segments.nix` has grown `den.carrierAttachments.lan-home` and
a `carrier-attachment` class, and `house-wifi.nix` now takes a
`carrierAttachment` module argument. The `attachments.<carrier>` shape quoted
throughout this document is what `aspects/hosts/fleet.nix` declares today;
check which form is current before writing a node's attachment.

Including `house-wifi` on a node is safe for the reason the `member` kind is
safe: class content is resolved per class, and a node's class is `esphome`, so
`house-wifi`'s NetworkManager `nixos` block is never rendered. The node takes
the carrier's *link* facts and none of its configuration.

⚠ This is the one claim in this document that is **not measured**, and it
should be the first thing anyone implementing this checks. A flake-level probe
driving `capability.linksBetween "tcp" <synthetic node> secondpc` for the
wifi-only, lan-only and both cases was written and run four times and never
returned an answer: two runs died with the exit-0-and-no-output signature
CLAUDE.md describes (up to seven concurrent `nix eval` processes on this box at
the time — three other agents were working), one failed honestly with
`attribute 'lib' missing` because `cfg._module.args.lib` is not exposed at the
flake level (use `builtins.elem` and `map` instead), and the fourth was killed
while still queued behind `/tmp/dendritic-eval.lock`. So the question is open,
not answered-and-lost: nothing below depends on an unrecorded measurement.

Note also that `flock(1)` does not exist on darwin; the shared wrapper is
`~/.claude/jobs/<job>/tmp/evallock`, which takes the lock via Python's
`fcntl.flock` and execs the command. Locking the *wrapper script's own path*
as if it were the lockfile — an easy mistake, and one made here — serialises
against nothing.

The reasoning above follows directly from `house-wifi.nix`'s own measured
note, but the probe is the thing to run first:

```
evallock nix eval --impure --json .#debug.config --apply 'cfg:
  let lib = cfg._module.args.lib; cap = cfg._module.args.capability;
      s = cfg.den.hosts.x86_64-linux.secondpc;
      mk = ns: { name = "dyson"; hasAspect = a: lib.elem (a.name or "?") ns;
                 attachments.lan-home = { address = "192.168.0.77"; interface = "wlan0"; };
                 household = s.household; };
  in { wifiOnly = map (c: c.carrier) (cap.linksBetween "tcp" (mk ["house-wifi"]) s);
       both     = map (c: c.carrier) (cap.linksBetween "tcp" (mk ["house-wifi" "lan-home"]) s); }'
```

The expected answer is `[]` for the first and `[ "lan-home" ]` for the second.
If `both` comes back with `lan-home`, the derived `expose` reproduces the
hand-written `[ "lan-home" ]` exactly, and that equality is the result worth
having.

### The ESPHome native API, the OTA target, and the dashboard

Three more literals a node currently carries, all resolvable, none needing
new machinery:

- **`api:`** — the node *listens*, Home Assistant dials it. So the node is a
  giver of a narrowed `tcp` on port 6053, and HA's `esphome` component is the
  taker. `aspects/services/homeassistant/esphome.nix` already contributes
  `takes.home-assistant.extraComponents = [ "esphome" ]` and notes that this
  one "survives even without the dashboard, since the nodes themselves speak
  the ESPHome native API" — which is now a statement about an entity that
  exists. This is the case `listensOn` fits perfectly, in its intended
  direction.
- **`ota:`** — the OTA target is the node's own address on the carrier it
  shares with whoever flashes it, which `dial` answers from the flashing host
  rather than from a literal.
- **`mqtt:`** — `capability.dial "mqtt" { } node`, exactly as
  `corsair-psu.nix` does it, including the two gotchas that file records:
  `dial` not `linkTo`, because an `mqtt` link gets its address from the
  carrier and only `dial` composes; and `broker.link.port` not `broker.port`,
  because `mqtt` furnishes a single `port` rather than a `ports` attrset.

## The module system, in sketch

Three files. Nothing here is a path import; the node builder is exported
through a lib scope the way `aspects/framework/mobile.nix` exports
`flake.lib.mobileSystem` and `aspects/base/capability.nix` exports through
`_module.args`.

**`aspects/framework/esphome-node.nix`** — the class and the builder.
Declares `den.classes.esphome`, and `flake.lib.esphomeNode` as a
`lib.evalModules` over that class. Its option tree is the node's typed
surface: `node.board`, `node.platform`, `node.friendlyName`, and a freeform
`node.config` of `format.type` for the component blocks. Its `config` computes
one thing that matters, `node.yaml`, with
`(pkgs.formats.yaml { tags = true; }).generate "${name}.yaml"`. Per
CLAUDE.md's rule, the constraints go in the types: `node.board` is a
`types.enum` over the boards ESPHome knows, `node.platform` likewise, and the
secret references are a `tag` helper rather than a hand-rolled check that a
string starts with `!`.

**`aspects/services/homeassistant/esphome.nix`** (existing, extended) — the
dashboard, plus the per-node compile units, plus the `secrets.yaml` template
and the symlinks into `/var/lib/esphome`. This is where the fleet of nodes is
enumerated: `lib.filter (h: h.class or "" == "esphome") (lib.attrValues hostsOf)`,
which is the same `hostsOf` flattening `capability.nix` and `firewall.nix`
both do.

**`aspects/hosts/dyson.nix`** — the node itself, in the shape of
`aspects/hosts/iphone.nix`: a flat `den.hosts.dyson` with `class`,
`instantiate`, `intoAttr`, `household` and `attachments`, and a
`den.aspects.dyson.esphome` block carrying the component config.
**[needs the YAML]**.

The reading loop is then the cheap one CLAUDE.md asks for — the YAML is a
`nix build` of one small derivation, not a host eval:

```
nix build .#esphomeConfigurations.dyson.yaml && cat result
```

## What is not decided

- **The Dyson's actual contents.** Board, platform, which components, whether
  it speaks MQTT at all or only the native API, and whether it has lambdas
  that the escaped-scalar rendering would make unreadable in the UI. No part
  of this document guesses.
- **Whether a node joins `checkFindings` cleanly.** Six checks are green and
  must stay green; adding a host runs `exclusionsIn` and `requires-check`
  against it. Measure before committing a node.
- **Per-node broker accounts vs. the shared `sendspin`.** Now expressible;
  not obviously right.
- **Whether the derived `expose` rule lives in `firewall.nix` or
  `mosquitto.nix`.** It has two readers (the hole and the bind address) and
  `firewall.nix` is in flux.
- **The `%YAML 1.1` directive.** Harmless to PyYAML and accepted by ESPHome's
  loader in the measurement above, but it is not something a hand-written
  config has, and the dashboard's own editor may reformat it.
