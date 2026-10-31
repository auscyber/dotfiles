"""Runtime aspects: diff a variant system against the base, then apply deltas."""

from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import os
import pwd
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import NewType

Rel = NewType("Rel", str)
Scope = NewType("Scope", str)

# Files a profile/buildEnv regenerates whenever its contents change.
REGENERATED = [
    re.compile(p)
    for p in (
        r"(^|/)manifest\.(nix|json)$",
        r"^share/info/dir$",
        r"^share/icons/[^/]+/icon-theme\.cache$",
        r"^share/mime/",
        r"^share/glib-2\.0/schemas/gschemas\.compiled$",
        r"^share/applications/mimeinfo\.cache$",
    )
]
STORE = re.compile(r"/nix/store/[a-z0-9]{32}-")


def regenerated(rel: str) -> bool:
    return any(r.search(rel) for r in REGENERATED)


def walk(root: Path) -> dict[Rel, Path]:
    """Every file under root, following symlinks, keyed by relative path."""
    out: dict[Rel, Path] = {}
    if not root.exists():
        return out
    stack: list[tuple[Path, str, frozenset[str]]] = [(root, "", frozenset())]
    while stack:
        p, rel, seen = stack.pop()
        real = Path(os.path.realpath(p))
        if real.is_dir():
            if str(real) in seen:
                continue
            for child in sorted(real.iterdir(), key=lambda c: c.name):
                stack.append((child, f"{rel}/{child.name}" if rel else child.name, seen | {str(real)}))
        elif real.exists():
            out[Rel(rel)] = real
    return out


def digest(p: Path) -> str:
    return hashlib.sha256(p.read_bytes()).hexdigest()


def same(a: Path, b: Path) -> bool:
    return a == b or digest(a) == digest(b)


def lines(p: Path) -> list[str] | None:
    try:
        return p.read_text().splitlines(keepends=True)
    except UnicodeDecodeError:
        return None


Inserts = list[tuple[int, list[str]]]


def inserts(base: Path, variant: Path) -> Inserts | None:
    """The variant as pure line insertions into the base, or None."""
    a, b = lines(base), lines(variant)
    if a is None or b is None:
        return None
    out: Inserts = []
    for tag, i1, _i2, j1, j2 in difflib.SequenceMatcher(a=a, b=b, autojunk=False).get_opcodes():
        if tag == "equal":
            continue
        if tag != "insert":
            return None
        out.append((i1, b[j1:j2]))
    return out


def compose(base: Path, parts: list[tuple[str, Inserts]]) -> str:
    """Base plus every part's insertions, ordered by base position then part name."""
    a = lines(base) or []
    at: dict[int, list[str]] = {}
    for _name, ins in sorted(parts, key=lambda p: p[0]):
        for i, ls in ins:
            at.setdefault(i, []).extend(ls)
    out: list[str] = []
    for i in range(len(a) + 1):
        if i in at:
            if out and not out[-1].endswith("\n"):
                out[-1] += "\n"
            out.extend(at[i])
        if i < len(a):
            out.append(a[i])
    return "".join(out)


def normalised(p: Path) -> list[str]:
    return [STORE.sub("/nix/store/HASH-", l) for l in (lines(p) or [])]


def delta(spec: dict) -> dict:
    m: dict = {"name": spec["name"], "add": [], "shared": [], "conflicts": [], "activation": []}
    ignore = [re.compile(p) for p in spec.get("ignore", [])]
    for s in spec["scopes"]:
        scope = Scope(s["scope"])
        b, v = walk(Path(s["base"])), walk(Path(s["variant"]))
        for rel, src in sorted(v.items()):
            if regenerated(rel) or any(r.search(f"{scope}:{rel}") for r in ignore):
                continue
            if rel not in b:
                m["add"].append({"scope": scope, "rel": rel, "src": str(src)})
            elif not same(b[rel], src):
                ins = inserts(b[rel], src)
                if ins is None:
                    m["conflicts"].append({"scope": scope, "rel": rel, "reason": "changes or removes base lines"})
                else:
                    m["shared"].append({"scope": scope, "rel": rel, "base": str(b[rel]), "inserts": ins})
        for rel in sorted(b.keys() - v.keys()):
            if not regenerated(rel) and not any(r.search(f"{scope}:{rel}") for r in ignore):
                m["conflicts"].append({"scope": scope, "rel": rel, "reason": "removes a base file"})
    for a in spec.get("activate", []):
        before, after = normalised(Path(a["base"])), normalised(Path(a["variant"]))
        added = [l for l in difflib.unified_diff(before, after, n=0) if l.startswith(("+", "-")) and not l.startswith(("+++", "---"))]
        if added:
            m["activation"].append({"scope": a["scope"], "diff": added})
    return m


# ── reconcile ──


@dataclass(frozen=True)
class Env:
    root: Path
    launchctl: str

    @staticmethod
    def get() -> "Env":
        return Env(Path(os.environ.get("DEN_RUNTIME_ROOT", "/")), os.environ.get("DEN_RUNTIME_LAUNCHCTL", "launchctl"))

    def at(self, p: str) -> Path:
        return self.root / p.lstrip("/")

    @property
    def run(self) -> Path:
        return self.at("/run/den-runtime")

    @property
    def ledger(self) -> Path:
        return self.at("/var/lib/den-runtime/ledger.json")

    def home(self, user: str) -> Path:
        return self.at(pwd.getpwnam(user).pw_dir)


@dataclass(frozen=True)
class Want:
    path: str
    kind: str  # link | copy | composed
    src: str  # store path, or composed text
    owner: str  # aspect name(s)
    launchd: str | None  # system | gui/<uid> | None


def placement(env: Env, scope: str, rel: str) -> tuple[Path, str, str | None]:
    """Where a delta entry lands, how, and which launchd domain loads it."""
    if scope == "etc":
        return env.at("/etc") / rel, "link", None
    if scope == "daemons":
        return env.at("/Library/LaunchDaemons") / rel, "copy", "system"
    if scope == "agents":
        return env.at("/Library/LaunchAgents") / rel, "copy", None
    if scope == "sw":
        return env.run / "sw" / rel, "link", None
    kind, _, user = scope.partition(":")
    if kind == "home-files":
        uid = pwd.getpwnam(user).pw_uid
        domain = f"gui/{uid}" if rel.startswith("Library/LaunchAgents/") and rel.endswith(".plist") else None
        return env.home(user) / rel, "link", domain
    if kind == "home-path":
        return env.home(user) / ".local/state/den-runtime/home-path" / rel, "link", None
    raise SystemExit(f"aspect: unknown scope {scope}")


class Refused(Exception):
    pass


def wanted(env: Env, manifests: list[dict]) -> dict[str, Want]:
    want: dict[str, Want] = {}
    shared: dict[str, tuple[str, list[tuple[str, Inserts]], str | None]] = {}
    for m in manifests:
        if m["conflicts"]:
            raise Refused(f"{m['name']}: bundle has conflicts, see `aspect check {m['name']}`")
        for a in m["add"]:
            path, kind, domain = placement(env, a["scope"], a["rel"])
            prior = want.get(str(path))
            if prior and prior.src != a["src"]:
                raise Refused(f"{path}: both {prior.owner} and {m['name']} provide it")
            want[str(path)] = Want(str(path), kind, a["src"], m["name"], domain)
        for s in m["shared"]:
            path, _kind, domain = placement(env, s["scope"], s["rel"])
            base, parts, _ = shared.get(str(path), (s["base"], [], domain))
            if base != s["base"]:
                raise Refused(f"{path}: bundles were built against different bases; `aspect build` again")
            parts.append((m["name"], [(i, ls) for i, ls in s["inserts"]]))
            shared[str(path)] = (base, parts, domain)
    for path, (base, parts, domain) in shared.items():
        if path in want:
            raise Refused(f"{path}: added by {want[path].owner} and extended by {parts[0][0]}")
        owners = ",".join(sorted(n for n, _ in parts))
        want[path] = Want(path, "composed", compose(Path(base), parts), owners, domain)
    return want


def load_ledger(env: Env) -> dict[str, dict]:
    try:
        return json.loads(env.ledger.read_text())
    except FileNotFoundError:
        return {}


def launchd(env: Env, verb: str, domain: str, plist: Path) -> None:
    args = [env.launchctl, verb, domain, str(plist)]
    if subprocess.run(args, check=False).returncode != 0:
        print(f"aspect: `{' '.join(args)}` failed", file=sys.stderr)


def undo(env: Env, path: str, entry: dict) -> None:
    p = Path(path)
    if entry.get("launchd"):
        launchd(env, "bootout", entry["launchd"], p)
    if p.is_symlink() or p.exists():
        p.unlink()
    if entry.get("restore"):
        p.symlink_to(entry["restore"])


def place(env: Env, w: Want, prior_link: str | None) -> dict:
    p = Path(w.path)
    p.parent.mkdir(parents=True, exist_ok=True)
    if w.kind == "composed":
        store = env.run / "composed" / hashlib.sha256(w.path.encode()).hexdigest()[:16]
        store.parent.mkdir(parents=True, exist_ok=True)
        store.write_text(w.src)
        if p.is_symlink() or p.exists():
            p.unlink()
        p.symlink_to(store)
    elif w.kind == "copy":
        shutil.copyfile(w.src, p)
        p.chmod(0o644)
    else:
        p.symlink_to(w.src)
    if w.launchd:
        launchd(env, "bootstrap", w.launchd, p)
    return {"kind": w.kind, "src": w.src, "owner": w.owner, "launchd": w.launchd, "restore": prior_link}


def reconcile(env: Env, manifests: list[dict]) -> list[str]:
    """Make the filesystem match exactly these deltas; returns what changed."""
    want = wanted(env, manifests)
    ledger = load_ledger(env)
    keep = {p for p, e in ledger.items() if p in want and e["src"] == want[p].src and e["kind"] == want[p].kind}
    for path, w in want.items():
        if path in keep or path in ledger:
            continue
        p = Path(path)
        if w.kind != "composed" and (p.is_symlink() or p.exists()):
            raise Refused(f"{path}: exists and is not from a runtime aspect")
        if w.kind == "composed" and not p.is_symlink():
            raise Refused(f"{path}: expected the base's symlink to extend")
    changed: list[str] = []
    for path in sorted(set(ledger) - keep):
        undo(env, path, ledger[path])
        changed.append(f"- {path}")
    restored = {p: ledger[p].get("restore") for p in ledger if p not in keep}
    new = {p: ledger[p] for p in keep}
    for path, w in sorted(want.items()):
        if path in keep:
            continue
        p = Path(path)
        prior = restored.get(path) or (os.readlink(p) if w.kind == "composed" and p.is_symlink() else None)
        new[path] = place(env, w, prior)
        changed.append(f"+ {path}")
    env.ledger.parent.mkdir(parents=True, exist_ok=True)
    tmp = env.ledger.with_suffix(".tmp")
    tmp.write_text(json.dumps(new, indent=1, sort_keys=True))
    tmp.replace(env.ledger)
    return changed


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(prog="den-runtime")
    sub = ap.add_subparsers(dest="cmd", required=True)
    d = sub.add_parser("delta")
    d.add_argument("spec", type=Path)
    d.add_argument("--out", type=Path, required=True)
    r = sub.add_parser("reconcile")
    r.add_argument("manifests", type=Path, nargs="*")
    a = ap.parse_args(argv)
    if a.cmd == "delta":
        m = delta(json.loads(a.spec.read_text()))
        a.out.mkdir(parents=True, exist_ok=True)
        (a.out / "manifest.json").write_text(json.dumps(m, indent=1))
        for c in m["conflicts"]:
            print(f"conflict: {c['scope']}:{c['rel']}: {c['reason']}", file=sys.stderr)
        for act in m["activation"]:
            print(f"note: {act['scope']} activation differs beyond files:", file=sys.stderr)
            for l in act["diff"][:20]:
                print(f"  {l.rstrip()}", file=sys.stderr)
        return 1 if m["conflicts"] else 0
    try:
        changed = reconcile(Env.get(), [json.loads(p.read_text()) for p in a.manifests])
    except Refused as e:
        print(f"aspect: refused, nothing changed: {e}", file=sys.stderr)
        return 2
    print("\n".join(changed) or "aspect: nothing to change")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
