import getpass
import json
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import runtime as rt  # noqa: E402

USER = getpass.getuser()


def tree(root: Path, files: dict[str, str]) -> Path:
    for rel, text in files.items():
        p = root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text)
    return root


class Delta(unittest.TestCase):
    def setUp(self):
        self.t = Path(tempfile.mkdtemp())

    def run_delta(self, base, variant, scope="etc", **extra):
        b = tree(self.t / "base", base)
        v = tree(self.t / "variant", variant)
        return rt.delta({"name": "x", "scopes": [{"scope": scope, "base": str(b), "variant": str(v)}], **extra})

    def test_new_file_is_an_add(self):
        m = self.run_delta({"a": "1\n"}, {"a": "1\n", "b": "2\n"})
        self.assertEqual([a["rel"] for a in m["add"]], ["b"])
        self.assertEqual(m["shared"], [])
        self.assertEqual(m["conflicts"], [])

    def test_identical_file_is_nothing(self):
        m = self.run_delta({"a": "1\n"}, {"a": "1\n"})
        self.assertEqual((m["add"], m["shared"], m["conflicts"]), ([], [], []))

    def test_appended_lines_are_shared_inserts(self):
        m = self.run_delta({"nix.conf": "a = 1\nb = 2\n"}, {"nix.conf": "a = 1\nx = 9\nb = 2\n"})
        self.assertEqual(m["shared"][0]["inserts"], [(1, ["x = 9\n"])])
        self.assertEqual(m["conflicts"], [])

    def test_changed_line_is_a_conflict(self):
        m = self.run_delta({"f": "a = 1\n"}, {"f": "a = 2\n"})
        self.assertEqual(m["conflicts"][0]["rel"], "f")

    def test_removed_file_is_a_conflict(self):
        m = self.run_delta({"f": "1\n", "g": "2\n"}, {"f": "1\n"})
        self.assertEqual([c["rel"] for c in m["conflicts"]], ["g"])

    def test_regenerated_and_ignored_files_are_skipped(self):
        m = self.run_delta(
            {"share/info/dir": "a", "den-runtime/homes.json": "1"},
            {"share/info/dir": "b", "den-runtime/homes.json": "2", "den-runtime-aspects": "x"},
            ignore=[r"^etc:den-runtime"],
        )
        self.assertEqual((m["add"], m["shared"], m["conflicts"]), ([], [], []))

    def test_symlinked_directories_are_followed(self):
        real = tree(self.t / "real", {"bin/tool": "#!/bin/sh\n"})
        b = self.t / "b"
        b.mkdir()
        v = self.t / "v"
        v.mkdir()
        (v / "pkg").symlink_to(real)
        m = rt.delta({"name": "x", "scopes": [{"scope": "sw", "base": str(b), "variant": str(v)}]})
        self.assertEqual([a["rel"] for a in m["add"]], ["pkg/bin/tool"])

    def test_activation_differences_ignore_store_hashes(self):
        h1, h2 = "a" * 32, "b" * 32
        b = tree(self.t / "ab", {"activate": f"/nix/store/{h1}-etc/x\n"})
        v = tree(self.t / "av", {"activate": f"/nix/store/{h2}-etc/x\necho gaming\n"})
        m = rt.delta(
            {"name": "x", "scopes": [], "activate": [{"scope": "system", "base": str(b / "activate"), "variant": str(v / "activate")}]}
        )
        self.assertEqual(m["activation"][0]["diff"], ["+echo gaming\n"])


class Compose(unittest.TestCase):
    def test_two_aspects_insert_independently_of_order(self):
        base = Path(tempfile.mkdtemp()) / "nix.conf"
        base.write_text("a = 1\nb = 2\n")
        p1 = ("celler", [(1, ["substituters = c\n"])])
        p2 = ("builder", [(2, ["builders = l\n"]), (1, ["max-jobs = 0\n"])])
        one = rt.compose(base, [p1, p2])
        self.assertEqual(one, rt.compose(base, [p2, p1]))
        self.assertEqual(one, "a = 1\nmax-jobs = 0\nsubstituters = c\nb = 2\nbuilders = l\n")

    def test_insert_after_unterminated_last_line(self):
        base = Path(tempfile.mkdtemp()) / "f"
        base.write_text("a")
        self.assertEqual(rt.compose(base, [("x", [(1, ["b\n"])])]), "a\nb\n")


class Reconcile(unittest.TestCase):
    def setUp(self):
        self.t = Path(tempfile.mkdtemp())
        self.root = self.t / "root"
        self.log = self.t / "launchctl.log"
        stub = self.t / "launchctl"
        stub.write_text(f"#!/bin/sh\necho \"$@\" >> {self.log}\n")
        stub.chmod(stub.stat().st_mode | stat.S_IEXEC)
        os.environ["DEN_RUNTIME_ROOT"] = str(self.root)
        os.environ["DEN_RUNTIME_LAUNCHCTL"] = str(stub)
        self.env = rt.Env.get()
        self.store = tree(self.t / "store", {"etc-nix.conf": "a = 1\nb = 2\n", "steam": "#!/bin/sh\n", "agent.plist": "<plist/>\n", "rc": "x\n"})
        etc = self.root / "etc/nix"
        etc.mkdir(parents=True)
        (etc / "nix.conf").symlink_to(self.store / "etc-nix.conf")
        self.home = self.env.home(USER)
        self.home.mkdir(parents=True)

    def manifest(self, name, add=(), shared=()):
        return {"name": name, "add": list(add), "shared": list(shared), "conflicts": [], "activation": []}

    def gaming(self):
        return self.manifest(
            "gaming",
            add=[
                {"scope": "sw", "rel": "bin/steam", "src": str(self.store / "steam")},
                {"scope": "daemons", "rel": "org.nixos.gaming.plist", "src": str(self.store / "agent.plist")},
                {"scope": f"home-files:{USER}", "rel": ".config/steam/rc", "src": str(self.store / "rc")},
            ],
        )

    def celler(self):
        return self.manifest(
            "celler",
            shared=[{"scope": "etc", "rel": "nix/nix.conf", "base": str(self.store / "etc-nix.conf"), "inserts": [[1, ["substituters = c\n"]]]}],
        )

    def builder(self):
        return self.manifest(
            "builder",
            shared=[{"scope": "etc", "rel": "nix/nix.conf", "base": str(self.store / "etc-nix.conf"), "inserts": [[2, ["builders = l\n"]]]}],
        )

    def nixconf(self):
        return (self.root / "etc/nix/nix.conf").read_text()

    def test_up_places_every_kind(self):
        rt.reconcile(self.env, [self.gaming()])
        self.assertEqual(os.readlink(self.env.run / "sw/bin/steam"), str(self.store / "steam"))
        self.assertTrue((self.root / "Library/LaunchDaemons/org.nixos.gaming.plist").is_file())
        self.assertEqual(os.readlink(self.home / ".config/steam/rc"), str(self.store / "rc"))
        self.assertIn("bootstrap system", self.log.read_text())

    def test_mixing_merges_a_shared_file_and_down_unmerges_it(self):
        rt.reconcile(self.env, [self.celler(), self.builder()])
        self.assertEqual(self.nixconf(), "a = 1\nsubstituters = c\nb = 2\nbuilders = l\n")
        rt.reconcile(self.env, [self.builder()])
        self.assertEqual(self.nixconf(), "a = 1\nb = 2\nbuilders = l\n")

    def test_reset_restores_the_base_exactly(self):
        rt.reconcile(self.env, [self.gaming(), self.celler()])
        rt.reconcile(self.env, [])
        self.assertEqual(os.readlink(self.root / "etc/nix/nix.conf"), str(self.store / "etc-nix.conf"))
        self.assertFalse((self.env.run / "sw/bin/steam").exists())
        self.assertFalse((self.root / "Library/LaunchDaemons/org.nixos.gaming.plist").exists())
        self.assertFalse((self.home / ".config/steam/rc").is_symlink())
        self.assertIn("bootout system", self.log.read_text())
        self.assertEqual(json.loads(self.env.ledger.read_text()), {})

    def test_reapplying_the_same_set_changes_nothing(self):
        rt.reconcile(self.env, [self.gaming()])
        self.assertEqual(rt.reconcile(self.env, [self.gaming()]), [])

    def test_two_aspects_adding_one_path_differently_is_refused(self):
        other = self.manifest("other", add=[{"scope": "sw", "rel": "bin/steam", "src": str(self.store / "rc")}])
        with self.assertRaises(rt.Refused):
            rt.reconcile(self.env, [self.gaming(), other])
        self.assertFalse(self.env.ledger.exists())

    def test_foreign_file_is_never_clobbered(self):
        (self.home / ".config/steam").mkdir(parents=True)
        (self.home / ".config/steam/rc").write_text("mine\n")
        with self.assertRaises(rt.Refused):
            rt.reconcile(self.env, [self.gaming()])
        self.assertEqual((self.home / ".config/steam/rc").read_text(), "mine\n")
        self.assertFalse((self.env.run / "sw/bin/steam").exists())

    def test_bundle_with_conflicts_is_refused(self):
        bad = self.manifest("bad")
        bad["conflicts"] = [{"scope": "etc", "rel": "x", "reason": "r"}]
        with self.assertRaises(rt.Refused):
            rt.reconcile(self.env, [bad])


if __name__ == "__main__":
    unittest.main()
