use async_mach_ports::{Codec, Error, Result as MachResult};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

pub const SERVICE: &str = "in.ivymect.wrapperd";

#[derive(Debug, Serialize, Deserialize)]
pub enum Request {
    WaitReady { label: String },
    Checkin { label: String },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Ack {
    Ready,
}

#[derive(Clone, Copy)]
pub struct Wire;

impl Codec for Wire {
    fn encode<T: Serialize + ?Sized>(&self, value: &T) -> MachResult<Vec<u8>> {
        postcard::to_allocvec(value).map_err(|_| Error::Encode)
    }

    fn decode<T: DeserializeOwned>(&self, bytes: &[u8]) -> MachResult<T> {
        postcard::from_bytes(bytes).map_err(|_| Error::Decode)
    }
}

pub mod plant {
    use serde::Deserialize;
    use std::collections::BTreeMap;
    use std::fs;
    use std::io;
    use std::os::unix::fs::PermissionsExt;
    use std::path::{Path, PathBuf};

    // The manifest Nix generates (via `pkgs.formats.toml`), deserialized with
    // serde. `dir` is the stable wrapper directory the shims exec through
    // (`/run/wrappers/bin`); `wrappers` maps a destination file name to its
    // source store path. Both the signed program and any hidden `.<name>-wrapped`
    // payload are keys here -- the generator enumerates each signed package's
    // `trusted/` once, at build time, so nothing is discovered at runtime.
    //
    // ```toml
    // dir = "/run/wrappers/bin"
    // [wrappers]
    // paneru = "/nix/store/.../trusted/paneru"
    // ".paneru-wrapped" = "/nix/store/.../trusted/.paneru-wrapped"
    // ```
    #[derive(Debug, Deserialize)]
    pub struct Manifest {
        pub dir: PathBuf,
        #[serde(default)]
        pub wrappers: BTreeMap<String, PathBuf>,
    }

    // Everything planted here is a code-signed copy that is the executed image,
    // so 0755 root-owned is the whole story -- no per-entry mode, unlike the
    // setuid stubs `extraModules/darwin/wrappers` owns separately.
    const MODE: u32 = 0o755;

    pub fn parse(text: &str) -> io::Result<Manifest> {
        toml::from_str(text).map_err(|e| {
            io::Error::new(io::ErrorKind::InvalidData, format!("wrapperd: manifest: {e}"))
        })
    }

    // Copy each wrapper to its destination unless the bytes already match, staging
    // through a sibling tempfile and renaming into place so a running executable
    // keeps its own vnode rather than being written under its feet.
    pub fn run(manifest: &Manifest) -> io::Result<u32> {
        fs::create_dir_all(&manifest.dir)?;
        let mut planted = 0;
        for (name, src) in &manifest.wrappers {
            let dst = manifest.dir.join(name);
            let bytes = fs::read(src)
                .map_err(|err| io::Error::new(err.kind(), format!("read {:?}: {err}", src)))?;
            if unchanged(&dst, &bytes) {
                continue;
            }

            let tmp = staged(&manifest.dir, name);
            fs::write(&tmp, &bytes)?;
            fs::set_permissions(&tmp, fs::Permissions::from_mode(MODE))?;
            fs::rename(&tmp, &dst)?;
            planted += 1;
            eprintln!("wrapperd: planted {}", dst.display());
        }
        Ok(planted)
    }

    fn unchanged(dst: &Path, src: &[u8]) -> bool {
        match fs::read(dst) {
            Ok(cur) => cur == src,
            Err(_) => false,
        }
    }

    fn staged(dir: &Path, name: &str) -> PathBuf {
        dir.join(format!(".wrapperd-staged.{name}"))
    }
}

// Hermetic: everything here writes under `$TMPDIR` and cleans up, so it runs
// under `cargoCheckHook` at nix build time (even in a sandbox). The Mach
// transport round-trip needs the bootstrap server and lives in `tests/mach.rs`,
// marked `#[ignore]`.
#[cfg(test)]
mod tests {
    use super::plant;
    use std::fs;
    use std::io::Read;
    use std::os::unix::fs::PermissionsExt;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU32, Ordering};

    static COUNTER: AtomicU32 = AtomicU32::new(0);

    // A fresh, empty scratch dir. Unique per call so tests can run in parallel.
    fn scratch() -> PathBuf {
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("wrapperd-test.{}.{n}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    // Write `sources` (name -> bytes) into `<root>/src` and return a manifest
    // TOML planting them into `<root>/dest`. Exercises `parse` as a side effect.
    fn manifest_for(root: &PathBuf, sources: &[(&str, &[u8])]) -> plant::Manifest {
        let src = root.join("src");
        let dest = root.join("dest");
        fs::create_dir_all(&src).unwrap();
        let mut body = format!("dir = {:?}\n\n[wrappers]\n", dest.to_str().unwrap());
        for (name, bytes) in sources {
            let p = src.join(name);
            fs::write(&p, bytes).unwrap();
            // Keys are TOML-quoted (a leading-dot key like `.foo-wrapped` is not
            // a bare key), mirroring what `pkgs.formats.toml` emits.
            body.push_str(&format!("{:?} = {:?}\n", name, p.to_str().unwrap()));
        }
        plant::parse(&body).unwrap()
    }

    #[test]
    fn parses_bare_and_quoted_keys() {
        let m = plant::parse(
            r#"
              dir = "/run/wrappers/bin"
              [wrappers]
              paneru = "/store/trusted/paneru"
              ".paneru-wrapped" = "/store/trusted/.paneru-wrapped"
            "#,
        )
        .unwrap();
        assert_eq!(m.dir, PathBuf::from("/run/wrappers/bin"));
        assert_eq!(m.wrappers.len(), 2);
        assert_eq!(m.wrappers["paneru"], PathBuf::from("/store/trusted/paneru"));
        assert_eq!(
            m.wrappers[".paneru-wrapped"],
            PathBuf::from("/store/trusted/.paneru-wrapped")
        );
    }

    #[test]
    fn parses_empty_wrapper_set() {
        let m = plant::parse(r#"dir = "/run/wrappers/bin""#).unwrap();
        assert!(m.wrappers.is_empty());
    }

    #[test]
    fn rejects_malformed_manifest() {
        assert!(plant::parse("dir = \n[wrappers]\n").is_err());
    }

    #[test]
    fn plants_content_dir_and_mode() {
        let root = scratch();
        let m = manifest_for(&root, &[("foo", b"the-binary"), (".foo-wrapped", b"payload")]);

        let changed = plant::run(&m).unwrap();
        assert_eq!(changed, 2);

        let dest = root.join("dest");
        assert_eq!(fs::read(dest.join("foo")).unwrap(), b"the-binary");
        assert_eq!(fs::read(dest.join(".foo-wrapped")).unwrap(), b"payload");
        // 0755, and the staging tempfile must not survive.
        for name in ["foo", ".foo-wrapped"] {
            let mode = fs::metadata(dest.join(name)).unwrap().permissions().mode() & 0o777;
            assert_eq!(mode, 0o755, "{name} mode");
        }
        assert!(!dest.join(".wrapperd-staged.foo").exists());
    }

    #[test]
    fn replant_is_idempotent() {
        let root = scratch();
        let m = manifest_for(&root, &[("foo", b"v1")]);
        assert_eq!(plant::run(&m).unwrap(), 1);
        // Nothing changed on disk, so a second run copies nothing.
        assert_eq!(plant::run(&m).unwrap(), 0);
    }

    #[test]
    fn missing_source_is_an_error() {
        let root = scratch();
        let m = manifest_for(&root, &[("foo", b"v1")]);
        fs::remove_file(root.join("src/foo")).unwrap();
        assert!(plant::run(&m).is_err());
    }

    // The reason for staging + rename rather than writing in place: a running
    // executable must keep its own vnode. Replacing the source and replanting
    // must leave a file descriptor already open on the old copy reading the old
    // bytes, while the path now resolves to the new ones.
    #[test]
    fn replant_swaps_the_inode_not_the_bytes_under_a_running_process() {
        let root = scratch();
        let dest = root.join("dest");
        let m = manifest_for(&root, &[("foo", b"old")]);
        assert_eq!(plant::run(&m).unwrap(), 1);

        let mut held = fs::File::open(dest.join("foo")).unwrap();

        fs::write(root.join("src/foo"), b"new-and-longer").unwrap();
        assert_eq!(plant::run(&m).unwrap(), 1);

        let mut from_old_fd = String::new();
        held.read_to_string(&mut from_old_fd).unwrap();
        assert_eq!(from_old_fd, "old", "held fd must still see the old vnode");
        assert_eq!(fs::read(dest.join("foo")).unwrap(), b"new-and-longer");
    }
}
