use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::io::{Cursor, Read, Write, stdin};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::{fmt, io};

use age_core::format::{FILE_KEY_BYTES, FileKey, Stanza};
use age_plugin::identity::Error as IdentityError;
use age_plugin::identity::IdentityPluginV1;
use age_plugin::recipient::Error as RecipientError;
use age_plugin::recipient::RecipientPluginV1;
use age_plugin::{PluginHandler, print_new_identity, run_state_machine};
use bech32::Variant;
use clap::{CommandFactory, Parser, ValueEnum};
use futures_util::StreamExt as _;
use rand::rngs::OsRng;
use rand::thread_rng;
use sequoia_gpg_agent::assuan::Response;
use sequoia_gpg_agent::{Agent, KeyPair};
use sequoia_ipc::Keygrip;
use sequoia_ipc::sexp::{Sexp, String_};
use sequoia_openpgp::crypto::SessionKey;
use sequoia_openpgp::crypto::mpi::{Ciphertext, PublicKey};
use sequoia_openpgp::packet::Key;
use sequoia_openpgp::packet::key::{Key6, KeyParts, KeyRole, PublicParts, UnspecifiedRole};
use sequoia_openpgp::parse::Parse;
use sequoia_openpgp::serialize::MarshalInto as _;
use sequoia_openpgp::types::{
	Curve, HashAlgorithm, PublicKeyAlgorithm, SymmetricAlgorithm, Timestamp,
};
use std::io::Result as IoResult;

use tokio::runtime::{Handle, Runtime};
use tracing::info;

use std::os::unix::ffi::OsStringExt;

use anyhow::{Context, Result, anyhow, bail, ensure};

use secrecy::ExposeSecret as _;

use self::stanza::{GPG_STANZA_TAG, GpgStanza};

mod stanza;

#[derive(clap::Subcommand)]
enum Command {
	/// Export gpg keygrip in format suitable for this plugin
	ExportKeygrip {
		// GPG keygrip for encryption key, can be seen with `gpg --list-keys --with-keygrip`
		keygrip: String,
	},
}

#[derive(Parser)]
struct Opts {
	#[clap(long)]
	age_plugin: Option<String>,
	#[clap(subcommand)]
	cmd: Option<Command>,
}

fn key(s: &Sexp) -> &[u8] {
	if let Sexp::List(alist) = s {
		if let Some(Sexp::String(key)) = alist.get(0) {
			return key;
		}
	}
	panic!("malformed alist");
}
fn value(s: &Sexp) -> &[Sexp] {
	if let Sexp::List(alist) = s {
		if let Some(Sexp::String(_key)) = alist.get(0) {
			return &alist[1..];
		}
	}

	panic!("malformed alist");
}
fn get<'e>(s: &'e Sexp, k: &[u8]) -> Option<&'e [Sexp]> {
	if key(s) == k { Some(value(s)) } else { None }
}

fn find_string(v: &[Sexp], k: &[u8]) -> Option<String_> {
	v.iter().find_map(|p| {
		get(p, k)
			.unwrap_or_default()
			.get(0)
			.and_then(Sexp::string)
			.cloned()
	})
}

fn parse_public_key(e: Sexp) -> Result<Key<PublicParts, UnspecifiedRole>> {
	let e = get(&e, b"public-key")
		.expect("todo: not a public key")
		.into_iter()
		.next()
		.expect("todo: not a public key");

	if let Some(e) = get(e, b"rsa") {
		let n = find_string(e, b"n").expect("todo: not a public key");
		let e = find_string(e, b"e").expect("todo: not a public key");
		Ok(Key::V6(Key6::import_public_rsa(&e, &n, None)?))
	} else if let Some(e) = get(e, b"ecc") {
		let curve = find_string(e, b"curve").expect("todo: not a public key");
		let q = find_string(e, b"q").expect("todo: not a public key");
		ensure!(&*curve == b"Curve25519", "unsupported curve: {:?}", curve);

		let hash = HashAlgorithm::SHA256;
		let sym = SymmetricAlgorithm::AES128;

		// There might be a

		Ok(Key::V6(Key6::new(
			// TS should match between encryption and decryption
			Timestamp::from(0u32),
			sequoia_openpgp::types::PublicKeyAlgorithm::ECDH,
			PublicKey::ECDH {
				curve: Curve::Cv25519,
				q: q.to_vec().into(),
				// TODO: Get that data from gpg somehow?..
				hash,
				sym,
			},
		)?))
	} else {
		todo!()
	}
}

fn gpg_to_age(gpg: SessionKey) -> FileKey {
	FileKey::init_with_mut(|data| data.copy_from_slice(&*gpg))
}

struct IdentityV1 {
	agent: Agent,
}
impl IdentityPluginV1 for IdentityV1 {
	fn add_identity(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> Result<(), IdentityError> {
		if plugin_name != "gpg" {
			return Ok(());
		}
		if bytes.len() != 0 {
			return Err(IdentityError::Identity {
				index,
				message: "only default identity supported for gpg plugin".to_owned(),
			});
		}
		Ok(())
	}

	fn unwrap_file_keys(
		&mut self,
		files: Vec<Vec<Stanza>>,
		_callbacks: impl age_plugin::Callbacks<IdentityError>,
	) -> IoResult<HashMap<usize, Result<FileKey, Vec<IdentityError>>>> {
		let mut out = HashMap::new();
		for (file_index, file) in files.into_iter().enumerate() {
			let mut stanza_errs = vec![];
			let mut stanza_ok = None::<FileKey>;
			for (stanza_index, stanza) in file.into_iter().enumerate() {
				if stanza.tag != GPG_STANZA_TAG {
					continue;
				}

				let mut add_err = |message| {
					stanza_errs.push(IdentityError::Stanza {
						file_index,
						stanza_index,
						message,
					});
				};

				let stanza = match GpgStanza::try_from(stanza) {
					Ok(v) => v,
					Err(e) => {
						add_err(e.to_string());
						continue;
					}
				};

				let mut keypair = match Handle::current()
					.block_on(fetch_keypair(&mut self.agent, stanza.keygrip))
				{
					Ok(k) => k,
					Err(e) => {
						add_err(format!("failed to get keypair handle: {e}"));
						continue;
					}
				};

				let session_key = match Handle::current()
					.block_on(keypair.decrypt_async(&stanza.ciphertext, Some(FILE_KEY_BYTES)))
				{
					Ok(sk) => sk,
					Err(e) => {
						add_err(e.to_string());
						continue;
					}
				};

				stanza_ok = Some(gpg_to_age(session_key));
				break;
			}
			if let Some(ok) = stanza_ok {
				out.insert(file_index, Ok(ok));
			} else if stanza_errs.len() > 0 {
				out.insert(file_index, Err(stanza_errs));
			}
		}
		Ok(out)
	}
}

struct RecipientV1 {
	agent: Agent,
	keys: HashMap<usize, Key<PublicParts, UnspecifiedRole>>,
}
impl RecipientPluginV1 for RecipientV1 {
	fn add_recipient(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> std::result::Result<(), age_plugin::recipient::Error> {
		if plugin_name != "gpg" {
			return Ok(());
		}
		let keygripstring = hex::encode(bytes);
		let keygrip: Keygrip = keygripstring
			.parse()
			.map_err(|e| RecipientError::Recipient {
				index,
				message: format!("invalid keygrip: {e}"),
			})?;

		let v = Handle::current()
			.block_on(fetch_key(&mut self.agent, keygrip))
			.map_err(|e| RecipientError::Recipient {
				index,
				message: format!("failed to fetch recipient key by keygrip: {e}"),
			})?;

		self.keys.insert(index, v);
		Ok(())
	}

	fn add_identity(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> std::result::Result<(), age_plugin::recipient::Error> {
		todo!()
	}

	fn labels(&mut self) -> std::collections::HashSet<String> {
		HashSet::new()
	}

	fn wrap_file_keys(
		&mut self,
		file_keys: Vec<FileKey>,
		_callbacks: impl age_plugin::Callbacks<RecipientError>,
	) -> std::io::Result<std::result::Result<Vec<Vec<Stanza>>, Vec<RecipientError>>> {
		let mut out = vec![];
		let mut errs = vec![];
		for file_key in file_keys {
			let mut stanzas = vec![];
			let session_key = SessionKey::from(file_key.expose_secret().as_slice());
			for (index, recipient) in &self.keys {
				let mut add_err = |message| {
					errs.push(RecipientError::Recipient {
						index: *index,
						message,
					});
				};
				let ciphertext = match recipient.encrypt(&session_key) {
					Ok(c) => c,
					Err(e) => {
						add_err(e.to_string());
						continue;
					}
				};
				let keygrip = Keygrip::of(recipient.mpis()).expect("can get keygrip back");
				let stanza = GpgStanza {
					keygrip,
					ciphertext,
				};
				stanzas.push(stanza.into());
			}
			out.push(stanzas);
		}
		if !errs.is_empty() {
			return Ok(Err(errs));
		}
		Ok(Ok(out))
	}
}

struct Plugin {
	agent: Agent,
}
impl PluginHandler for Plugin {
	type RecipientV1 = RecipientV1;

	type IdentityV1 = IdentityV1;

	fn identity_v1(self) -> std::io::Result<Self::IdentityV1> {
		Ok(IdentityV1 { agent: self.agent })
	}
	fn recipient_v1(self) -> std::io::Result<Self::RecipientV1> {
		Ok(RecipientV1 {
			agent: self.agent,
			keys: HashMap::new(),
		})
	}
}

async fn fetch_key(
	agent: &mut Agent,
	keygrip: Keygrip,
) -> Result<Key<PublicParts, UnspecifiedRole>> {
	let key = agent.send_simple(format!("READKEY {keygrip}")).await?;

	let key = sequoia_ipc::sexp::Sexp::from_bytes(&key)
		.map_err(|e| anyhow!("failed to parse key sexp: {e}"))?;
	let key = parse_public_key(key)?;
	Ok(key)
}
async fn fetch_keypair(agent: &mut Agent, keygrip: Keygrip) -> Result<KeyPair> {
	let key = fetch_key(agent, keygrip).await?;

	if !agent.has_key(&key).await? {
		bail!("no secret for key")
	}

	let pair = agent.keypair(&key)?;
	Ok(pair)
}

fn main() -> Result<()> {
	tracing_subscriber::fmt().with_writer(io::stderr).init();

	let runtime = Runtime::new()?;
	let _runtime = runtime.enter();

	let mut agent = Handle::current()
		.block_on(Agent::connect_to_default())
		.context("gpg agent connection failed")?;

	// print_new_identity("gpg", &keygripbytes, &keygripbytes);

	// let encrypted = key.encrypt(&SessionKey::from("Hello, world!!!!".as_bytes().to_vec()))?;
	//
	// ensure!(agent.has_key(&key).await.context("has key check")?);
	// let mut keypair = agent.keypair(&key)?;
	//
	// let decrypted = keypair.decrypt_async(&encrypted, None).await?;

	// dbg!(String::from_utf8_lossy(decrypted.as_bytes()));
	// dbg!(kek.as_bytes());

	let opts = Opts::parse();

	if let Some(age_plugin) = opts.age_plugin {
		run_state_machine(&age_plugin, Plugin { agent })?;
	} else if let Some(cmd) = opts.cmd {
		match cmd {
			Command::ExportKeygrip { keygrip } => {
				use bech32::ToBase32;
				let keygripid: Keygrip = keygrip.parse()?;
				let keygripbytes = hex::decode(keygripid.to_string()).expect("keygrip is hex");

				Handle::current().block_on(fetch_key(&mut agent, keygripid))?;

				println!(
					"{}",
					bech32::encode("age1gpg", keygripbytes.to_base32(), Variant::Bech32)
						.expect("hrp is valid")
				);
			}
		}
	} else {
		let _ = Opts::command().print_help();
		bail!("no arguments provided")
	};

	Ok(())
}
