use std::collections::{HashMap, HashSet};
use std::io;

use age_core::format::{FILE_KEY_BYTES, FileKey, Stanza};
use age_plugin::identity::Error as IdentityError;
use age_plugin::identity::IdentityPluginV1;
use age_plugin::recipient::Error as RecipientError;
use age_plugin::recipient::RecipientPluginV1;
use age_plugin::{PluginHandler, run_state_machine};
use bech32::Variant;
use clap::{CommandFactory, Parser};
use dialoguer::{Select, theme::ColorfulTheme};
use sequoia_gpg_agent::{Agent, KeyPair};
use sequoia_ipc::Keygrip;
use sequoia_ipc::sexp::{Sexp, String_};
use sequoia_openpgp::crypto::SessionKey;
use sequoia_openpgp::crypto::mpi::PublicKey;
use sequoia_openpgp::packet::Key;
use sequoia_openpgp::packet::key::{Key6, PublicParts, UnspecifiedRole};
use sequoia_openpgp::types::{Curve, HashAlgorithm, SymmetricAlgorithm, Timestamp};
use std::io::Result as IoResult;

use tokio::runtime::{Handle, Runtime};

use anyhow::{Context, Result, anyhow, bail, ensure};

use secrecy::ExposeSecret as _;

use self::stanza::{GPG_STANZA_TAG, GpgStanza};

mod stanza;

#[derive(clap::Subcommand)]
enum Command {
	/// Export gpg keygrip in format suitable for this plugin
	ExportKeygrip {
		/// GPG keygrip for encryption key, can be seen with `gpg --list-keys --with-keygrip`
		keygrip: String,
	},
	/// Generate age identity file
	Generate,
}

#[derive(Parser)]
struct Opts {
	#[clap(long)]
	age_plugin: Option<String>,
	#[clap(subcommand)]
	cmd: Option<Command>,
}

fn key(s: &Sexp) -> &[u8] {
	if let Sexp::List(alist) = s
		&& let Some(Sexp::String(key)) = alist.first()
	{
		return key;
	}
	panic!("malformed alist");
}
fn value(s: &Sexp) -> &[Sexp] {
	if let Sexp::List(alist) = s
		&& let Some(Sexp::String(_key)) = alist.first()
	{
		return &alist[1..];
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
			.first()
			.and_then(Sexp::string)
			.cloned()
	})
}

fn parse_public_key(e: Sexp) -> Result<Key<PublicParts, UnspecifiedRole>> {
	let e = get(&e, b"public-key")
		.expect("todo: not a public key")
		.iter()
		.next()
		.expect("todo: not a public key");

	if let Some(e) = get(e, b"rsa") {
		let n = find_string(e, b"n").expect("todo: not a public key");
		let e = find_string(e, b"e").expect("todo: not a public key");
		Ok(Key::V6(Key6::import_public_rsa(&e, &n, None)?))
	} else if let Some(e) = get(e, b"ecc") {
		let curve = find_string(e, b"curve").expect("todo: not a public key");
		let q = find_string(e, b"q").expect("todo: not a public key");

		if let Some(flags) = find_string(e, b"flags")
			&& &*flags != b"djb-tweak"
		{
			bail!("unsupported flags: {flags:?}");
		}
		ensure!(&*curve == b"Curve25519", "unsupported curve: {curve:?}");

		// Those parameters should depend on curve: https://datatracker.ietf.org/doc/html/rfc6637
		// Those are for Cv25519
		let hash = HashAlgorithm::SHA256;
		let sym = SymmetricAlgorithm::AES128;

		// I'm not sure when this might happen
		if let Some(kdfp) = find_string(e, b"kdf-params") {
			bail!(
				"custom kdf params not supported: {kdfp:?}. Please report how did you get that error, because I'm not sure how it should be possible to customize kdf params in gpg"
			)
		}

		Ok(Key::V6(Key6::new(
			// TS should match between encryption and decryption.
			Timestamp::from(0u32),
			sequoia_openpgp::types::PublicKeyAlgorithm::ECDH,
			PublicKey::ECDH {
				curve: Curve::Cv25519,
				q: q.to_vec().into(),
				hash,
				sym,
			},
		)?))
	} else {
		todo!()
	}
}

fn gpg_to_age(gpg: SessionKey) -> FileKey {
	FileKey::init_with_mut(|data| data.copy_from_slice(&gpg))
}

struct IdentityV1 {
	agent: Agent,
	identities: Vec<Keygrip>,
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
		if bytes.is_empty() {
			return Ok(());
		}
		let keygripstring = hex::encode(bytes);
		let keygrip: Keygrip = keygripstring.parse().map_err(|e| IdentityError::Identity {
			index,
			message: format!("invalid keygrip: {e}"),
		})?;
		self.identities.push(keygrip);
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

				if !self.identities.is_empty() && !self.identities.contains(&stanza.keygrip) {
					continue;
				}

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
			} else if !stanza_errs.is_empty() {
				out.insert(file_index, Err(stanza_errs));
			}
		}
		Ok(out)
	}
}

struct RecipientV1 {
	agent: Agent,
	recipients: HashMap<usize, Key<PublicParts, UnspecifiedRole>>,
	identities: HashMap<usize, Key<PublicParts, UnspecifiedRole>>,
}
impl RecipientPluginV1 for RecipientV1 {
	fn add_recipient(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> std::result::Result<(), RecipientError> {
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

		self.recipients.insert(index, v);
		Ok(())
	}

	fn add_identity(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> std::result::Result<(), RecipientError> {
		if plugin_name != "gpg" {
			return Ok(());
		}
		let keygripstring = hex::encode(bytes);
		let keygrip: Keygrip = keygripstring
			.parse()
			.map_err(|e| RecipientError::Identity {
				index,
				message: format!("invalid keygrip: {e}"),
			})?;

		let v = Handle::current()
			.block_on(fetch_key(&mut self.agent, keygrip))
			.map_err(|e| RecipientError::Identity {
				index,
				message: format!("failed to fetch recipient key by keygrip: {e}"),
			})?;

		self.identities.insert(index, v);
		Ok(())
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

			for (index, recipient) in &self.recipients {
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

			for (index, identity) in &self.identities {
				let mut add_err = |message| {
					errs.push(RecipientError::Identity {
						index: *index,
						message,
					});
				};
				let ciphertext = match identity.encrypt(&session_key) {
					Ok(c) => c,
					Err(e) => {
						add_err(e.to_string());
						continue;
					}
				};
				let keygrip = Keygrip::of(identity.mpis()).expect("can get keygrip back");
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
		Ok(IdentityV1 {
			agent: self.agent,
			identities: vec![],
		})
	}
	fn recipient_v1(self) -> std::io::Result<Self::RecipientV1> {
		Ok(RecipientV1 {
			agent: self.agent,
			recipients: HashMap::new(),
			identities: HashMap::new(),
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

fn generate_identity(agent: &mut Agent) -> Result<()> {
	let response = Handle::current()
		.block_on(agent.send_simple("KEYINFO --data --list"))
		.context("Failed to list keys from agent")?;
	// gpg-agent returns percent-encoded data for this command
	let response = String::from_utf8(response.to_vec())
		.context("Agent output is not UTF-8")?
		.replace("%0A", "\n");

	struct OptionItem {
		label: String,
		keygrip: String,
	}
	let mut options = Vec::new();

	for line in response.lines() {
		let parts: Vec<&str> = line.split_whitespace().collect();
		if parts.is_empty() {
			continue;
		}

		let keygrip_str = parts[0];
		let keygrip: Keygrip = match keygrip_str.parse() {
			Ok(k) => k,
			Err(_) => continue,
		};

		let type_str = parts.get(1).copied().unwrap_or("-");
		let is_smartcard = type_str == "T";

		let key_info = match Handle::current().block_on(fetch_key(agent, keygrip.clone())) {
			Ok(k) => k,
			Err(_) => continue,
		};

		let algo_str = match key_info {
			Key::V6(k) => match k.mpis() {
				PublicKey::RSA { .. } => "RSA".to_string(),
				PublicKey::ECDH { curve, .. } => format!("{:?}", curve),
				PublicKey::EdDSA { curve, .. } => format!("{:?}", curve),
				_ => "Unknown".to_string(),
			},
			_ => "Unknown".to_string(),
		};

		let stub_mark = if is_smartcard { " (Card)" } else { "" };
		let serial = if is_smartcard {
			parts.get(2).copied().unwrap_or("")
		} else {
			""
		};

		let serial_info = if !serial.is_empty() && serial != "-" {
			format!(" S/N: {}", serial)
		} else {
			String::new()
		};

		options.push(OptionItem {
			label: format!("{} - {}{}{}", algo_str, keygrip_str, stub_mark, serial_info),
			keygrip: keygrip_str.to_string(),
		});
	}
	if options.is_empty() {
		bail!("No keys found in GPG agent directory");
	}

	let labels: Vec<&String> = options.iter().map(|o| &o.label).collect();
	let selection = Select::with_theme(&ColorfulTheme::default())
		.with_prompt("Select a key")
		.default(0)
		.items(&labels)
		.interact()
		.context("Failed to read selection")?;

	let selected = &options[selection];
	let keygripbytes = hex::decode(&selected.keygrip).expect("hex valid");

	use bech32::ToBase32;
	let identity = bech32::encode(
		"AGE-PLUGIN-GPG-",
		keygripbytes.to_base32(),
		Variant::Bech32,
	)
	.unwrap()
	.to_uppercase();
	let recipient = bech32::encode("age1gpg", keygripbytes.to_base32(), Variant::Bech32).unwrap();

	println!("# recipient: {}", recipient);
	println!("{}", identity);

	Ok(())
}

fn main() -> Result<()> {
	tracing_subscriber::fmt().with_writer(io::stderr).init();
	let opts = Opts::parse();

	let runtime = Runtime::new()?;
	let _runtime = runtime.enter();

	let mut agent = Handle::current()
		.block_on(Agent::connect_to_default())
		.context("gpg agent connection failed")?;

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
			Command::Generate => generate_identity(&mut agent)?,
		}
	} else {
		let _ = Opts::command().print_help();
		bail!("no arguments provided")
	};

	Ok(())
}
