use std::ffi::OsString;
use std::fmt;
use std::io::{Read, Write, stdin};
use std::os::unix::net::UnixStream;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use aes_kw::{KekAes128, KekAes256};
use age_plugin::identity::IdentityPluginV1;
use age_plugin::recipient::RecipientPluginV1;
use age_plugin::{PluginHandler, run_state_machine};
use clap::{Parser, ValueEnum};
use futures_util::StreamExt as _;
use nom::bytes::{tag, take, take_while, take_while1};
use nom::combinator::{cond, map, map_res};
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, terminated};
use nom::{AsBytes, IResult, Parser as _};
use rand::rngs::OsRng;
use rand::thread_rng;
use sequoia_gpg_agent::Agent;
use sequoia_gpg_agent::assuan::Response;
use sequoia_openpgp::crypto::SessionKey;
use sequoia_openpgp::crypto::mpi::PublicKey;
use sequoia_openpgp::packet::Key;
use sequoia_openpgp::packet::key::{Key6, KeyParts, KeyRole, PublicParts, UnspecifiedRole};
use sequoia_openpgp::parse::Parse;
use sequoia_openpgp::types::{Curve, HashAlgorithm, SymmetricAlgorithm, Timestamp};

use std::os::unix::ffi::OsStringExt;

use anyhow::{Context, Result, anyhow, bail, ensure};

#[derive(ValueEnum, Clone, Copy)]
enum StateMachine {
	RecipientV1,
	IdentitiyV1,
}
impl StateMachine {
	fn as_str(&self) -> &'static str {
		match self {
			StateMachine::RecipientV1 => "recipient-v1",
			StateMachine::IdentitiyV1 => "identity-v1",
		}
	}
}

#[derive(Parser)]
struct Opts {
	#[clap(long)]
	age_plugin: StateMachine,
}

struct RecipientV1;
impl RecipientPluginV1 for RecipientV1 {
	fn add_recipient(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> Result<(), age_plugin::recipient::Error> {
		todo!()
	}

	fn add_identity(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> Result<(), age_plugin::recipient::Error> {
		todo!()
	}

	fn labels(&mut self) -> std::collections::HashSet<String> {
		todo!()
	}

	fn wrap_file_keys(
		&mut self,
		file_keys: Vec<age_core::format::FileKey>,
		callbacks: impl age_plugin::Callbacks<age_plugin::recipient::Error>,
	) -> std::io::Result<
		Result<Vec<Vec<age_core::format::Stanza>>, Vec<age_plugin::recipient::Error>>,
	> {
		todo!()
	}
}

struct IdentityV1;
impl IdentityPluginV1 for IdentityV1 {
	fn add_identity(
		&mut self,
		index: usize,
		plugin_name: &str,
		bytes: &[u8],
	) -> Result<(), age_plugin::identity::Error> {
		todo!()
	}

	fn unwrap_file_keys(
		&mut self,
		files: Vec<Vec<age_core::format::Stanza>>,
		callbacks: impl age_plugin::Callbacks<age_plugin::identity::Error>,
	) -> std::io::Result<
		std::collections::HashMap<
			usize,
			Result<age_core::format::FileKey, Vec<age_plugin::identity::Error>>,
		>,
	> {
		todo!()
	}
}

struct Plugin;
impl PluginHandler for Plugin {
	type RecipientV1 = RecipientV1;

	type IdentityV1 = IdentityV1;
}

fn read_arg(s: &mut impl Read) -> Result<(bool, Vec<u8>)> {
	let mut out = vec![];
	let mut b = [b' '];
	loop {
		s.read_exact(&mut b)?;
		if b[0] == b'\n' {
			return Ok((true, out));
		} else if b[0] == b' ' {
			return Ok((false, out));
		}
		out.push(b[0]);
	}
}
fn read_msg(s: &mut impl Read) -> Result<String> {
	let mut out = vec![];
	let mut b = [b' '];
	loop {
		s.read_exact(&mut b)?;
		if b[0] == b'\n' {
			return Ok(String::from_utf8_lossy(&b).to_string());
		}
		out.push(b[0]);
	}
}

fn read_response(s: &mut impl Read) -> Result<Option<(bool, Vec<u8>)>> {
	loop {
		let (end, cmd) = read_arg(s)?;
		return if &cmd == b"OK" {
			if !end {
				let msg = read_msg(s)?;
				eprintln!("OK {msg}");
			} else {
				eprintln!("OK");
			}
			Ok(None)
		} else if &cmd == b"S" {
			ensure!(!end, "expected status message");
			let msg = read_msg(s)?;
			eprintln!("S {msg}");
			continue;
		} else if &cmd == b"ERR" {
			ensure!(!end, "expected error code");
			let (end, code) = read_arg(s)?;
			let code = str::from_utf8(&code).context("code is ascii number")?;
			let code = u32::from_str_radix(&code, 10)?;
			if !end {
				let msg = read_msg(s)?;
				eprintln!("ERR {code} {msg}");
			} else {
				eprintln!("ERR {code}");
			}
			bail!("gpg error {code}")
		} else {
			Ok(Some((end, cmd)))
		};
	}
}
fn read_simple_result(s: &mut impl Read) -> Result<()> {
	let v = read_response(s)?;
	if let Some((_, v)) = v {
		bail!(
			"unexpected response type: {:?}",
			String::from_utf8_lossy(&v)
		);
	}
	Ok(())
}
fn read_cmd(s: &mut impl Read) -> Result<(bool, Vec<u8>)> {
	let Some((end, cmd)) = read_response(s)? else {
		bail!("expected command");
	};
	Ok((end, cmd))
}
fn read_raw(s: &mut impl Read) -> Result<Vec<u8>> {
	let (end, cmd) = read_cmd(s)?;
	ensure!(cmd == b"D");
	ensure!(!end, "missing raw data");
	let mut out = Vec::new();
	loop {
		let mut b = [0];
		s.read_exact(&mut b)?;
		if b[0] == b'%' {
			let mut hb = [0, 0];
			s.read_exact(&mut hb)?;
			match &hb {
				b"25" => {
					out.push(b'%');
				}
				b"0D" => {
					out.push(b'\r');
				}
				b"0A" => {
					out.push(b'\n');
				}
				_ => todo!("{}{}", hb[0] as char, hb[1] as char),
			}
		} else if b[0] == b'\n' {
			return Ok(out);
		} else {
			out.push(b[0]);
		}
	}
}

fn write_raw(s: &mut impl Write, data: &[u8]) -> Result<()> {
	s.write_all(b"D ")?;
	for b in data {
		match b {
			b'%' => {
				s.write_all(b"%25")?;
			}
			b'\r' => {
				s.write_all(b"%0D")?;
			}
			b'\n' => {
				s.write_all(b"%0A")?;
			}
			v => s.write_all(&[*v])?,
		}
	}
	s.write_all(b"\n")?;
	Ok(())
}
fn read_inquire(s: &mut impl Read) -> Result<Vec<u8>> {
	let (end, cmd) = read_cmd(s)?;
	ensure!(cmd == b"INQUIRE");
	ensure!(!end, "missing inquiry");

	let (end, kw) = read_arg(s)?;

	if !end {
		read_msg(s)?;
	}

	Ok(kw)
}

fn find_socket() -> Result<PathBuf> {
	let mut cmd = Command::new("gpgconf");
	cmd.arg("--list-dirs").arg("agent-socket");
	cmd.stdout(Stdio::piped());
	let child = cmd.spawn()?;
	let mut out = child.wait_with_output()?;
	if !out.status.success() {
		bail!("failed to find agent socket");
	}
	assert!(out.stdout.ends_with(b"\n"));
	out.stdout.remove(out.stdout.len() - 1);
	Ok(OsString::from_vec(out.stdout).into())
}

struct Atom<'i>(&'i [u8]);
impl fmt::Debug for Atom<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let t = String::from_utf8_lossy(&self.0);
		write!(f, "{t:?}")
	}
}

fn dec_num(input: &[u8]) -> IResult<&[u8], u32> {
	let digit1 = take_while(|c: u8| c.is_ascii_digit());

	map_res(digit1, |digits| {
		let digits = std::str::from_utf8(digits).expect("digits are valid utf8");
		digits.parse()
	})
	.parse(input)
}

fn atom(input: &[u8]) -> IResult<&[u8], Atom<'_>> {
	let (input, len) = terminated(dec_num, tag(":")).parse(input)?;
	map(take(len as usize), Atom).parse(input)
}

#[derive(Debug)]
struct Expr<'i> {
	start: Atom<'i>,
	children: Vec<SexprOrAtom<'i>>,
}
impl Expr<'_> {
	fn unwrap<'t>(self, v: &str) -> Result<Vec<SexprOrAtom<'t>>>
	where
		Self: 't,
	{
		ensure!(
			self.start.0 == v.as_bytes(),
			"expected wrapper for {v}, got {self:?}",
		);
		Ok(self.children)
	}
	fn unwrap_single<'t>(self, v: &str) -> Result<SexprOrAtom<'t>>
	where
		Self: 't,
	{
		let r = self.unwrap(v)?;
		ensure!(r.len() == 1, "unexpected number of children for {v}");
		Ok(r.into_iter().next().expect("exactly one"))
	}
	fn unwrap_single_expr<'t>(self, v: &str) -> Result<Expr<'t>>
	where
		Self: 't,
	{
		let r = self.unwrap_single(v)?;
		match r {
			SexprOrAtom::Expr(expr) => Ok(expr),
			SexprOrAtom::Atom(_) => bail!("unexpected atom wrapping for {v}"),
		}
	}
	fn unwrap_single_atom<'t>(self, v: &str) -> Result<Atom<'t>>
	where
		Self: 't,
	{
		let r = self.unwrap_single(v)?;
		match r {
			SexprOrAtom::Atom(expr) => Ok(expr),
			SexprOrAtom::Expr(_) => bail!("unexpected expr wrapping for {v}"),
		}
	}
}

#[derive(Debug)]
enum SexprOrAtom<'i> {
	Expr(Expr<'i>),
	Atom(Atom<'i>),
}
impl<'i> SexprOrAtom<'i> {
	fn unwrap_expr(self) -> Result<Expr<'i>> {
		match self {
			SexprOrAtom::Expr(expr) => Ok(expr),
			SexprOrAtom::Atom(atom) => bail!("expected expr"),
		}
	}
}

fn sexpr_or_atom(input: &[u8]) -> IResult<&[u8], SexprOrAtom<'_>> {
	if matches!(input.first(), Some(b'(')) {
		map(sexpr, SexprOrAtom::Expr).parse(input)
	} else {
		map(atom, SexprOrAtom::Atom).parse(input)
	}
}

fn sexpr<'i>(input: &'i [u8]) -> IResult<&'i [u8], Expr<'i>> {
	map(
		preceded(tag("("), (atom, many_till(sexpr_or_atom, tag(")")))),
		|(start, (children, _))| Expr { start, children },
	)
	.parse(input)
}

fn parse_public_key(e: Expr) -> Result<Key<PublicParts, UnspecifiedRole>> {
	let e = e.unwrap_single_expr("public-key")?;
	if e.start.0 == b"rsa" {
		let e = e.unwrap("rsa")?;
		ensure!(e.len() == 2, "rsa should have two children");
		let mut e = e.into_iter();
		let n = e
			.next()
			.expect("n")
			.unwrap_expr()?
			.unwrap_single_atom("n")?;
		let e = e
			.next()
			.expect("e")
			.unwrap_expr()?
			.unwrap_single_atom("e")?;
		Ok(Key::V6(Key6::import_public_rsa(e.0, n.0, None)?))
	} else if e.start.0 == b"ecc" {
		let e = e.unwrap("ecc")?;
		ensure!(e.len() == 3, "ecc should have three children");
		let mut e = e.into_iter();
		let curve = e
			.next()
			.expect("curve")
			.unwrap_expr()?
			.unwrap_single_atom("curve")?;
		let flags = e
			.next()
			.expect("flags")
			.unwrap_expr()?
			.unwrap_single_atom("flags")?;
		let q = e
			.next()
			.expect("q")
			.unwrap_expr()?
			.unwrap_single_atom("q")?;
		dbg!(flags);
		ensure!(curve.0 == b"Curve25519", "unsupported curve: {:?}", curve);
		Ok(Key::V6(Key6::new(
			Timestamp::now(),
			sequoia_openpgp::types::PublicKeyAlgorithm::ECDH,
			PublicKey::ECDH {
				curve: Curve::Cv25519,
				q: q.0.to_vec().into(),
				// TODO: Get that data from gpg somehow?..
				hash: HashAlgorithm::SHA256,
				sym: SymmetricAlgorithm::AES128,
			},
		)?))
	} else {
		todo!()
	}
}

fn read_sexpr<'i, T: 'static>(
	s: &mut impl Read,
	parse: impl FnOnce(Expr<'_>) -> Result<T>,
) -> Result<T> {
	let r = read_raw(s)?;
	// TODO: Multiple raw responses might be sent before result, they should be concatenated.
	// In fact, they should be concatenated at read_raw level.
	read_simple_result(s)?;
	let (input, value) = sexpr(&r).map_err(|e| anyhow!("failed to parse sexpr: {e}"))?;
	ensure!(input.is_empty(), "unexpected data after sexpr");
	parse(value)
}

fn setkey<S: Read + Write>(s: &mut S, keygrip: &str) -> Result<()> {
	eprintln!("SETKEY {keygrip}");
	s.write_all(format!("SETKEY {keygrip}\n").as_bytes())?;
	read_simple_result(s)
}
fn pkdecrypt<S: Read + Write>(s: &mut S, v: &[u8], extra: &[u8]) -> Result<Vec<u8>> {
	eprintln!("PKDECRYPT");
	s.write_all(b"PKDECRYPT\n")?;
	let i = read_inquire(s)?;
	ensure!(i == b"CIPHERTEXT");

	// Build complete S-expression as a single buffer
	write_raw(s, b"(7:enc-val(4:ecdh(1:s")?;
	write_raw(s, format!("{}:", v.len()).as_bytes())?;
	write_raw(s, v)?;
	write_raw(s, b")(1:e")?;
	write_raw(s, format!("{}:", extra.len()).as_bytes())?;
	write_raw(s, extra)?;
	write_raw(s, b")))")?;

	s.write_all(b"END\n")?;

	let res = read_raw(s)?;
	read_simple_result(s)?;

	Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
	let mut agent = Agent::connect_to_default()
		.await
		.context("gpg agent connection failed")?;

	let keys = agent.list_keys().await.context("failed to list keys")?;

	let keygrip = "E968AB03A34F6F291B800C6121F350FCFCE8DE4C";
	// let key =
	// let kg = keys.lookup(keygrip).expect("kg");
	// let kg = kg.keygrip();
	let key = agent.send_simple(format!("READKEY {keygrip}")).await?;
	let (_res, key) = sexpr(&key).map_err(|e| anyhow!("failed to parse key sexpr: {e}"))?;
	assert!(_res.is_empty(), "unexpected data after key expr");
	let key = parse_public_key(key)?;

	let encrypted = key.encrypt(&SessionKey::from("Hello, world!!!!".as_bytes().to_vec()))?;

	agent.send_simple(format!("SETKEY {keygrip}")).await?;

	ensure!(agent.has_key(&key).await.context("has key check")?);
	let mut keypair = agent.keypair(&key)?;

	let decrypted = keypair.decrypt_async(&encrypted, None).await?;

	dbg!(String::from_utf8_lossy(decrypted.as_bytes()));
	// dbg!(kek.as_bytes());

	let aussan = find_socket()?;

	let mut stream = UnixStream::connect(aussan)?;
	read_simple_result(&mut stream)?;

	// let key = readkey(&mut stream, keygrip)?;
	// let (data, extra) = key.encrypt(b"Hello, world!!!!")?;
	//
	// setkey(&mut stream, keygrip)?;
	// let v = pkdecrypt(&mut stream, &data, &extra)?;

	eprintln!("DONE");

	let mut buf = [0u8; 2048];
	loop {
		let len = stream.read(&mut buf)?;
		let buf = &buf[0..len];
		dbg!(String::from_utf8_lossy(buf));
	}

	// let opts = Opts::parse();
	//
	// run_state_machine(opts.age_plugin.as_str(), Plugin);

	Ok(())
}
