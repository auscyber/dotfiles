use std::io::Cursor;

use age_core::format::Stanza;
use sequoia_ipc::Keygrip;
use sequoia_openpgp::crypto::mpi::Ciphertext;
use sequoia_openpgp::serialize::MarshalInto;
use sequoia_openpgp::types::PublicKeyAlgorithm;

pub const GPG_STANZA_TAG: &str = "gpg-v1";

#[derive(thiserror::Error, Debug)]
pub enum Error {
	#[error("invalid stanza tag")]
	InvalidTag,
	#[error("missing stanza keygrip")]
	MissingKeygrip,
	#[error("missing stanza pk algo")]
	MissingPkAlgo,
	#[error("invalid stanza pk algo")]
	InvalidPkAlgo,
	#[error("unexpected number of argument")]
	UnknownArgs,
	#[error("invalid stanza keygrip: {0}")]
	Keygrip(anyhow::Error),
	#[error("invalid ciphertext")]
	Ciphertext(anyhow::Error),
}

pub struct GpgStanza {
	pub keygrip: Keygrip,
	pub ciphertext: Ciphertext,
}

impl From<GpgStanza> for Stanza {
	fn from(val: GpgStanza) -> Self {
		Stanza {
			tag: GPG_STANZA_TAG.to_owned(),
			args: vec![
				val.keygrip.to_string(),
				u8::from(val.ciphertext.pk_algo().expect("expected known algo")).to_string(),
			],
			body: val.ciphertext.to_vec().expect("to_vec shouldn't fail"),
		}
	}
}

impl TryFrom<Stanza> for GpgStanza {
	type Error = Error;

	fn try_from(value: Stanza) -> Result<Self, Self::Error> {
		if value.tag != GPG_STANZA_TAG {
			return Err(Error::InvalidTag);
		}
		let keygrip = value.args.first().ok_or(Error::MissingKeygrip)?;
		let keygrip: Keygrip = keygrip.parse().map_err(Error::Keygrip)?;

		let pk_algo = value.args.get(1).ok_or(Error::MissingPkAlgo)?;
		let pk_algo: u8 = pk_algo.parse().map_err(|_| Error::InvalidPkAlgo)?;
		let pk_algo = PublicKeyAlgorithm::from(pk_algo);

		if value.args.len() > 2 {
			return Err(Error::UnknownArgs);
		}

		Ok(Self {
			keygrip,
			ciphertext: Ciphertext::parse(pk_algo, Cursor::new(&value.body))
				.map_err(Error::Ciphertext)?,
		})
	}
}
