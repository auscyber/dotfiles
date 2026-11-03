//! Signs Mach-O binaries with a key held by an autopen server: the signature
//! is produced by `autopen sign` against a remote-key handle, so this process
//! never holds key material.

use std::{
    path::{Path, PathBuf},
    process::{Command, ExitCode},
};

use apple_codesign::{SettingsScope, SigningSettings, UnifiedSigner};
use bytes::Bytes;
use clap::Parser;
use x509_certificate::{
    CapturedX509Certificate, KeyAlgorithm, KeyInfoSigner, Sign, Signature, SignatureAlgorithm,
    X509CertificateError,
};
use zeroize::Zeroizing;

/// RSA-3072, the only key autopen issues.
const SIGNATURE_LEN: usize = 384;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("reading {path}: {source}")]
    Read {
        path: PathBuf,
        source: std::io::Error,
    },
    #[error("running {autopen}: {source}")]
    Spawn {
        autopen: PathBuf,
        source: std::io::Error,
    },
    #[error("autopen sign failed ({status}): {stderr}")]
    Autopen { status: String, stderr: String },
    #[error("autopen returned a {0}-byte signature, expected {SIGNATURE_LEN}")]
    SignatureLength(usize),
    #[error("parsing the certificate: {0}")]
    Certificate(#[from] X509CertificateError),
    #[error(transparent)]
    Codesign(#[from] apple_codesign::AppleCodesignError),
    #[error("temporary file: {0}")]
    Temp(std::io::Error),
}

/// An autopen remote-key handle (`autopen signing-key remote create`).
#[derive(Debug, Clone)]
struct RemoteKey(PathBuf);

/// The code-signing identifier, e.g. the binary's name. Never empty.
#[derive(Debug, Clone)]
struct BinaryIdentifier(String);

impl std::str::FromStr for BinaryIdentifier {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Err("binary identifier must not be empty")
        } else {
            Ok(Self(s.to_owned()))
        }
    }
}

struct AutopenSigner {
    autopen: PathBuf,
    key: RemoteKey,
    public_key: Bytes,
}

impl AutopenSigner {
    fn sign_bytes(&self, message: &[u8]) -> Result<Vec<u8>, Error> {
        let dir = tempfile::tempdir().map_err(Error::Temp)?;
        let msg = dir.path().join("message");
        let sig = dir.path().join("signature");
        std::fs::write(&msg, message).map_err(Error::Temp)?;

        let out = Command::new(&self.autopen)
            .arg("sign")
            .arg("--signing-key")
            .arg(&self.key.0)
            .arg("--output")
            .arg(&sig)
            .arg(&msg)
            .output()
            .map_err(|source| Error::Spawn {
                autopen: self.autopen.clone(),
                source,
            })?;
        if !out.status.success() {
            return Err(Error::Autopen {
                status: out.status.to_string(),
                stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
            });
        }

        let signature = read(&sig)?;
        if signature.len() != SIGNATURE_LEN {
            return Err(Error::SignatureLength(signature.len()));
        }
        Ok(signature)
    }
}

impl signature::Signer<Signature> for AutopenSigner {
    fn try_sign(&self, msg: &[u8]) -> Result<Signature, signature::Error> {
        self.sign_bytes(msg)
            .map(Signature::from)
            .map_err(signature::Error::from_source)
    }
}

impl Sign for AutopenSigner {
    fn sign(&self, message: &[u8]) -> Result<(Vec<u8>, SignatureAlgorithm), X509CertificateError> {
        self.sign_bytes(message)
            .map(|s| (s, SignatureAlgorithm::RsaSha256))
            .map_err(|e| X509CertificateError::Other(e.to_string()))
    }

    fn key_algorithm(&self) -> Option<KeyAlgorithm> {
        Some(KeyAlgorithm::Rsa)
    }

    fn public_key_data(&self) -> Bytes {
        self.public_key.clone()
    }

    fn signature_algorithm(&self) -> Result<SignatureAlgorithm, X509CertificateError> {
        Ok(SignatureAlgorithm::RsaSha256)
    }

    fn private_key_data(&self) -> Option<Zeroizing<Vec<u8>>> {
        None
    }

    fn rsa_primes(
        &self,
    ) -> Result<Option<(Zeroizing<Vec<u8>>, Zeroizing<Vec<u8>>)>, X509CertificateError> {
        Ok(None)
    }
}

impl KeyInfoSigner for AutopenSigner {}

/// Sign a Mach-O with a key held by an autopen server.
#[derive(Debug, Parser)]
#[command(version)]
struct Cli {
    /// The autopen executable.
    #[arg(long, env = "AUTOPEN", default_value = "autopen")]
    autopen: PathBuf,

    /// Remote-key handle from `autopen signing-key remote create`.
    #[arg(long, value_name = "PATH")]
    remote_key: PathBuf,

    /// DER certificate for that key, from `autopen x509 create-certificate`.
    #[arg(long, value_name = "PATH")]
    certificate: PathBuf,

    #[arg(long)]
    binary_identifier: Option<BinaryIdentifier>,

    #[arg(long, value_name = "PATH")]
    entitlements_xml_path: Option<PathBuf>,

    input: PathBuf,
    output: PathBuf,
}

fn read(path: &Path) -> Result<Vec<u8>, Error> {
    std::fs::read(path).map_err(|source| Error::Read {
        path: path.to_owned(),
        source,
    })
}

fn run(cli: Cli) -> Result<(), Error> {
    let certificate = CapturedX509Certificate::from_der(read(&cli.certificate)?)?;
    let signer = AutopenSigner {
        autopen: cli.autopen,
        key: RemoteKey(cli.remote_key),
        public_key: certificate.public_key_data(),
    };

    let mut settings = SigningSettings::default();
    settings.set_signing_key(&signer, certificate);
    if let Some(id) = cli.binary_identifier {
        settings.set_binary_identifier(SettingsScope::Main, id.0);
    }
    if let Some(path) = cli.entitlements_xml_path {
        let xml = String::from_utf8_lossy(&read(&path)?).into_owned();
        settings.set_entitlements_xml(SettingsScope::Main, xml)?;
    }

    UnifiedSigner::new(settings).sign_path(&cli.input, &cli.output)?;
    Ok(())
}

fn main() -> ExitCode {
    match run(Cli::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("autopen-codesign: {e}");
            ExitCode::FAILURE
        }
    }
}
