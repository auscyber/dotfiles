use std::process::ExitCode;

use async_mach_ports::{Receiver, RecvPort};
use futures_lite::future::block_on;
use wrapperd::{plant, Ack, Request, Wire, SERVICE};

fn usage() -> ExitCode {
    eprintln!(
        "usage:\n  wrapperd plant --manifest <path>\n  wrapperd serve --manifest <path> [--service <name>]"
    );
    ExitCode::from(2)
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut it = args.iter();
    let Some(cmd) = it.next() else {
        return usage();
    };

    let mut manifest: Option<String> = None;
    let mut service = SERVICE.to_string();
    while let Some(a) = it.next() {
        match a.as_str() {
            "--manifest" => manifest = it.next().cloned(),
            "--service" => {
                if let Some(s) = it.next() {
                    service = s.clone();
                }
            }
            other => {
                eprintln!("wrapperd: unexpected argument {other:?}");
                return usage();
            }
        }
    }

    match cmd.as_str() {
        "plant" => match do_plant(manifest.as_deref()) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("wrapperd: {e}");
                ExitCode::FAILURE
            }
        },
        "serve" => match serve(manifest.as_deref(), &service) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("wrapperd: {e}");
                ExitCode::FAILURE
            }
        },
        _ => usage(),
    }
}

fn do_plant(manifest: Option<&str>) -> Result<(), String> {
    let path = manifest.ok_or("plant needs --manifest")?;
    let text = std::fs::read_to_string(path).map_err(|e| format!("read {path}: {e}"))?;
    let manifest = plant::parse(&text).map_err(|e| e.to_string())?;
    let n = plant::run(&manifest).map_err(|e| e.to_string())?;
    eprintln!(
        "wrapperd: plant complete ({n} changed, {} total)",
        manifest.wrappers.len()
    );
    Ok(())
}

// Plant the wrappers, then answer readiness over the Mach service so the
// per-agent `wrapperd-wait` shims that gate on it unblock only once the trusted
// binaries actually exist. The barrier is global: every label is ready as soon
// as planting finishes. Per-agent ordering hangs off the same loop when it is
// wanted -- track state keyed by `label` and defer the reply.
fn serve(manifest: Option<&str>, service: &str) -> Result<(), String> {
    do_plant(manifest)?;

    block_on(async {
        let rx = Receiver::<Request, _>::bind(service, Wire)
            .map_err(|e| format!("bind {service}: {e:?}"))?;
        eprintln!("wrapperd: serving {service}");
        loop {
            let delivery = rx.recv().await.map_err(|e| format!("recv: {e:?}"))?;
            match delivery.value {
                Request::WaitReady { label } => {
                    if let Some(reply) = delivery.reply {
                        let _ = reply.send(&Ack::Ready);
                    }
                    eprintln!("wrapperd: released {label}");
                }
                Request::Checkin { label } => {
                    eprintln!("wrapperd: checkin {label}");
                }
            }
        }
    })
}
