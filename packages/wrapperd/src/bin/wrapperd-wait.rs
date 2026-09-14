use std::os::unix::process::CommandExt;
use std::process::{Command, ExitCode};
use std::thread::sleep;
use std::time::{Duration, Instant};

use async_mach_ports::{Sender, SendPort};
use wrapperd::{Ack, Request, Wire, SERVICE};

// Gate a launchd job on the orchestrator: block until the wrappers are planted
// and this label is released, then `exec` the real (signed) binary at its stable
// path. Kept off `/run/wrappers` itself -- this shim is a store binary -- so it
// always runs at boot even while the directory it waits on is still empty.
fn main() -> ExitCode {
    let mut label: Option<String> = None;
    let mut then: Option<String> = None;
    let mut service = SERVICE.to_string();
    let mut timeout = Duration::from_secs(60);
    let mut rest: Vec<String> = Vec::new();

    let mut it = std::env::args().skip(1);
    while let Some(a) = it.next() {
        match a.as_str() {
            "--label" => label = it.next(),
            "--then" => then = it.next(),
            "--service" => {
                if let Some(s) = it.next() {
                    service = s;
                }
            }
            "--timeout" => {
                if let Some(s) = it.next() {
                    if let Ok(n) = s.parse::<u64>() {
                        timeout = Duration::from_secs(n);
                    }
                }
            }
            "--" => {
                rest.extend(it.by_ref());
                break;
            }
            other => {
                eprintln!("wrapperd-wait: unexpected argument {other:?}");
                return ExitCode::from(2);
            }
        }
    }

    let (Some(label), Some(then)) = (label, then) else {
        eprintln!("wrapperd-wait: --label and --then are required");
        return ExitCode::from(2);
    };

    let deadline = Instant::now() + timeout;
    loop {
        if try_release(&service, &label) {
            let err = Command::new(&then).args(&rest).exec();
            eprintln!("wrapperd-wait: exec {then}: {err}");
            return ExitCode::FAILURE;
        }
        if Instant::now() >= deadline {
            eprintln!("wrapperd-wait: timed out waiting for {service} ({label})");
            return ExitCode::FAILURE;
        }
        sleep(Duration::from_millis(200));
    }
}

fn try_release(service: &str, label: &str) -> bool {
    let Ok(sender) = Sender::<Request, _>::connect(service, Wire) else {
        return false;
    };
    matches!(
        sender.call_blocking::<Ack>(&Request::WaitReady {
            label: label.to_string(),
        }),
        Ok(Ack::Ready)
    )
}
