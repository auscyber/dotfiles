// The actual ordering mechanism: a `wrapperd serve` receiver releases a waiter
// over a Mach service. This exercises `async-mach-ports` + the `Wire` codec +
// the `Request`/`Ack` protocol end to end, in-process (the receiver on one
// thread, the blocking client -- exactly what `wrapperd-wait` does -- on
// another).
//
// `#[ignore]` because it touches the bootstrap server, which a build sandbox
// denies; `cargoCheckHook` runs plain `cargo test`, so it stays out of the nix
// build. Run it explicitly: `cargo test -- --ignored`.

use std::thread;
use std::time::Duration;

use async_mach_ports::{Receiver, RecvPort, Sender, SendPort};
use wrapperd::{Ack, Request, Wire};

fn unique_service(tag: &str) -> String {
    format!("in.ivymect.wrapperd.test.{}.{tag}", std::process::id())
}

// A waiter blocked on `WaitReady` is released once the server answers -- the
// core of the boot ordering.
#[test]
#[ignore = "needs the Mach bootstrap server; not sandbox-safe"]
fn wait_ready_is_answered() {
    let service = unique_service("ready");

    let server_service = service.clone();
    let server = thread::spawn(move || {
        futures_lite::future::block_on(async {
            let rx = Receiver::<Request, _>::bind(&server_service, Wire).expect("bind service");
            let delivery = rx.recv().await.expect("recv request");
            match delivery.value {
                Request::WaitReady { label } => {
                    assert_eq!(label, "paneru");
                    delivery
                        .reply
                        .expect("WaitReady carries a reply port")
                        .send(&Ack::Ready)
                        .expect("send ack");
                }
                other => panic!("unexpected request: {other:?}"),
            }
        });
    });

    // The client retries connect until the server thread has bound the name --
    // the same shape `wrapperd-wait` uses, so this also covers the "daemon not
    // up yet" path.
    let ack = call_with_retry(&service, &Request::WaitReady { label: "paneru".into() });
    assert!(matches!(ack, Some(Ack::Ready)), "expected Ready, got {ack:?}");

    server.join().unwrap();
}

// A client that gives up (drops the future / stops retrying) before the server
// exists must not wedge: `connect`/`call` simply keep failing. This is the
// timeout path `wrapperd-wait` relies on to exit non-zero so KeepAlive retries.
#[test]
#[ignore = "needs the Mach bootstrap server; not sandbox-safe"]
fn call_to_absent_service_fails_rather_than_blocking() {
    let service = unique_service("absent");
    let sender = Sender::<Request, _>::connect(&service, Wire);
    // Either connect fails outright (no such name) or the subsequent call does;
    // either way it returns, it does not hang.
    if let Ok(s) = sender {
        assert!(s
            .call_blocking::<Ack>(&Request::WaitReady { label: "x".into() })
            .is_err());
    }
}

fn call_with_retry(service: &str, req: &Request) -> Option<Ack> {
    for _ in 0..100 {
        if let Ok(sender) = Sender::<Request, _>::connect(service, Wire) {
            if let Ok(ack) = sender.call_blocking::<Ack>(req) {
                return Some(ack);
            }
        }
        thread::sleep(Duration::from_millis(50));
    }
    None
}
