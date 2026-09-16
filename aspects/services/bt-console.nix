{ den, ... }:
# A small web console for a headless box: pair/connect bluetooth devices,
# read and cap the battery, and merge several bluetooth sinks into one
# pipewire combine-sink so AirPlay can address them as a single speaker.
#
# There is no off-the-shelf web UI for BlueZ, and blueman/bluetuith both want
# a session. This talks to org.bluez over the system bus directly and
# registers its own NoInputNoOutput agent, which is what makes pairing a
# speaker work with nobody sitting at the machine.
#
# NO AUTHENTICATION. Bind it to a trusted interface (the tailnet, or a LAN you
# own) and leave `openFirewall` off if you are not sure.
{
  den.aspects.bt-console = {
    includes = [ den.aspects.bluetooth ];

    nixos =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (lib) mkOption types;

        cfg = config.services.btConsole;

        pythonEnv = pkgs.python3.withPackages (ps: [
          ps.dbus-python
          ps.pygobject3
        ]);

        page = pkgs.writeText "bt-console.html" ''
          <!doctype html>
          <meta charset="utf-8">
          <meta name="viewport" content="width=device-width,initial-scale=1">
          <title>bt console</title>
          <style>
            :root { color-scheme: dark; --bg:#14161a; --fg:#e6e6e6; --dim:#8a8f98; --acc:#7aa2f7; --line:#262a31; }
            body { margin:0; padding:1.5rem; background:var(--bg); color:var(--fg);
                   font:15px/1.5 ui-sans-serif,system-ui,sans-serif; }
            h1 { font-size:1.1rem; letter-spacing:.06em; text-transform:uppercase; color:var(--dim); }
            h2 { font-size:.85rem; letter-spacing:.08em; text-transform:uppercase; color:var(--dim);
                 margin:2rem 0 .5rem; }
            .card { border:1px solid var(--line); border-radius:10px; padding:.8rem 1rem; margin:.5rem 0; }
            .row { display:flex; gap:.6rem; align-items:center; flex-wrap:wrap; }
            .grow { flex:1 1 auto; min-width:12rem; }
            .dim { color:var(--dim); font-size:.85rem; }
            button { background:#1d2027; color:var(--fg); border:1px solid var(--line);
                     border-radius:6px; padding:.35rem .7rem; cursor:pointer; font:inherit; }
            button:hover { border-color:var(--acc); color:var(--acc); }
            button[disabled] { opacity:.4; cursor:default; }
            .on { color:#9ece6a; }
            .bar { height:6px; background:#1d2027; border-radius:3px; overflow:hidden; margin-top:.4rem; }
            .bar > i { display:block; height:100%; background:var(--acc); }
            input[type=number] { width:5rem; background:#1d2027; color:var(--fg);
                                 border:1px solid var(--line); border-radius:6px; padding:.3rem; }
          </style>
          <h1>bt console</h1>
          <div id="app">loading…</div>
          <script>
          var S = {};

          function post(op, extra) {
            var body = Object.assign({op: op}, extra || {});
            return fetch('/api/action', {
              method: 'POST',
              headers: {'Content-Type': 'application/json'},
              body: JSON.stringify(body)
            }).then(refresh);
          }

          function el(tag, cls, text) {
            var n = document.createElement(tag);
            if (cls) n.className = cls;
            if (text != null) n.textContent = text;
            return n;
          }

          function btn(label, fn) {
            var b = el('button', null, label);
            b.onclick = fn;
            return b;
          }

          function renderBattery(root) {
            root.appendChild(el('h2', null, 'battery'));
            if (!S.battery || !S.battery.length) {
              root.appendChild(el('div', 'dim', 'no battery reported under /sys/class/power_supply'));
              return;
            }
            S.battery.forEach(function (b) {
              var c = el('div', 'card');
              var r = el('div', 'row');
              r.appendChild(el('div', 'grow', b.name + ' — ' + b.status));
              r.appendChild(el('div', null, b.capacity == null ? '?' : b.capacity + '%'));
              c.appendChild(r);
              if (b.capacity != null) {
                var bar = el('div', 'bar');
                var i = el('i');
                i.style.width = b.capacity + '%';
                bar.appendChild(i);
                c.appendChild(bar);
              }
              if (b.behaviours && b.behaviours.length > 1) {
                var r3 = el('div', 'row');
                r3.style.marginTop = '.6rem';
                r3.appendChild(el('span', 'dim', 'charging'));
                b.behaviours.forEach(function (name) {
                  var x = btn(name, function () {
                    post('charge_behaviour', {name: b.name, value: name});
                  });
                  if (name === b.behaviour) x.classList.add('on');
                  r3.appendChild(x);
                });
                c.appendChild(r3);
              }
              if (b.charge_limit_supported) {
                var r2 = el('div', 'row');
                r2.style.marginTop = '.6rem';
                r2.appendChild(el('span', 'dim', 'charge limit'));
                var inp = document.createElement('input');
                inp.type = 'number';
                inp.min = 50;
                inp.max = 100;
                inp.value = b.charge_limit == null ? 100 : b.charge_limit;
                r2.appendChild(inp);
                r2.appendChild(btn('set', function () {
                  post('charge_limit', {name: b.name, value: parseInt(inp.value, 10)});
                }));
                c.appendChild(r2);
              }
              root.appendChild(c);
            });
          }

          function renderDevices(root) {
            var h = el('h2', null, 'bluetooth');
            root.appendChild(h);
            var bar = el('div', 'row');
            bar.appendChild(el('span', 'dim',
              'adapter ' + (S.adapter && S.adapter.powered ? 'on' : 'off')));
            bar.appendChild(btn(S.adapter && S.adapter.powered ? 'power off' : 'power on',
              function () { post('power', {value: !(S.adapter && S.adapter.powered)}); }));
            bar.appendChild(btn(S.adapter && S.adapter.discovering ? 'stop scan' : 'scan',
              function () { post(S.adapter && S.adapter.discovering ? 'scan_off' : 'scan_on'); }));
            root.appendChild(bar);

            (S.devices || []).forEach(function (d) {
              var c = el('div', 'card');
              var r = el('div', 'row');
              var t = el('div', 'grow');
              t.appendChild(el('div', null, d.name || d.address));
              var meta = [d.address];
              if (d.audio) meta.push('audio');
              if (d.paired) meta.push('paired');
              if (d.trusted) meta.push('trusted');
              if (d.battery != null) meta.push(d.battery + '%');
              t.appendChild(el('div', 'dim', meta.join(' · ')));
              r.appendChild(t);
              if (d.connected) r.appendChild(el('span', 'on', 'connected'));
              if (!d.paired) r.appendChild(btn('pair', function () { post('pair', {path: d.path}); }));
              r.appendChild(btn(d.connected ? 'disconnect' : 'connect', function () {
                post(d.connected ? 'disconnect' : 'connect', {path: d.path});
              }));
              if (!d.trusted) r.appendChild(btn('trust', function () { post('trust', {path: d.path}); }));
              if (d.paired) r.appendChild(btn('forget', function () { post('remove', {path: d.path}); }));
              c.appendChild(r);
              root.appendChild(c);
            });
          }

          function renderCombine(root) {
            root.appendChild(el('h2', null, 'merge sinks'));
            var c = el('div', 'card');
            if (S.combined) {
              c.appendChild(el('div', null, 'combined sink active: ' + S.combined.slaves.join(', ')));
              var r0 = el('div', 'row');
              r0.style.marginTop = '.5rem';
              r0.appendChild(btn('unmerge', function () { post('combine_clear'); }));
              c.appendChild(r0);
              root.appendChild(c);
              return;
            }
            var chosen = {};
            (S.sinks || []).forEach(function (s) {
              var r = el('div', 'row');
              var cb = document.createElement('input');
              cb.type = 'checkbox';
              cb.onchange = function () { chosen[s.name] = cb.checked; };
              r.appendChild(cb);
              var t = el('div', 'grow');
              t.appendChild(el('div', null, s.description || s.name));
              t.appendChild(el('div', 'dim', s.name));
              r.appendChild(t);
              c.appendChild(r);
            });
            var r2 = el('div', 'row');
            r2.style.marginTop = '.5rem';
            r2.appendChild(btn('merge selected', function () {
              var names = Object.keys(chosen).filter(function (k) { return chosen[k]; });
              if (names.length < 2) { alert('pick at least two sinks'); return; }
              post('combine_set', {sinks: names});
            }));
            c.appendChild(r2);
            root.appendChild(c);
          }

          function render() {
            var root = el('div');
            renderDevices(root);
            renderCombine(root);
            renderBattery(root);
            var app = document.getElementById('app');
            app.replaceChildren(root);
          }

          function refresh() {
            return fetch('/api/state')
              .then(function (r) { return r.json(); })
              .then(function (j) { S = j; render(); })
              .catch(function (e) {
                document.getElementById('app').textContent = 'error: ' + e;
              });
          }

          refresh();
          setInterval(refresh, 4000);
          </script>
        '';

        server = pkgs.writeText "bt-console.py" ''
          import json
          import os
          import re
          import subprocess
          import threading
          import time
          from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

          import dbus
          import dbus.service
          from dbus.mainloop.glib import DBusGMainLoop, threads_init
          from gi.repository import GLib

          PAGE = "${page}"
          LISTEN = (os.environ["BTC_ADDRESS"], int(os.environ["BTC_PORT"]))
          PULSE = os.environ["BTC_PULSE_SERVER"]
          COMBINED = "btconsole-combined"
          PSDIR = "/sys/class/power_supply"
          AUDIO_UUIDS = {
              "0000110b-0000-1000-8000-00805f9b34fb",
              "0000110a-0000-1000-8000-00805f9b34fb",
          }
          AGENT_PATH = "/dendritic/btconsole/agent"

          DBusGMainLoop(set_as_default=True)
          threads_init()
          bus = dbus.SystemBus()
          lock = threading.Lock()


          class Agent(dbus.service.Object):
              """NoInputNoOutput: accept everything. Nobody is at the keyboard."""

              @dbus.service.method("org.bluez.Agent1", in_signature="", out_signature="")
              def Release(self):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="o", out_signature="")
              def RequestAuthorization(self, device):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="os", out_signature="")
              def AuthorizeService(self, device, uuid):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="o", out_signature="s")
              def RequestPinCode(self, device):
                  return "0000"

              @dbus.service.method("org.bluez.Agent1", in_signature="o", out_signature="u")
              def RequestPasskey(self, device):
                  return dbus.UInt32(0)

              @dbus.service.method("org.bluez.Agent1", in_signature="ouq", out_signature="")
              def DisplayPasskey(self, device, passkey, entered):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="os", out_signature="")
              def DisplayPinCode(self, device, pincode):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="ou", out_signature="")
              def RequestConfirmation(self, device, passkey):
                  pass

              @dbus.service.method("org.bluez.Agent1", in_signature="", out_signature="")
              def Cancel(self):
                  pass


          def wait_for_bluez(timeout=180):
              """bluetoothd takes the bus name some time after the unit starts."""
              deadline = time.monotonic() + timeout
              while time.monotonic() < deadline:
                  try:
                      if bus.name_has_owner("org.bluez"):
                          return True
                  except dbus.DBusException:
                      pass
                  time.sleep(2)
              return False


          def register_agent():
              Agent(bus, AGENT_PATH)
              mgr = dbus.Interface(
                  bus.get_object("org.bluez", "/org/bluez"), "org.bluez.AgentManager1"
              )
              mgr.RegisterAgent(AGENT_PATH, "NoInputNoOutput")
              mgr.RequestDefaultAgent(AGENT_PATH)


          def managed():
              try:
                  mgr = dbus.Interface(
                      bus.get_object("org.bluez", "/"), "org.freedesktop.DBus.ObjectManager"
                  )
                  return mgr.GetManagedObjects()
              except dbus.DBusException:
                  # bluetoothd restarting: serve battery and sinks anyway.
                  return {}


          def iface(path, name):
              return dbus.Interface(bus.get_object("org.bluez", path), name)


          def props(path):
              return dbus.Interface(
                  bus.get_object("org.bluez", path), "org.freedesktop.DBus.Properties"
              )


          def pactl(*args):
              env = dict(os.environ, PULSE_SERVER=PULSE)
              return subprocess.run(
                  ["pactl", *args], capture_output=True, text=True, env=env, check=False
              )


          def sinks():
              out = pactl("-f", "json", "list", "sinks")
              if out.returncode != 0:
                  return []
              try:
                  return [
                      {"name": s["name"], "description": s.get("description", "")}
                      for s in json.loads(out.stdout)
                  ]
              except (ValueError, KeyError):
                  return []


          def combined():
              out = pactl("list", "short", "modules")
              for line in out.stdout.splitlines():
                  if "module-combine-sink" in line and COMBINED in line:
                      idx = line.split("\t")[0]
                      m = re.search(r"slaves=(\S+)", line)
                      return {"index": idx, "slaves": m.group(1).split(",") if m else []}
              return None


          def battery():
              found = []
              if not os.path.isdir(PSDIR):
                  return found
              for name in sorted(os.listdir(PSDIR)):
                  base = os.path.join(PSDIR, name)

                  def read(attr):
                      try:
                          with open(os.path.join(base, attr)) as fh:
                              return fh.read().strip()
                      except OSError:
                          return None

                  if read("type") != "Battery":
                      continue
                  cap = read("capacity")
                  limit = read("charge_control_end_threshold")
                  # This EC exposes charge_behaviour but no end threshold, so
                  # both are optional and the UI hides whichever is absent.
                  raw = read("charge_behaviour")
                  options = raw.split() if raw else []
                  active = next(
                      (o[1:-1] for o in options if o.startswith("[")), None
                  )
                  found.append(
                      {
                          "name": name,
                          "status": read("status") or "unknown",
                          "capacity": int(cap) if cap and cap.isdigit() else None,
                          "charge_limit_supported": limit is not None,
                          "charge_limit": int(limit) if limit and limit.isdigit() else None,
                          "behaviours": [o.strip("[]") for o in options],
                          "behaviour": active,
                      }
                  )
              return found


          def adapter_path():
              for path, ifaces in managed().items():
                  if "org.bluez.Adapter1" in ifaces:
                      return path
              return None


          def state():
              objs = managed()
              adapter = None
              devices = []
              for path, ifaces in objs.items():
                  if "org.bluez.Adapter1" in ifaces and adapter is None:
                      a = ifaces["org.bluez.Adapter1"]
                      adapter = {
                          "path": path,
                          "powered": bool(a.get("Powered", False)),
                          "discovering": bool(a.get("Discovering", False)),
                      }
                  d = ifaces.get("org.bluez.Device1")
                  if d is None:
                      continue
                  uuids = {str(u).lower() for u in d.get("UUIDs", [])}
                  devices.append(
                      {
                          "path": path,
                          "address": str(d.get("Address", "")),
                          "name": str(d.get("Alias", d.get("Name", ""))),
                          "paired": bool(d.get("Paired", False)),
                          "trusted": bool(d.get("Trusted", False)),
                          "connected": bool(d.get("Connected", False)),
                          "audio": bool(uuids & AUDIO_UUIDS),
                          "battery": (
                              int(ifaces["org.bluez.Battery1"]["Percentage"])
                              if "org.bluez.Battery1" in ifaces
                              else None
                          ),
                      }
                  )
              devices.sort(key=lambda d: (not d["connected"], not d["audio"], d["name"].lower()))
              return {
                  "adapter": adapter,
                  "devices": devices,
                  "sinks": sinks(),
                  "combined": combined(),
                  "battery": battery(),
              }


          def act(body):
              op = body.get("op")
              path = body.get("path")
              if op == "power":
                  ap = adapter_path()
                  props(ap).Set("org.bluez.Adapter1", "Powered", dbus.Boolean(body["value"]))
              elif op in ("scan_on", "scan_off"):
                  ap = adapter_path()
                  a = iface(ap, "org.bluez.Adapter1")
                  a.StartDiscovery() if op == "scan_on" else a.StopDiscovery()
              elif op == "pair":
                  iface(path, "org.bluez.Device1").Pair()
                  props(path).Set("org.bluez.Device1", "Trusted", dbus.Boolean(True))
              elif op == "connect":
                  iface(path, "org.bluez.Device1").Connect()
              elif op == "disconnect":
                  iface(path, "org.bluez.Device1").Disconnect()
              elif op == "trust":
                  props(path).Set("org.bluez.Device1", "Trusted", dbus.Boolean(True))
              elif op == "remove":
                  ap = adapter_path()
                  iface(ap, "org.bluez.Adapter1").RemoveDevice(path)
              elif op == "charge_behaviour":
                  target = os.path.join(PSDIR, body["name"], "charge_behaviour")
                  with open(target, "w") as fh:
                      fh.write(str(body["value"]))
              elif op == "charge_limit":
                  value = max(50, min(100, int(body["value"])))
                  target = os.path.join(PSDIR, body["name"], "charge_control_end_threshold")
                  with open(target, "w") as fh:
                      fh.write(str(value))
              elif op == "combine_set":
                  existing = combined()
                  if existing:
                      pactl("unload-module", existing["index"])
                  pactl(
                      "load-module",
                      "module-combine-sink",
                      "sink_name=" + COMBINED,
                      "slaves=" + ",".join(body["sinks"]),
                      "sink_properties=device.description=Combined",
                  )
              elif op == "combine_clear":
                  existing = combined()
                  if existing:
                      pactl("unload-module", existing["index"])
              else:
                  raise ValueError("unknown op " + str(op))


          class Handler(BaseHTTPRequestHandler):
              protocol_version = "HTTP/1.1"

              def log_message(self, fmt, *args):
                  pass

              def _send(self, code, body, ctype):
                  self.send_response(code)
                  self.send_header("Content-Type", ctype)
                  self.send_header("Content-Length", str(len(body)))
                  self.end_headers()
                  self.wfile.write(body)

              def do_GET(self):
                  if self.path.startswith("/api/state"):
                      with lock:
                          payload = json.dumps(state()).encode()
                      self._send(200, payload, "application/json")
                  elif self.path in ("/", "/index.html"):
                      with open(PAGE, "rb") as fh:
                          self._send(200, fh.read(), "text/html; charset=utf-8")
                  else:
                      self._send(404, b"not found", "text/plain")

              def do_POST(self):
                  if self.path != "/api/action":
                      self._send(404, b"not found", "text/plain")
                      return
                  n = int(self.headers.get("Content-Length", "0"))
                  try:
                      body = json.loads(self.rfile.read(n) or b"{}")
                      with lock:
                          act(body)
                  except Exception as exc:  # surfaced in the UI, not swallowed
                      self._send(
                          500, json.dumps({"error": str(exc)}).encode(), "application/json"
                      )
                      return
                  self._send(200, b'{"ok":true}', "application/json")


          def main():
              if not wait_for_bluez():
                  raise SystemExit("bt-console: org.bluez never appeared on the system bus")
              register_agent()
              threading.Thread(target=GLib.MainLoop().run, daemon=True).start()
              ThreadingHTTPServer(LISTEN, Handler).serve_forever()


          main()
        '';
      in
      {
        options.services.btConsole = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Web console for bluetooth pairing, battery and sink merging.";
          };
          address = mkOption {
            type = types.str;
            default = "0.0.0.0";
          };
          port = mkOption {
            type = types.port;
            default = 8088;
          };
          pulseServer = mkOption {
            type = types.str;
            # System-wide pipewire-pulse listens on %t/pulse/native, and %t is
            # /run for a system unit -- not under /run/pipewire.
            default = "unix:/run/pulse/native";
            description = "pipewire-pulse socket `pactl` drives for the combine-sink.";
          };
          openFirewall = mkOption {
            type = types.bool;
            default = false;
            description = "There is no authentication; only do this on a network you trust.";
          };
        };

        config = lib.mkIf cfg.enable {
          systemd.services.bt-console = {
            wantedBy = [ "multi-user.target" ];
            after = [ "bluetooth.service" ];
            wants = [ "bluetooth.service" ];
            path = [
              pkgs.pulseaudio # pactl
            ];
            environment = {
              BTC_ADDRESS = cfg.address;
              BTC_PORT = toString cfg.port;
              BTC_PULSE_SERVER = cfg.pulseServer;
            };
            serviceConfig = {
              # Root: it writes the EC charge threshold under
              # /sys/class/power_supply and owns the bluez pairing agent.
              ExecStart = "${pythonEnv}/bin/python3 ${server}";
              Restart = "on-failure";
              RestartSec = 5;
              ProtectHome = true;
            };
          };

          networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];
        };
      };
  };
}
