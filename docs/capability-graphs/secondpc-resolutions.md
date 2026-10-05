# Capability resolution on `secondpc`

66 takes, from the aspects this host has.

### audiobookshelf takes `gated`

- clauses: `oauthProxy=false, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### auscyber takes `celler`

- clauses: (none)
- routed: no
- giver: `celler` on `secondpc` (reach = fleet, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

### bazarr takes `gated`

- clauses: `clients=(homepage), subpath=/subs`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### bazarr takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### celler takes `proxy`

- clauses: `maxBodyBytes=15000000000, subdomain=cache`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### celler-default takes `celler`

- clauses: (none)
- routed: no
- giver: `celler` on `secondpc` (reach = fleet, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

### donetick takes `gated`

- clauses: `oauthProxy=false, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### donetick takes `postgres`

- clauses: `services=(donetick), tcp=true`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### esphome takes `gated`

- clauses: `websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### esphome takes `home-assistant`

- clauses: `extraComponents=(esphome)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### esphome takes `mqtt`

- clauses: `username=sendspin`
- routed: yes
- giver: `mosquitto` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  mosquitto: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### grafana takes `gated`

- clauses: `websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### grafana takes `postgres`

- clauses: `services=(grafana)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### grocy takes `gated`

- clauses: `forwardUser=true, oauthProxy=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### homeassistant takes `mqtt`

- clauses: (none)
- routed: yes
- giver: `mosquitto` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  mosquitto: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### homeassistant takes `postgres`

- clauses: `services=(home-assistant)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### homepage takes `credential`

- clauses: `description=Dashboard widgets, envPrefix=HOMEPAGE_VAR_, restartUnits=(homepage-dashboard.service)`
- routed: no
- giver: `kanidm` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  kanidm: OK
```

### homepage takes `gated`

- clauses: `forwardUser=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### immich takes `gated`

- clauses: `oauthProxy=false, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### immich takes `postgres`

- clauses: `services=(immich-server,immich-machine-learning)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### lidarr takes `gated`

- clauses: `clients=(prowlarr,homepage), subpath=/music, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### lidarr takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### lidarr takes `tcp`

- clauses: `name=qbittorrent`
- routed: yes
- giver: `qbittorrent` on `secondpc` (reach = host, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  loopback: is named Ivys-MacBook-Pro, want qbittorrent
  loopback: is named auspc, want qbittorrent
  loopback: is named birdsarentreal, want qbittorrent
  loopback: is named celler2, want qbittorrent
  loopback: is named iphone, want qbittorrent
  loopback: is named lora-pi, want qbittorrent
  loopback: is named macmini, want qbittorrent
  loopback: is named pentestvm, want qbittorrent
  loopback: is named secondpc, want qbittorrent
  loopback: is named surfacelaptop, want qbittorrent
  loopback: is named wsl-nixos, want qbittorrent
  container-bridge: is named secondpc, want qbittorrent
  lan-home: is named auspc, want qbittorrent
  lan-home: is named birdsarentreal, want qbittorrent
  lan-home: is named secondpc, want qbittorrent
  tailscale: is named Ivys-MacBook-Pro, want qbittorrent
  tailscale: is named auspc, want qbittorrent
  tailscale: is named birdsarentreal, want qbittorrent
  tailscale: is named celler2, want qbittorrent
  tailscale: is named secondpc, want qbittorrent
  audiobookshelf: is named audiobookshelf, want qbittorrent
  bazarr: is named bazarr, want qbittorrent
  celler: is named secondpc, want qbittorrent
  celler-container: is named celler2, want qbittorrent
  donetick: is named donetick, want qbittorrent
  esphome: is named esphome, want qbittorrent
  garage: is named secondpc, want qbittorrent
  grafana: is named grafana, want qbittorrent
  grocy: is named grocy, want qbittorrent
  homeassistant: is named home-assistant, want qbittorrent
  homepage: is named homepage, want qbittorrent
  immich: is named immich, want qbittorrent
  kanidm: is named kanidm, want qbittorrent
  lidarr: is named lidarr, want qbittorrent
  mail: is named mail, want qbittorrent
  matter-server: is named matter-server, want qbittorrent
  mosquitto: is named mosquitto, want qbittorrent
  music-assistant: is named music-assistant, want qbittorrent
  openrgb: is named openrgb, want qbittorrent
  paperless: is named paperless, want qbittorrent
  postgres: is named postgres, want qbittorrent
  prowlarr: is named prowlarr, want qbittorrent
  qbittorrent: OK
  radarr: is named radarr, want qbittorrent
  sonarr: is named sonarr, want qbittorrent
  splitpro: is named splitpro, want qbittorrent
  tandoor: is named tandoor, want qbittorrent
  tempo: is named tempo, want qbittorrent
  vikunja: is named vikunja, want qbittorrent
  publish-ivymect-in: is named secondpc, want qbittorrent
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### loki takes `proxy`

- clauses: `port=3100, subdomain=logs`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### mail takes `dns`

- clauses: `records=((...),(...),(...),(...),(...),(...),(...),(...),(...),(...),(...))`
- routed: no
- giver: `publish-ivymect-in` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  publish-ivymect-in: OK
```

### mail takes `ldap`

- clauses: `application=mail, applicationGroup=media-users`
- routed: yes
- giver: `kanidm` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  kanidm: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### mail-ddns takes `dns`

- clauses: `dynamic=(mail.ivymect.in)`
- routed: no
- giver: `publish-ivymect-in` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  publish-ivymect-in: OK
```

### matter-server takes `home-assistant`

- clauses: `extraComponents=(matter,thread)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### mosquitto takes `home-assistant`

- clauses: `extraComponents=(mqtt)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### music-assistant takes `gated`

- clauses: `port=8095, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### music-assistant takes `home-assistant`

- clauses: `extraComponents=(music_assistant)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### music-assistant takes `mqtt`

- clauses: (none)
- routed: yes
- giver: `mosquitto` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  mosquitto: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### navidrome takes `proxy`

- clauses: `port=4533, subdomain=music`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### nextcloud takes `postgres`

- clauses: `services=(nextcloud-setup)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### nextcloud takes `proxy`

- clauses: `maxBodyBytes=10000000000, subdomain=cloud`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### nextcloud takes `smtp`

- clauses: (none)
- routed: yes
- giver: `mail` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  mail: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### paperless takes `gated`

- clauses: `forwardUser=true, maxBodyBytes=50000000, oauthProxy=true, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### paperless takes `postgres`

- clauses: `services=(paperless-scheduler,paperless-task-queue,paperless-consumer,paperless-web)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### paperwork takes `share`

- clauses: `displayName=Paperwork, permissions=(read,write,share,delete)`
- routed: no
- giver: `shares` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  shares: OK
```

### photos takes `share`

- clauses: `displayName=Photos, quota=200G`
- routed: no
- giver: `shares` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  shares: OK
```

### postgres takes `unix`

- clauses: (none)
- routed: no
- giver: `loopback` on `Ivys-MacBook-Pro` (reach = host, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
  loopback: OK
```

### prowlarr takes `credential`

- clauses: `envPrefix=PROWLARR_, restartUnits=(prowlarr-connect.service)`
- routed: no
- giver: `kanidm` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  kanidm: OK
```

### prowlarr takes `gated`

- clauses: `clients=(homepage), subpath=/indexers, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### prowlarr takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### qbittorrent takes `gated`

- clauses: `banner=Read-only - qBittorrent's settings are declared in Nix; changes made here are discarded, clients=(sonarr,radarr,lidarr)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### qbittorrent takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### radarr takes `gated`

- clauses: `clients=(prowlarr,seerr,homepage), subpath=/movies, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### radarr takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### radarr takes `tcp`

- clauses: `name=qbittorrent`
- routed: yes
- giver: `qbittorrent` on `secondpc` (reach = host, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  loopback: is named Ivys-MacBook-Pro, want qbittorrent
  loopback: is named auspc, want qbittorrent
  loopback: is named birdsarentreal, want qbittorrent
  loopback: is named celler2, want qbittorrent
  loopback: is named iphone, want qbittorrent
  loopback: is named lora-pi, want qbittorrent
  loopback: is named macmini, want qbittorrent
  loopback: is named pentestvm, want qbittorrent
  loopback: is named secondpc, want qbittorrent
  loopback: is named surfacelaptop, want qbittorrent
  loopback: is named wsl-nixos, want qbittorrent
  container-bridge: is named secondpc, want qbittorrent
  lan-home: is named auspc, want qbittorrent
  lan-home: is named birdsarentreal, want qbittorrent
  lan-home: is named secondpc, want qbittorrent
  tailscale: is named Ivys-MacBook-Pro, want qbittorrent
  tailscale: is named auspc, want qbittorrent
  tailscale: is named birdsarentreal, want qbittorrent
  tailscale: is named celler2, want qbittorrent
  tailscale: is named secondpc, want qbittorrent
  audiobookshelf: is named audiobookshelf, want qbittorrent
  bazarr: is named bazarr, want qbittorrent
  celler: is named secondpc, want qbittorrent
  celler-container: is named celler2, want qbittorrent
  donetick: is named donetick, want qbittorrent
  esphome: is named esphome, want qbittorrent
  garage: is named secondpc, want qbittorrent
  grafana: is named grafana, want qbittorrent
  grocy: is named grocy, want qbittorrent
  homeassistant: is named home-assistant, want qbittorrent
  homepage: is named homepage, want qbittorrent
  immich: is named immich, want qbittorrent
  kanidm: is named kanidm, want qbittorrent
  lidarr: is named lidarr, want qbittorrent
  mail: is named mail, want qbittorrent
  matter-server: is named matter-server, want qbittorrent
  mosquitto: is named mosquitto, want qbittorrent
  music-assistant: is named music-assistant, want qbittorrent
  openrgb: is named openrgb, want qbittorrent
  paperless: is named paperless, want qbittorrent
  postgres: is named postgres, want qbittorrent
  prowlarr: is named prowlarr, want qbittorrent
  qbittorrent: OK
  radarr: is named radarr, want qbittorrent
  sonarr: is named sonarr, want qbittorrent
  splitpro: is named splitpro, want qbittorrent
  tandoor: is named tandoor, want qbittorrent
  tempo: is named tempo, want qbittorrent
  vikunja: is named vikunja, want qbittorrent
  publish-ivymect-in: is named secondpc, want qbittorrent
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### samba-user takes `samba-user`

- clauses: (none)
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

### seerr takes `credential`

- clauses: `description=Request frontend, talks to sonarr and radarr`
- routed: no
- giver: `kanidm` on `secondpc` (reach = household, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  kanidm: OK
```

### seerr takes `proxy`

- clauses: `port=5055, subdomain=requests`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### sonarr takes `gated`

- clauses: `clients=(prowlarr,seerr,homepage), subpath=/tv, websockets=true`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### sonarr takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### sonarr takes `tcp`

- clauses: `name=qbittorrent`
- routed: yes
- giver: `qbittorrent` on `secondpc` (reach = host, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  loopback: is named Ivys-MacBook-Pro, want qbittorrent
  loopback: is named auspc, want qbittorrent
  loopback: is named birdsarentreal, want qbittorrent
  loopback: is named celler2, want qbittorrent
  loopback: is named iphone, want qbittorrent
  loopback: is named lora-pi, want qbittorrent
  loopback: is named macmini, want qbittorrent
  loopback: is named pentestvm, want qbittorrent
  loopback: is named secondpc, want qbittorrent
  loopback: is named surfacelaptop, want qbittorrent
  loopback: is named wsl-nixos, want qbittorrent
  container-bridge: is named secondpc, want qbittorrent
  lan-home: is named auspc, want qbittorrent
  lan-home: is named birdsarentreal, want qbittorrent
  lan-home: is named secondpc, want qbittorrent
  tailscale: is named Ivys-MacBook-Pro, want qbittorrent
  tailscale: is named auspc, want qbittorrent
  tailscale: is named birdsarentreal, want qbittorrent
  tailscale: is named celler2, want qbittorrent
  tailscale: is named secondpc, want qbittorrent
  audiobookshelf: is named audiobookshelf, want qbittorrent
  bazarr: is named bazarr, want qbittorrent
  celler: is named secondpc, want qbittorrent
  celler-container: is named celler2, want qbittorrent
  donetick: is named donetick, want qbittorrent
  esphome: is named esphome, want qbittorrent
  garage: is named secondpc, want qbittorrent
  grafana: is named grafana, want qbittorrent
  grocy: is named grocy, want qbittorrent
  homeassistant: is named home-assistant, want qbittorrent
  homepage: is named homepage, want qbittorrent
  immich: is named immich, want qbittorrent
  kanidm: is named kanidm, want qbittorrent
  lidarr: is named lidarr, want qbittorrent
  mail: is named mail, want qbittorrent
  matter-server: is named matter-server, want qbittorrent
  mosquitto: is named mosquitto, want qbittorrent
  music-assistant: is named music-assistant, want qbittorrent
  openrgb: is named openrgb, want qbittorrent
  paperless: is named paperless, want qbittorrent
  postgres: is named postgres, want qbittorrent
  prowlarr: is named prowlarr, want qbittorrent
  qbittorrent: OK
  radarr: is named radarr, want qbittorrent
  sonarr: is named sonarr, want qbittorrent
  splitpro: is named splitpro, want qbittorrent
  tandoor: is named tandoor, want qbittorrent
  tempo: is named tempo, want qbittorrent
  vikunja: is named vikunja, want qbittorrent
  publish-ivymect-in: is named secondpc, want qbittorrent
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### splitpro takes `gated`

- clauses: `oauthProxy=false`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### splitpro takes `postgres`

- clauses: `extensions=(pg_cron), fromContainer=true, preload=(pg_cron), services=(docker-splitpro), settings=(...), tcp=true`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### tandoor takes `gated`

- clauses: `maxBodyBytes=67108864, oauthProxy=false`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### tandoor takes `postgres`

- clauses: `services=(tandoor-recipes)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```

### tautulli takes `gated`

- clauses: `port=8181`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### tempo takes `gated`

- clauses: (none)
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### tempo takes `nginx`

- clauses: `api=(...)`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### vaultwarden takes `proxy`

- clauses: `port=8222, subdomain=bitwarden`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
  proxy-nginx: OK
```

### vaultwarden takes `smtp`

- clauses: `fromName=ivymect.in Bitwarden server, localPart=bitwarden`
- routed: yes
- giver: `mail` on `secondpc` (reach = household, same host)
- carrier: `loopback` at preference 0, address 127.0.0.1

WHO (which giver):

```
  mail: OK
```

HOW (which carrier):

```
  loopback: OK
  container-bridge: OK
  lan-home: OK
  tailscale: OK
  audiobookshelf: OK
  bazarr: OK
  celler: OK
  donetick: OK
  esphome: OK
  garage: OK
  grafana: OK
  grocy: OK
  homeassistant: OK
  homepage: OK
  immich: OK
  kanidm: OK
  lidarr: OK
  mail: OK
  matter-server: OK
  mosquitto: OK
  music-assistant: OK
  paperless: OK
  postgres: OK
  prowlarr: OK
  qbittorrent: OK
  radarr: OK
  sonarr: OK
  splitpro: OK
  tandoor: OK
  tempo: OK
  vikunja: OK
  publish-ivymect-in: OK
```

### vikunja takes `gated`

- clauses: `oauthProxy=false`
- routed: no
- giver: `gated-oauth2-proxy` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  gated-oauth2-proxy: OK
```

### vikunja takes `postgres`

- clauses: `services=(vikunja)`
- routed: no
- giver: `postgres` on `secondpc` (reach = host, same host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  postgres: OK
```
