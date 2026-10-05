# Capability resolution on `auspc`

10 takes, from the aspects this host has.

### auscyber takes `celler`

- clauses: (none)
- routed: no
- giver: `celler` on `secondpc` (reach = fleet, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

### celler-cache-pull takes `http`

- clauses: (none)
- routed: yes
- giver: `celler` on `secondpc` (reach = fleet, cross-host)
- carrier: `lan-home` at preference 20, address 192.168.0.26

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

HOW (which carrier):

```
  lan-home: OK
  tailscale: OK
  publish-ivymect-in: OK
```

### celler-default takes `celler`

- clauses: (none)
- routed: no
- giver: `celler` on `secondpc` (reach = fleet, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  celler: OK
  celler-container: OK
```

### corsair-psu takes `home-assistant`

- clauses: `extraComponents=(mqtt)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### corsair-psu takes `mqtt`

- clauses: `acl=(write homeassistant/sensor/#,write sensors/#)`
- routed: yes
- giver: `mosquitto` on `secondpc` (reach = household, cross-host)
- carrier: `lan-home` at preference 20, address 192.168.0.26

WHO (which giver):

```
  mosquitto: OK
```

HOW (which carrier):

```
  lan-home: OK
  tailscale: OK
  publish-ivymect-in: OK
```

### openrgb takes `home-assistant`

- clauses: `extraComponents=(openrgb), listensOn=(...)`
- routed: no
- giver: `homeassistant` on `secondpc` (reach = household, cross-host)
- carrier: none (not a routed capability)

WHO (which giver):

```
  homeassistant: OK
```

### paperwork takes `share`

- clauses: `displayName=Paperwork, permissions=(read,write,share,delete)`
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

OUT OF REACH: shares on secondpc (reach = host)

### photos takes `share`

- clauses: `displayName=Photos, quota=200G`
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

OUT OF REACH: shares on secondpc (reach = host)

### samba-user takes `samba-user`

- clauses: (none)
- routed: no
- **unresolved**
- carrier: none (not a routed capability)

### vban-turntable takes `udp`

- clauses: `mtu=<fn>, name=music-assistant`
- routed: yes
- giver: `music-assistant` on `secondpc` (reach = household, cross-host)
- carrier: `lan-home` at preference 20, address 192.168.0.26

WHO (which giver):

```
  loopback: is named Ivys-MacBook-Pro, want music-assistant
  loopback: is named auspc, want music-assistant
  loopback: is named birdsarentreal, want music-assistant
  loopback: is named celler2, want music-assistant
  loopback: is named iphone, want music-assistant
  loopback: is named lora-pi, want music-assistant
  loopback: is named macmini, want music-assistant
  loopback: is named pentestvm, want music-assistant
  loopback: is named secondpc, want music-assistant
  loopback: is named surfacelaptop, want music-assistant
  loopback: is named wsl-nixos, want music-assistant
  lan-home: is named auspc, want music-assistant
  lan-home: is named birdsarentreal, want music-assistant
  lan-home: is named secondpc, want music-assistant
  tailscale: is named Ivys-MacBook-Pro, want music-assistant
  tailscale: is named auspc, want music-assistant
  tailscale: is named birdsarentreal, want music-assistant
  tailscale: is named celler2, want music-assistant
  tailscale: is named secondpc, want music-assistant
  homeassistant: is named secondpc, want music-assistant
  house-wifi: is named auspc, want music-assistant
  music-assistant: OK
```

HOW (which carrier):

```
  lan-home: OK
  tailscale: mtu is 1280, which this flow refuses
```
