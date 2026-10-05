# Capability resolution on `auspc` -- `vban-turntable.udp`

1 take, from the aspects this host has.

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
