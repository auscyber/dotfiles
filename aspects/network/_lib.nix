{ lib }:
let
  # The fleet as plain data, readable from any partition. See its header for
  # why this cannot be `den.hosts`.
  registry = import ../hosts/_registry.nix;

  # 6 hex chars of sha256 → integer in 2..254. Server lives at .1.
  hexToInt = s: (builtins.fromTOML "v=0x${s}").v;
  hostOctet =
    name: 2 + lib.mod (hexToInt (lib.substring 0 6 (builtins.hashString "sha256" name))) 253;
  # `vpn-secrets/` is the scope prefix: the keypair is declared by the
  # `vpn-secrets` aspect, so agenix-rekey's generator writes it (and the `.pub`
  # sidecar) under `<generatedSecretsDir>/vpn-secrets/`.
  pubKeyFile = name: ../../secrets/generated + "/${name}/vpn-secrets/wireguard_key.pub";
  pubKey = name: lib.removeSuffix "\n" (builtins.readFile (pubKeyFile name));

  # A host peers over wireguard iff the generator has produced a host-level
  # keypair for it. Hosts without the vpn aspect have no such file.
  hasKey = name: builtins.pathExists (pubKeyFile name);

  # Names come from the registry rather than `den.hosts` (partition-scoped, so
  # it drops every host in another bucket) and rather than a `readDir` of
  # secrets/generated (partition-agnostic, but it discovers hosts by the side
  # effect of a secret having been generated for them — which is a fact about
  # the secrets tree, not a declaration, and says nothing about a host that
  # has no keypair yet). `hasKey` still gates membership: a host listed here
  # but never `agenix generate`d has no public key to put in a peer entry.
  allHostNames = lib.filter hasKey (lib.attrNames registry);
  clientNames = name: lib.filter (n: n != name) allHostNames;

  # Mirrors `vpn.nix`'s `tunnelIp`, but by name only: a peer's own
  # `cfg.role`/`cfg.ipAddress` aren't visible from here, only its name (same
  # constraint `vpn.nix` documents for `tunnelPeers`). "secondpc" matches
  # `vpnSubmodule`'s `serverHost` default, so this stays equivalent to the live
  # values as long as no host overrides `vpn.ipAddress`.
  tunnelIpByName =
    name: if name == "secondpc" then "10.100.0.1" else "10.100.0.${toString (hostOctet name)}";
in
{
  inherit
    registry
    hexToInt
    hostOctet
    pubKeyFile
    pubKey
    hasKey
    allHostNames
    clientNames
    tunnelIpByName
    ;
}
