# The celler2 den host (../hosts/celler2.nix) as one OCI
# image, built by nix-oci from that host's own NixOS modules. cellerd's unit is
# the entrypoint; the host's container-only preStart decrypts its secrets with
# the mounted ssh key and starts tailscaled and cloudflared next to it.
#
#   nix run .#oci-load-docker-celler2
#   docker run -d --name celler2 --restart unless-stopped \
#     -v ~/.ssh/celler2:/keys/ssh_host_ed25519_key:ro \
#     -v celler2:/var/lib \
#     celler2:latest
#
# First use of the input: `nix run .#write-flake && nix flake lock`.
{
  inputs,
  lib,
  ...
}:
let
  host = "celler2";
in
{
  ff.nix-oci = {
    url = "github:dauliac/nix-oci";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-parts.follows = "flake-parts";
  };

  imports = lib.optional (inputs ? nix-oci) inputs.nix-oci.modules.flake.nix-oci;
}
// lib.optionalAttrs (inputs ? nix-oci) {
  oci.enabled = true;

  perSystem =
    { system, ... }:
    lib.optionalAttrs (system == "x86_64-linux") {
      oci.containers.${host} = {
        isRoot = true;
        nixosConfig = {
          mainService = "cellerd";
          modules = inputs.self.nixosConfigurations.${host}.ociModules;
        };
      };
    };
}
