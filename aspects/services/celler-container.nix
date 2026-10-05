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

        # ── TOP-LEVEL `mainService`, NOT `nixosConfig.mainService` ──
        #
        # nix-oci declares both, and its entrypoint check reads the top-level
        # one through `config.mainService or nixosCfg.mainService or null`
        # (modules/oci/containers/nixosConfig/integration.nix:23). `config
        # .mainService` is a declared option with `default = null`, so it
        # EXISTS, `or` never fires, and the nested value is never consulted --
        # ../../CLAUDE.md's first trap, in someone else's tree. Setting only
        # the nested one therefore failed the check with a message naming the
        # option that was already set:
        #
        #   Container "celler2": no entrypoint defined. ... None of the
        #   following are set: ... `nixosConfig.mainService`
        #
        # The top-level option's own description is the one that matches this
        # setup: "Requires `nixosConfig.modules` to include the service
        # configuration."
        mainService = "cellerd";

        nixosConfig = {
          # nix-oci's base image sets `system.stateVersion = "25.05"` as a
          # plain definition (modules/_nixos-oci/base.nix), and this fleet's
          # ../framework/den.nix sets "23.11". Both arrive at the same priority
          # in this evaluation, so the module system refuses them rather than
          # letting the `mkDefault` lose -- measured, not assumed; why the
          # priority does not survive into `ociModules` is not established, and
          # the 58 entries are mostly functions so it is not greppable from
          # here.
          #
          # The fleet's value wins because ../hosts/celler2.nix's modules were
          # written under it. A container built on nix-oci's base with 23.11 is
          # the same choice every other host in this tree already makes.
          modules = inputs.self.nixosConfigurations.${host}.ociModules ++ [
            { system.stateVersion = lib.mkForce "23.11"; }
          ];
        };
      };
    };
}
