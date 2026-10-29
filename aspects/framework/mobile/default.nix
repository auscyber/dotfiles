# A `mobile` host class -- den hosts for devices nix cannot build.
#
# Works without patching den, for three non-obvious reasons
# (den nix/lib/entities/host.nix):
#   * `class` is a free string (`strOpt`), not an enum.
#   * `instantiate` is overridable. Setting it explicitly is required, not
#     cosmetic: den's default is `{nixos=..;darwin=..;systemManager=..}.${class}`
#     and would throw on an unknown class.
#   * `intoAttr` picks the flake output, so a phone lands in
#     `mobileConfigurations` -- which ../../tooling/ci-matrix.nix does not know
#     about, so nothing ever tries to `nix build` a phone.
#
# The device's own module tree is `mobile.modules`, one file per concern
# beside this one, the way nixos has its module-list.
{
  lib,
  den,
  config,
  withSystem,
  ...
}:
let
  top = config;
in
{
  # `config.` is forced: a module declaring `options` may not also carry
  # top-level `den`/`flake` attributes.
  #
  # `mobile` is to `ios` what `os` is to `nixos`/`darwin`: aspects write
  # `mobile`, hosts are `ios`, the policy below forwards.
  #
  # A policy rather than `den.batteries.forward` for the reason ../../base/nix.nix
  # documents: a forward reaches only the scope tree it is included in, so
  # content from user-included aspects is silently dropped.
  config.den.classes.mobile.description =
    "Portable mobile-device content, forwarded to the host's concrete class";
  config.den.classes.ios = { };

  config.den.policies.mobile-to-host =
    { host, ... }:
    lib.optional (host ? class && builtins.elem host.class [ "ios" ]) (
      den.lib.policy.route {
        fromClass = "mobile";
        intoClass = host.class;
      }
    );

  config.den.default.includes = [ den.policies.mobile-to-host ];

  options.mobile.modules = lib.mkOption {
    type = lib.types.attrsOf lib.types.deferredModule;
    default = { };
    description = "The device module tree every mobile host is evaluated with.";
  };

  options.flake.mobileConfigurations = lib.mkOption {
    default = { };
    type = lib.types.lazyAttrsOf lib.types.raw;
    description = ''
      Devices declared with `class = "ios"`. Each is an evalModules result
      over the `mobile` class; `.toplevel` is the generation and
      `nix run .#mobileConfigurations.<name>.toplevel` activates it.
    '';
  };

  # Profiles and shortcuts key everything by UUID; deriving them keeps a
  # regenerated file a replacement of the installed one, not a second copy.
  config.den.lib.mobile.uuid =
    seed:
    let
      h = lib.toUpper (builtins.hashString "sha256" seed);
      s = builtins.substring;
    in
    "${s 0 8 h}-${s 8 4 h}-${s 12 4 h}-${s 16 4 h}-${s 20 12 h}";

  config.mobile.modules.base =
    { config, ... }:
    {
      # The device runs nothing nix builds; its generation is built and
      # activated from a Mac, so `pkgs` is that machine's.
      config._module.args.pkgs = withSystem config.build.system ({ pkgs, ... }: pkgs);
    };

  config.flake.lib.mobileSystem =
    {
      name,
      system,
    }:
    # den calls this with { modules; pkgs?; }; den's `pkgs` would be for a
    # system nixpkgs does not have, so it is ignored.
    { modules, ... }:
    let
      evaluated = lib.evalModules {
        specialArgs = { inherit name system den; };
        modules = modules ++ lib.attrValues top.mobile.modules;
      };
    in
    {
      inherit (evaluated) config options extendModules;
      inherit name system;
      inherit (evaluated.config) bundle;
      inherit (evaluated.config.build) toplevel manifest;
    };
}
