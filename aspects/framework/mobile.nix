# A `mobile` host class -- den hosts for devices nix cannot build.
#
# Works without patching den, for three non-obvious reasons
# (den nix/lib/entities/host.nix):
#   * `class` is a free string (`strOpt`), not an enum.
#   * `instantiate` is overridable. Setting it explicitly is required, not
#     cosmetic: den's default is `{nixos=..;darwin=..;systemManager=..}.${class}`
#     and would throw on an unknown class.
#   * `intoAttr` picks the flake output, so a phone lands in
#     `mobileConfigurations` -- which ../tooling/ci-matrix.nix does not know
#     about, so nothing ever tries to `nix build` a phone.
#
# `system` is a label here; nothing asks nixpkgs to instantiate `aarch64-ios`,
# and the `pkgs` den passes to `instantiate` is ignored (lazy, so never forced).
{ lib, den, ... }:
{
  # `config.` is forced: a module declaring `options` may not also carry
  # top-level `den`/`flake` attributes.
  #
  # `mobile` is to `ios` what `os` is to `nixos`/`darwin`: aspects write
  # `mobile`, hosts are `ios`, the policy below forwards. Android would be a
  # second concrete class and no change here.
  #
  # A policy rather than `den.batteries.forward` for the reason ../base/nix.nix
  # documents: a forward reaches only the scope tree it is included in, so
  # content from user-included aspects is silently dropped.
  config.den.classes.mobile.description = "Portable mobile-device content, forwarded to the host's concrete class";
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

  # flake-parts needs the option before anything may write the output.
  options.flake.mobileConfigurations = lib.mkOption {
    default = { };
    type = lib.types.lazyAttrsOf lib.types.raw;
    description = ''
      Devices declared with `class = "mobile"`. Each is an evalModules result
      over the `mobile` class, not a system closure: `.config` is the device's
      settings and `.bundle` is a readable summary of how to enrol it.
    '';
  };

  # `evalModules`, not `nixosSystem`: keeps typed options and merging, drops
  # stdenv/pkgs/activation -- which is the part a phone has no use for.
  config.flake.lib.mobileSystem =
    {
      name,
      system,
    }:
    # den calls this with { modules; pkgs?; }; `pkgs` is ignored on purpose.
    {
      modules,
      ...
    }:
    let
      evaluated = lib.evalModules {
        specialArgs = { inherit name system den; };
        modules = modules ++ [
          (
            { config, ... }:
            {
              options = {
                # den gives every host `den.batteries.hostname`, which writes
                # `networking.hostName` into the `os` class -- and `os` forwards
                # into whatever the host's class is, mobile included. Declaring
                # it is cheaper than excluding the battery, and a device does
                # have a hostname (it is the name it takes on the tailnet).
                networking.hostName = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                  description = "The device's hostname, as seen on the tailnet.";
                };

                # den ships `insecure-predicate` and `unfree-predicate` as `os`
                # aspects on every host, and `os` forwards into whatever the
                # host's class is -- so both land here writing `nixpkgs.config`.
                # Declared, not freeform: a freeform `config` made the modules
                # that READ config self-referential and the whole thing hit
                # infinite recursion.
                nixpkgs = lib.mkOption {
                  type = lib.types.attrsOf lib.types.raw;
                  default = { };
                  description = "Inert on a device; den's predicate aspects write here.";
                };

                device.description = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                  description = "How this device is described in a picker, and on the tailnet.";
                };

                # Three different names, deliberately. The den host name
                # (`iphone`) is a nix identifier; `networking.hostName` is what
                # the tailnet calls it; this is what the device calls itself,
                # which is what devicectl and Finder match on and is usually
                # something like "Ivy's iPhone" -- apostrophe and all.
                device.deviceName = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                  description = "The device's own name, as devicectl reports it.";
                };

                device.tailscale.enable = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = ''
                    Whether this device joins the tailnet. Enrolment is by a
                    single-use key minted on demand -- see
                    `.#tailscale-client-key` -- so there is nothing stored per
                    device and nothing to deploy.
                  '';
                };

                device.notes = lib.mkOption {
                  type = lib.types.listOf lib.types.str;
                  default = [ ];
                  description = "Lines shown by `.bundle`: what a human has to do by hand.";
                };

                # Native apps, installed with `xcrun devicectl`, which takes a
                # `.app` bundle only -- an `.ipa` is unwrapped to its
                # `Payload/*.app` first. The bundle must be signed with a
                # profile listing this device; signing is the caller's problem.
                device.apps = lib.mkOption {
                  default = { };
                  type = lib.types.attrsOf (
                    lib.types.submodule (
                      { name, ... }:
                      {
                        options = {
                          label = lib.mkOption {
                            type = lib.types.str;
                            default = name;
                          };
                          source = lib.mkOption { type = lib.types.either lib.types.path lib.types.package; };
                          bundleId = lib.mkOption {
                            type = lib.types.nullOr lib.types.str;
                            default = null;
                          };
                          launch = lib.mkOption {
                            type = lib.types.bool;
                            default = false;
                          };
                        };
                      }
                    )
                  );
                };

                # Home-screen web clips, shipped as an Apple configuration
                # profile. `tailnetPath` resolves against the serving host's
                # MagicDNS name at run time; the tailnet domain is not knowable
                # at eval time.
                device.webClips = lib.mkOption {
                  default = { };
                  type = lib.types.attrsOf (
                    lib.types.submodule (
                      { name, ... }:
                      {
                        options = {
                          label = lib.mkOption {
                            type = lib.types.str;
                            default = name;
                          };
                          url = lib.mkOption {
                            type = lib.types.nullOr lib.types.str;
                            default = null;
                          };
                          tailnetPath = lib.mkOption {
                            type = lib.types.nullOr lib.types.str;
                            default = null;
                          };
                          servedBy = lib.mkOption {
                            type = lib.types.nullOr lib.types.str;
                            default = null;
                          };
                          fullScreen = lib.mkOption {
                            type = lib.types.bool;
                            default = true;
                          };
                          removable = lib.mkOption {
                            type = lib.types.bool;
                            default = true;
                          };
                        };
                      }
                    )
                  );
                };

                device.manifest = lib.mkOption {
                  type = lib.types.str;
                  internal = true;
                };

                bundle = lib.mkOption {
                  type = lib.types.raw;
                  internal = true;
                  description = "Readable summary of how to enrol this device.";
                };
              };

              # Text, not a derivation: a derivation needs a package set for a
              # system that does not exist. Read with `nix eval --raw`.
              config.bundle = ''
                ${config.device.description} (${name}, ${system})

                ${lib.concatStringsSep "\n" (map (n: "  - ${n}") config.device.notes)}
              '';

              config.device.manifest = builtins.toJSON {
                inherit name;
                hostname = config.networking.hostName;
                inherit (config.device) deviceName;
                apps = lib.mapAttrs (_: a: {
                  inherit (a) label bundleId launch;
                  source = toString a.source;
                }) config.device.apps;
                webClips = lib.mapAttrs (_: w: {
                  inherit (w)
                    label
                    url
                    tailnetPath
                    servedBy
                    fullScreen
                    removable
                    ;
                }) config.device.webClips;
              };
            }
          )
        ];
      };
    in
    {
      inherit (evaluated) config options;
      inherit name system;
      inherit (evaluated.config) bundle;
      inherit (evaluated.config.device) manifest;
    };
}
