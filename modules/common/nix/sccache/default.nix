{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.auscybernix.nix.sccache;
  envVars = lib.mapAttrs' (key: value: {
    name = "SCCACHE_${key}";
    value = "${builtins.toString value}";
  }) cfg.env;
in
{
  options.auscybernix.nix.sccache = {
    enable = lib.mkEnableOption "sccache";
    cacheDir = lib.mkOption {
      type = lib.types.path;
      description = "SCCache directory";
      default = "/var/cache/sccache";
    };
    env = lib.mkOption {
      type = lib.types.attrsOf (lib.types.anything);
      default = {
        COMPRESS = 1;
        MAX_SIZE = "10G";
        MAXSIZE = "10G";

      };
      description = "Env";
    };
    packageNames = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "Nix top-level packages to be compiled using CCache";
      default = [ ];
      example = [
        "wxwidgets_3_2"
        "ffmpeg"
        "libav_all"
      ];
    };
    owner = lib.mkOption {
      type = lib.types.str;
      default = "root";
      description = "Owner of SCCache directory";
    };
    group = lib.mkOption {
      type = lib.types.str;
      default = "nixbld";
      description = "Group owner of SCCache directory";
    };
    trace = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Trace ccache usage to see which derivations use ccache";
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf (cfg.packageNames != [ ]) {
        nixpkgs.overlays = [
          (
            self: super:
            lib.genAttrs cfg.packageNames (
              pn:
              super.${pn}.override {
                stdenv =
                  if cfg.trace then builtins.trace "with sccache: ${pn}" self.sccacheStdenv else self.sccacheStdenv;
              }
            )
          )
        ];
      })

      ({
        #    programs.ccache.enable = true;
        nix.settings.extra-sandbox-paths = [ config.auscybernix.nix.sccache.cacheDir ];
        system.activationScripts.nix-ccache.text = lib.mkAfter ''
          	  mkdir -p ${cfg.cacheDir}
          	  chown ${cfg.owner}:${cfg.group} ${cfg.cacheDir}
          	  chmod 0770 ${cfg.cacheDir}
          	  '';
        nixpkgs.overlays = [
          (self: super: {
            sccacheWrapper = super.ccacheWrapper.override {
              extraConfig = ''

                                        export SCCACHE_COMPRESS=1
                                        export SCCACHE_DIR="${cfg.cacheDir}"
                						${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${v}") envVars)}
                            			export CCACHE_SLOPPINESS=random_seed
                                        export SCCACHE_UMASK=007
                                        if [ ! -d "$SCCACHE_DIR" ]; then
                                          echo "====="
                                          echo "Directory '$SCCACHE_DIR' does not exist"
                                          echo "Please create it with:"
                                          echo "  sudo mkdir -m0770 '$SCCACHE_DIR'"
                                          echo "  sudo chown root:nixbld '$SCCACHE_DIR'"
                                          echo "====="
                                          exit 1
                                        fi
                                        if [ ! -w "$SCCACHE_DIR" ]; then
                                          echo "====="
                                          echo "Directory '$SCCACHE_DIR' is not accessible for user $(whoami)"
                                          echo "Please verify its access permissions"
                                          echo "====="
                                          exit 1
                                        fi
              '';
            };
          })
        ];
      }
      #    // lib.optionalAttrs pkgs.stdenv.isLinux {
      #      security.wrappers.nix-ccache = {
      #        inherit (cfg) owner group;
      #        setuid = false;
      #        setgid = true;
      #        source =
      #
      #          pkgs.writeScript "nix-ccache.pl" ''
      #            #!${pkgs.perl}/bin/perl
      #            use JSON::PP;
      #            my $json_script = q|${builtins.toJSON envVars}|;
      #            my $extra_env=decode_json ($json_script);
      #
      #            %ENV=( SCCACHE_DIR => '${cfg.cacheDir}', %{$extra_env} );
      #            sub untaint {
      #              my $v = shift;
      #              return '-C' if $v eq '-C' || $v eq '--clear';
      #              return '-V' if $v eq '-V' || $v eq '--version';
      #              return '-s' if $v eq '-s' || $v eq '--show-stats';
      #              return '-z' if $v eq '-z' || $v eq '--zero-stats';
      #              exec('${pkgs.sccache}/bin/sccache', '-h');
      #            }
      #            exec('${pkgs.sccache}/bin/sccache', map { untaint $_ } @ARGV);
      #          '';
      #      };
      #    }
      )
    ]
  );
}
