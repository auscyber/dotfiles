{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.auscybernix.nix.ccache;
  envVars = lib.mapAttrs' (key: value: {
    name = "CCACHE_${key}";
    value = "${builtins.toString value}";
  }) cfg.env;
in
{
  options.auscybernix.nix.ccache = {
    enable = lib.mkEnableOption "ccache";
    cacheDir = lib.mkOption {
      type = lib.types.path;
      description = "CCache directory";
      default = "/var/cache/ccache";
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
    owner = lib.mkOption {
      type = lib.types.str;
      default = "root";
      description = "Owner of CCache directory";
    };
    group = lib.mkOption {
      type = lib.types.str;
      default = "nixbld";
      description = "Group owner of CCache directory";
    };
    trace = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Trace ccache usage to see which derivations use ccache";
    };
  };
  config = lib.mkIf cfg.enable {
    #    programs.ccache.enable = true;
    nix.settings.extra-sandbox-paths = [ config.programs.ccache.cacheDir ];
    systemd.tmpfiles.rules = [ "d ${cfg.cacheDir} 0770 ${cfg.owner} ${cfg.group} -" ];
    security.wrappers.nix-ccache = {
      inherit (cfg) owner group;
      setuid = false;
      setgid = true;
      source =
        let
          perl = pkgs.perl.withPackages (p: [ p.JSON ]);
        in

        pkgs.writeScript "nix-ccache.pl" ''
          #!${perl}/bin/perl
          use JSON::PP;
          my $json_script = q|${builtins.toJSON envVars}|;
          my $extra_env=decode_json ($json_script);

          %ENV=( CCACHE_DIR => '${cfg.cacheDir}', %{$extra_env} );
          sub untaint {
            my $v = shift;
            return '-C' if $v eq '-C' || $v eq '--clear';
            return '-V' if $v eq '-V' || $v eq '--version';
            return '-s' if $v eq '-s' || $v eq '--show-stats';
            return '-z' if $v eq '-z' || $v eq '--zero-stats';
            exec('${pkgs.ccache}/bin/ccache', '-h');
          }
          exec('${pkgs.ccache}/bin/ccache', map { untaint $_ } @ARGV);
        '';
    };
    nixpkgs.overlays = [
      (self: super: {
        ccacheWrapper = super.ccacheWrapper.override {
          extraConfig = ''

                                    export CCACHE_COMPRESS=1
                                    export CCACHE_DIR="${config.programs.ccache.cacheDir}"
            						${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${v}") envVars)}
                        			export CCACHE_SLOPPINESS=random_seed
                                    export CCACHE_UMASK=007
                                    if [ ! -d "$CCACHE_DIR" ]; then
                                      echo "====="
                                      echo "Directory '$CCACHE_DIR' does not exist"
                                      echo "Please create it with:"
                                      echo "  sudo mkdir -m0770 '$CCACHE_DIR'"
                                      echo "  sudo chown root:nixbld '$CCACHE_DIR'"
                                      echo "====="
                                      exit 1
                                    fi
                                    if [ ! -w "$CCACHE_DIR" ]; then
                                      echo "====="
                                      echo "Directory '$CCACHE_DIR' is not accessible for user $(whoami)"
                                      echo "Please verify its access permissions"
                                      echo "====="
                                      exit 1
                                    fi
          '';
        };
      })
    ];
  };
}
