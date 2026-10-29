{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.profile =
    {
      config,
      name,
      den,
      pkgs,
      ...
    }:
    let
      cfg = config.profile;

      # A password is never a string: it names a declared secret, filled in
      # at activation, so no plaintext reaches the store.
      secretRef = types.submodule {
        options.secret = mkOption { type = types.enum (lib.attrNames config.secrets); };
      };
      secretField = mkOption { type = secretRef; };
      renderSecret = ref: "@secret:${ref.secret}@";

      # `null` host is whichever Mac runs the activation.
      tailnetUrl = types.submodule {
        options.tailnet = mkOption {
          type = types.nullOr types.str;
          default = null;
        };
        options.path = mkOption {
          type = types.strMatching "/.*";
          default = "/";
        };
      };
      renderUrl =
        u:
        if builtins.isString u then
          u
        else if u.tailnet == null then
          "https://@SELF@${u.path}"
        else
          "https://${u.tailnet}.@TAILNET@${u.path}";

      server =
        defaultPort:
        types.submodule {
          options = {
            host = mkOption { type = types.str; };
            port = mkOption {
              type = types.port;
              default = defaultPort;
            };
            ssl = mkOption {
              type = types.bool;
              default = true;
            };
            username = mkOption { type = types.str; };
            password = secretField;
          };
        };

      dav = types.submodule {
        options = {
          description = mkOption { type = types.str; };
          host = mkOption { type = types.str; };
          port = mkOption {
            type = types.port;
            default = 443;
          };
          principalUrl = mkOption {
            type = types.nullOr types.str;
            default = null;
          };
          username = mkOption { type = types.str; };
          password = secretField;
        };
      };

      payloadType = types.attrTag {
        webClip = mkOption {
          type = types.submodule (
            { name, ... }:
            {
              options = {
                label = mkOption {
                  type = types.str;
                  default = name;
                };
                url = mkOption { type = types.either types.str tailnetUrl; };
                fullScreen = mkOption {
                  type = types.bool;
                  default = true;
                };
                removable = mkOption {
                  type = types.bool;
                  default = true;
                };
              };
            }
          );
        };
        caldav = mkOption { type = dav; };
        carddav = mkOption { type = dav; };
        mail = mkOption {
          type = types.submodule {
            options = {
              description = mkOption { type = types.str; };
              address = mkOption { type = types.strMatching "[^@]+@[^@]+"; };
              displayName = mkOption {
                type = types.nullOr types.str;
                default = null;
              };
              imap = mkOption { type = server 993; };
              smtp = mkOption { type = server 465; };
            };
          };
        };
        wifi = mkOption {
          type = types.submodule {
            options = {
              ssid = mkOption { type = types.str; };
              security = mkOption {
                type = types.enum [
                  "WPA2"
                  "WPA3"
                  "Any"
                ];
                default = "WPA2";
              };
              password = secretField;
              hidden = mkOption {
                type = types.bool;
                default = false;
              };
              autoJoin = mkOption {
                type = types.bool;
                default = true;
              };
            };
          };
        };
        dns = mkOption {
          type = types.submodule {
            options = {
              protocol = mkOption {
                type = types.enum [
                  "HTTPS"
                  "TLS"
                ];
              };
              server = mkOption {
                type = types.str;
                description = "A DoH URL for HTTPS, a server name for TLS.";
              };
              addresses = mkOption {
                type = types.listOf types.str;
                default = [ ];
              };
            };
          };
        };
        raw = mkOption {
          type = types.submodule {
            options.type = mkOption { type = types.strMatching "com\\.apple\\..+"; };
            options.content = mkOption { type = types.attrsOf types.anything; };
          };
        };
      };

      render =
        key: p:
        let
          tag = lib.head (lib.attrNames p);
          v = p.${tag};
        in
        {
          webClip = {
            PayloadType = "com.apple.webClip.managed";
            PayloadDisplayName = v.label;
            URL = renderUrl v.url;
            Label = v.label;
            IsRemovable = v.removable;
            FullScreen = v.fullScreen;
          };
          caldav = {
            PayloadType = "com.apple.caldav.account";
            PayloadDisplayName = v.description;
            CalDAVAccountDescription = v.description;
            CalDAVHostName = v.host;
            CalDAVPort = v.port;
            CalDAVUseSSL = true;
            CalDAVUsername = v.username;
            CalDAVPassword = renderSecret v.password;
          }
          // lib.optionalAttrs (v.principalUrl != null) { CalDAVPrincipalURL = v.principalUrl; };
          carddav = {
            PayloadType = "com.apple.carddav.account";
            PayloadDisplayName = v.description;
            CardDAVAccountDescription = v.description;
            CardDAVHostName = v.host;
            CardDAVPort = v.port;
            CardDAVUseSSL = true;
            CardDAVUsername = v.username;
            CardDAVPassword = renderSecret v.password;
          }
          // lib.optionalAttrs (v.principalUrl != null) { CardDAVPrincipalURL = v.principalUrl; };
          mail = {
            PayloadType = "com.apple.mail.managed";
            PayloadDisplayName = v.description;
            EmailAccountDescription = v.description;
            EmailAccountType = "EmailTypeIMAP";
            EmailAddress = v.address;
            IncomingMailServerAuthentication = "EmailAuthPassword";
            IncomingMailServerHostName = v.imap.host;
            IncomingMailServerPortNumber = v.imap.port;
            IncomingMailServerUseSSL = v.imap.ssl;
            IncomingMailServerUsername = v.imap.username;
            IncomingPassword = renderSecret v.imap.password;
            OutgoingMailServerAuthentication = "EmailAuthPassword";
            OutgoingMailServerHostName = v.smtp.host;
            OutgoingMailServerPortNumber = v.smtp.port;
            OutgoingMailServerUseSSL = v.smtp.ssl;
            OutgoingMailServerUsername = v.smtp.username;
            OutgoingPassword = renderSecret v.smtp.password;
          }
          // lib.optionalAttrs (v.displayName != null) { EmailAccountName = v.displayName; };
          wifi = {
            PayloadType = "com.apple.wifi.managed";
            PayloadDisplayName = v.ssid;
            SSID_STR = v.ssid;
            HIDDEN_NETWORK = v.hidden;
            AutoJoin = v.autoJoin;
            EncryptionType = v.security;
            Password = renderSecret v.password;
          };
          dns = {
            PayloadType = "com.apple.dnsSettings.managed";
            PayloadDisplayName = "DNS (${v.server})";
            DNSSettings = {
              DNSProtocol = v.protocol;
              ${if v.protocol == "HTTPS" then "ServerURL" else "ServerName"} = v.server;
            }
            // lib.optionalAttrs (v.addresses != [ ]) { ServerAddresses = v.addresses; };
          };
          raw = v.content // {
            PayloadType = v.type;
          };
        }
        .${tag}
        // rec {
          PayloadVersion = 1;
          PayloadIdentifier = "${cfg.identifier}.${key}";
          PayloadUUID = den.lib.mobile.uuid PayloadIdentifier;
        };
    in
    {
      # No free-form command: a secret is ciphertext or lives outside nix.
      options.secrets = mkOption {
        default = { };
        description = "Secrets a payload may name, decrypted or fetched at activation.";
        type = types.attrsOf (
          types.attrTag {
            age = mkOption {
              type = types.submodule {
                options.file = mkOption {
                  type = types.path;
                  description = "Ciphertext; landing in the store is fine.";
                };
                options.identity = mkOption {
                  type = types.addCheck (types.strMatching "/.+") (p: !lib.hasPrefix builtins.storeDir p) // {
                    description = "absolute path outside the nix store";
                  };
                  description = "An age or ssh identity outside the store.";
                };
                options.sudo = mkOption {
                  type = types.bool;
                  default = false;
                  description = "For a root-only identity such as the host key.";
                };
              };
            };
            keychain = mkOption {
              type = types.submodule {
                options.service = mkOption { type = types.str; };
                options.account = mkOption {
                  type = types.nullOr types.str;
                  default = null;
                };
              };
            };
          }
        );
      };

      options.build.secretCommands = mkOption {
        type = types.attrsOf (types.listOf types.str);
        readOnly = true;
      };

      config.build.secretCommands = lib.mapAttrs (
        _: s:
        if s ? age then
          lib.optional s.age.sudo "sudo"
          ++ [
            (lib.getExe pkgs.age)
            "-d"
            "-i"
            s.age.identity
            "${s.age.file}"
          ]
        else
          [
            "/usr/bin/security"
            "find-generic-password"
            "-w"
            "-s"
            s.keychain.service
          ]
          ++ lib.optionals (s.keychain.account != null) [
            "-a"
            s.keychain.account
          ]
      ) config.secrets;

      options.profile = {
        identifier = mkOption {
          type = types.strMatching "[A-Za-z0-9.-]+";
          default = "in.ivymect.mobile.${name}";
        };
        displayName = mkOption {
          type = types.str;
          default = "${config.device.description} (dendritic)";
        };
        description = mkOption {
          type = types.str;
          default = "Generated from the ${name} den host.";
        };
        payloads = mkOption {
          type = types.attrsOf payloadType;
          default = { };
        };
      };

      options.build.profile = mkOption {
        type = types.nullOr types.str;
        readOnly = true;
        description = "The profile as plist XML, with `@…@` placeholders left for activation.";
      };

      config.build.profile =
        if cfg.payloads == { } then
          null
        else
          lib.generators.toPlist { escape = true; } {
            PayloadType = "Configuration";
            PayloadVersion = 1;
            PayloadIdentifier = cfg.identifier;
            PayloadUUID = den.lib.mobile.uuid cfg.identifier;
            PayloadDisplayName = cfg.displayName;
            PayloadDescription = cfg.description;
            PayloadRemovalDisallowed = false;
            PayloadContent = lib.mapAttrsToList render cfg.payloads;
          };
    };
}
