# Shortcuts as data. Each action is one tag; `id` names an action's output so
# later actions can read it (`{ ref.id = "x"; }`, or `{ ref = { id; key; }; }`
# for a dictionary value). Compiled to the unsigned WFWorkflow plist; signing
# needs the Mac's iCloud identity and happens at activation.
{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.shortcuts =
    {
      config,
      name,
      den,
      ...
    }:
    let
      opt = type: mkOption { inherit type; };
      optDefault = type: default: mkOption { inherit type default; };
      sub = options: types.submodule { inherit options; };
      id = opt (types.strMatching "[A-Za-z][A-Za-z0-9_-]*");

      variable = types.attrTag {
        ref = opt (sub {
          id = opt types.str;
          key = optDefault (types.nullOr types.str) null;
        });
        repeatItem = opt (sub { });
      };
      # Attachment offsets count UTF-16 units, so literals spliced between
      # variables are kept to ASCII; a lone string may be anything.
      text = types.either types.str (types.listOf (types.either (types.strMatching "[ -~]*") variable));

      action = types.attrTag {
        comment = opt types.str;
        alert = opt (sub {
          title = opt types.str;
          message = opt text;
          cancel = optDefault types.bool false;
        });
        notify = opt (sub {
          title = optDefault types.str "";
          body = opt text;
        });
        text = opt (sub {
          inherit id;
          text = opt text;
        });
        download = opt (sub {
          inherit id;
          url = opt text;
        });
        dictionary = opt (sub {
          inherit id;
          input = opt variable;
        });
        getFile = opt (sub {
          inherit id;
          path = opt types.str;
        });
        saveFile = opt (sub {
          input = opt variable;
          path = opt types.str;
        });
        deviceDetails = opt (sub {
          inherit id;
          detail = opt (
            types.enum [
              "Device Model"
              "Device Name"
              "System Version"
            ]
          );
        });
        openUrl = opt text;
        setWallpaper = opt (sub {
          image = opt variable;
          where = optDefault (types.enum [
            "Lock Screen"
            "Home Screen"
            "Lock Screen and Home Screen"
          ]) "Lock Screen and Home Screen";
        });
        forEach = opt (sub {
          over = opt variable;
          body = opt (types.listOf action);
        });
        ifEquals = opt (sub {
          input = opt variable;
          value = opt text;
          "then" = optDefault (types.listOf action) [ ];
          "else" = optDefault (types.listOf action) [ ];
        });
        stop = opt (sub { });
      };

      compile =
        sname: actions:
        let
          uuid = s: den.lib.mobile.uuid "${name}/${sname}/${s}";
          objectReplacement = builtins.fromJSON ''"￼"'';

          attach =
            v:
            if v ? repeatItem then
              {
                Type = "Variable";
                VariableName = "Repeat Item";
              }
            else
              {
                Type = "ActionOutput";
                OutputUUID = uuid "out:${v.ref.id}";
                OutputName = v.ref.id;
              }
              // lib.optionalAttrs (v.ref.key != null) {
                Aggrandizements = [
                  {
                    Type = "WFDictionaryValueVariableAggrandizement";
                    DictionaryKey = v.ref.key;
                  }
                ];
              };
          attachment = v: {
            Value = attach v;
            WFSerializationType = "WFTextTokenAttachment";
          };

          tokens =
            t:
            if builtins.isString t then
              t
            else
              let
                acc =
                  lib.foldl'
                    (
                      a: s:
                      if builtins.isString s then
                        a
                        // {
                          string = a.string + s;
                          pos = a.pos + lib.stringLength s;
                        }
                      else
                        a
                        // {
                          string = a.string + objectReplacement;
                          pos = a.pos + 1;
                          ranges = a.ranges // {
                            "{${toString a.pos}, 1}" = attach s;
                          };
                        }
                    )
                    {
                      string = "";
                      pos = 0;
                      ranges = { };
                    }
                    t;
              in
              {
                Value = {
                  inherit (acc) string;
                  attachmentsByRange = acc.ranges;
                };
                WFSerializationType = "WFTextTokenString";
              };

          # A text that is exactly one variable is passed as the variable.
          input =
            t:
            if builtins.isList t && builtins.length t == 1 && !builtins.isString (lib.head t) then
              attachment (lib.head t)
            else
              tokens t;

          act = ident: params: {
            WFWorkflowActionIdentifier = "is.workflow.actions.${ident}";
            WFWorkflowActionParameters = params;
          };
          output = i: {
            UUID = uuid "out:${i}";
            CustomOutputName = i;
          };

          many = path: list: lib.concatLists (lib.imap0 (i: one "${path}.${toString i}") list);
          one =
            path: a:
            let
              tag = lib.head (lib.attrNames a);
              v = a.${tag};
              group = uuid "group:${path}";
              flow =
                mode: extra:
                {
                  WFControlFlowMode = mode;
                  GroupingIdentifier = group;
                }
                // extra;
            in
            {
              comment = [ (act "comment" { WFCommentActionText = v; }) ];
              alert = [
                (act "alert" {
                  WFAlertActionTitle = v.title;
                  WFAlertActionMessage = tokens v.message;
                  WFAlertActionCancelButtonShown = v.cancel;
                })
              ];
              notify = [
                (act "notification" {
                  WFNotificationActionTitle = v.title;
                  WFNotificationActionBody = tokens v.body;
                })
              ];
              text = [ (act "gettext" ({ WFTextActionText = tokens v.text; } // output v.id)) ];
              download = [ (act "downloadurl" ({ WFURL = tokens v.url; } // output v.id)) ];
              dictionary = [ (act "detect.dictionary" ({ WFInput = attachment v.input; } // output v.id)) ];
              getFile = [
                (act "documentpicker.open" (
                  {
                    WFGetFilePath = v.path;
                    WFShowFilePicker = false;
                    WFFileErrorIfNotFound = false;
                  }
                  // output v.id
                ))
              ];
              saveFile = [
                (act "documentpicker.save" {
                  WFInput = attachment v.input;
                  WFAskWhereToSave = false;
                  WFFileDestinationPath = v.path;
                  WFSaveFileOverwrite = true;
                })
              ];
              deviceDetails = [ (act "getdevicedetails" ({ WFDeviceDetail = v.detail; } // output v.id)) ];
              openUrl = [ (act "openurl" { WFInput = input v; }) ];
              setWallpaper = [
                (act "wallpaper.set" {
                  WFInput = attachment v.image;
                  WFWallpaperLocation = v.where;
                  WFWallpaperShowPreview = false;
                })
              ];
              forEach = [
                (act "repeat.each" (flow 0 { WFInput = attachment v.over; }))
              ]
              ++ many path v.body
              ++ [ (act "repeat.each" (flow 2 { UUID = uuid "end:${path}"; })) ];
              ifEquals = [
                (act "conditional" (
                  flow 0 {
                    WFCondition = 4;
                    WFConditionalActionString = tokens v.value;
                    WFInput = {
                      Type = "Variable";
                      Variable = attachment v.input;
                    };
                  }
                ))
              ]
              ++ many "${path}.then" v."then"
              ++ lib.optionals (v."else" != [ ]) (
                [ (act "conditional" (flow 1 { })) ] ++ many "${path}.else" v."else"
              )
              ++ [ (act "conditional" (flow 2 { UUID = uuid "end:${path}"; })) ];
              stop = [ (act "exit" { }) ];
            }
            .${tag};
        in
        many "" actions;
    in
    {
      options.shortcuts = mkOption {
        default = { };
        description = "Shortcuts, by the name they take in the Shortcuts app.";
        type = types.attrsOf (
          types.submodule {
            options = {
              icon.color = optDefault types.ints.unsigned 4282601983;
              icon.glyph = optDefault types.ints.unsigned 59511;
              actions = opt (types.listOf action);
            };
          }
        );
      };

      options.build.shortcuts = mkOption {
        type = types.attrsOf types.str;
        readOnly = true;
      };

      config.build.shortcuts = lib.mapAttrs (
        sname: s:
        lib.generators.toPlist { escape = true; } {
          WFWorkflowMinimumClientVersion = 900;
          WFWorkflowMinimumClientVersionString = "900";
          WFWorkflowClientVersion = "2302.0.4";
          WFWorkflowIcon = {
            WFWorkflowIconStartColor = s.icon.color;
            WFWorkflowIconGlyphNumber = s.icon.glyph;
          };
          WFWorkflowImportQuestions = [ ];
          WFWorkflowTypes = [ ];
          WFWorkflowInputContentItemClasses = [ ];
          WFWorkflowActions = compile sname s.actions;
        }
      ) config.shortcuts;
    };
}
