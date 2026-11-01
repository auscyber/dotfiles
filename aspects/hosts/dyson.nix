# The Dyson HP00 fan, driven over IR by an ESP32-C3. The fan has no network
# interface, so the node keeps a shadow model: `a_*` globals are what the fan is
# believed to be doing, `d_*` what Home Assistant asked for, and `reconcile`
# presses one remote code at a time until they agree. Also a BLE proxy.
#
#   nix build .#esphome-dyson-yaml && cat result
#   nix run .#esphome-dyson-check
{
  lib,
  den,
  config,
  houseWifi,
  ...
}:
let
  name = "dyson";

  codes = {
    power = "0xc00";
    cool = "0xc2a";
    tempUp = "0xc1e";
    tempDown = "0xc33";
    speedUp = "0xc15";
    speedDown = "0xc3f";
    osc = "0xc0c";
    focused = "0xc12";
    diffused = "0xc2d";
    timer = "0xc21";
  };

  gap = "1200ms";

  lambda = src: { "!lambda" = src; };

  lines = lib.concatStringsSep "\n";

  stateVars = [
    {
      n = "on";
      type = "bool";
      init = "false";
    }
    {
      n = "heat";
      type = "bool";
      init = "false";
    }
    {
      n = "temp";
      type = "int";
      init = "21";
    }
    {
      n = "speed";
      type = "int";
      init = "4";
    }
    {
      n = "osc";
      type = "bool";
      init = "false";
    }
    {
      n = "focused";
      type = "bool";
      init = "false";
    }
  ];

  mirrorAD = indent: lines (map (v: "${indent}id(d_${v.n}) = id(a_${v.n});") stateVars);

  scratchGlobals = [
    {
      id = "dy_idx";
      type = "int";
      restore_value = false;
      initial_value = "0";
    }
    {
      id = "echo_until";
      type = "uint32_t";
      restore_value = false;
      initial_value = "0";
    }
    {
      id = "last_sent";
      type = "int";
      restore_value = false;
      initial_value = "-1";
    }
    {
      id = "next_code";
      type = "int";
      restore_value = false;
      initial_value = "0";
    }
    {
      id = "rx_ok";
      type = "bool";
      restore_value = false;
      initial_value = "false";
    }
    {
      id = "rx_code";
      type = "int";
      restore_value = false;
      initial_value = "0";
    }
    {
      id = "busy";
      type = "bool";
      restore_value = false;
      initial_value = "false";
    }
  ];

  stateGlobals = lib.concatMap (v: [
    {
      id = "a_${v.n}";
      inherit (v) type;
      restore_value = true;
      initial_value = v.init;
    }
    {
      id = "d_${v.n}";
      inherit (v) type;
      restore_value = false;
      initial_value = v.init;
    }
  ]) stateVars;

  remoteButtons =
    map
      (b: {
        platform = "template";
        name = "Dyson Remote ${b.label}";
        on_press = [
          {
            "script.execute" = {
              id = "press";
              code = lambda "return ${b.code};";
            };
          }
          {
            "script.execute" = {
              id = "apply_code";
              code = lambda "return ${b.code};";
              from_user = true;
            };
          }
        ];
      })
      [
        {
          label = "Power";
          code = codes.power;
        }
        {
          label = "Cool";
          code = codes.cool;
        }
        {
          label = "Temp Up";
          code = codes.tempUp;
        }
        {
          label = "Temp Down";
          code = codes.tempDown;
        }
        {
          label = "Speed Up";
          code = codes.speedUp;
        }
        {
          label = "Speed Down";
          code = codes.speedDown;
        }
        {
          label = "Oscillate";
          code = codes.osc;
        }
        {
          label = "Focused";
          code = codes.focused;
        }
        {
          label = "Diffused";
          code = codes.diffused;
        }
      ];

  assumeButtons =
    map
      (b: {
        platform = "template";
        name = "Dyson Assume ${b.label}";
        on_press = [
          {
            lambda = lines [
              "id(a_${b.var}) = ${b.value};"
              "id(d_${b.var}) = ${b.value};"
            ];
          }
        ];
      })
      [
        {
          label = "On";
          var = "on";
          value = "true";
        }
        {
          label = "Off";
          var = "on";
          value = "false";
        }
        {
          label = "Heat";
          var = "heat";
          value = "true";
        }
        {
          label = "Cool";
          var = "heat";
          value = "false";
        }
      ];

  syncButtons =
    map
      (b: {
        platform = "template";
        name = "Dyson Sync ${b.label}";
        on_press = [
          {
            "if" = {
              condition.lambda = b.guard;
              "then" = [
                { lambda = "id(busy) = true;"; }
                {
                  repeat = {
                    inherit (b) count;
                    "then" = [
                      {
                        "script.execute" = {
                          id = "press";
                          code = lambda "return ${b.code};";
                        };
                      }
                      { "script.wait" = "press"; }
                    ];
                  };
                }
                { lambda = lines (b.settle ++ [ "id(busy) = false;" ]); }
              ];
            };
          }
        ];
      })
      [
        {
          label = "Temp Floor";
          guard = "return id(a_on);";
          count = 40;
          code = codes.tempDown;
          settle = [
            "id(a_heat) = true;"
            "id(a_temp) = 1;"
            "id(d_heat) = true;"
            "id(d_temp) = 1;"
          ];
        }
        {
          label = "Temp Ceiling";
          guard = "return id(a_on);";
          count = 40;
          code = codes.tempUp;
          settle = [
            "id(a_heat) = true;"
            "id(a_temp) = 37;"
            "id(d_heat) = true;"
            "id(d_temp) = 37;"
          ];
        }
        {
          label = "Speed Floor";
          guard = "return id(a_on) && !id(a_heat);";
          count = 12;
          code = codes.speedDown;
          settle = [
            "id(a_speed) = 1;"
            "id(d_speed) = 1;"
          ];
        }
        {
          label = "Speed Ceiling";
          guard = "return id(a_on) && !id(a_heat);";
          count = 12;
          code = codes.speedUp;
          settle = [
            "id(a_speed) = 10;"
            "id(d_speed) = 10;"
          ];
        }
      ];

  flagSwitch = label: var: {
    platform = "template";
    name = "Dyson ${label}";
    lambda = "return id(d_${var});";
    turn_on_action = [
      { lambda = "id(d_${var}) = true;"; }
      { "script.execute" = "reconcile"; }
    ];
    turn_off_action = [
      { lambda = "id(d_${var}) = false;"; }
      { "script.execute" = "reconcile"; }
    ];
  };

  flagSelect = a: {
    platform = "template";
    name = "Dyson ${a.label}";
    options = [
      a.whenTrue
      a.whenFalse
    ];
    update_interval = "2s";
    lambda = ''return id(d_${a.var}) ? std::string("${a.whenTrue}") : std::string("${a.whenFalse}");'';
    set_action = [
      { lambda = ''id(d_${a.var}) = (x == "${a.whenTrue}");''; }
      { "script.execute" = "reconcile"; }
    ];
  };

  intNumber = a: {
    platform = "template";
    name = "Dyson ${a.label}";
    min_value = a.min;
    max_value = a.max;
    step = 1;
    update_interval = "2s";
    lambda = "return id(d_${a.var});";
    set_action = [
      { lambda = "id(d_${a.var}) = (int) x;"; }
      { "script.execute" = "reconcile"; }
    ];
  };

  tempMin = 1;
  tempMax = 37;
  speedMin = 1;
  speedMax = 10;

  clampTemp =
    expr: "id(d_temp) = std::max(${toString tempMin}, std::min(${toString tempMax}, ${expr}));";
in
{
  # Flat: den only reads a key as a system group if it is in flakeExposed.
  den.hosts.${name} = {
    system = "esp32-idf";
    class = "esphome";
    instantiate = config.flake.lib.esphomeNode { inherit name; };
    intoAttr = [
      "esphomeConfigurations"
      name
    ];
    household = den.households.ivymectin;
  };

  den.aspects.${name} = {
    # lan-home too: the AP is bridged onto the lan, and `linksBetween`
    # computes siblinghood by carrier name.
    includes = [
      den.aspects.house-wifi
      den.aspects.lan-home
      den.aspects.esphome-bluetooth-proxy
    ];

    takes.home-assistant.extraComponents = [ "esphome" ];

    esphome = {
      node.friendlyName = "Dyson";
      # The running device's name; renaming orphans its HA entities.
      node.name = "dyson-hp00";
      node.platform = "esp32";
      node.board = "esp32-c3-devkitm-1";

      node.config = {

        esphome.on_boot = {
          priority = -100;
          "then" = [ { lambda = mirrorAD ""; } ];
        };

        esp32 = {
          variant = "esp32c3";
          framework.type = "esp-idf";
        };

        logger.hardware_uart = "USB_SERIAL_JTAG";

        # Not null: formats.yaml drops null attrs, so `api = null` loses the API.
        api = { };
        ota = [ { platform = "esphome"; } ];

        wifi.networks = map (ssid: {
          inherit ssid;
          password = {
            "!secret" = "wifi_password";
          };
        }) houseWifi.allSsids;

        remote_transmitter = {
          id = "tx";
          pin = "GPIO4";
          carrier_duty_percent = "50%";
        };

        remote_receiver = {
          id = "rx";
          pin = {
            number = "GPIO3";
            inverted = true;
            mode = {
              input = true;
              pullup = true;
            };
          };
          tolerance = "30%";
          idle = "25ms";
          dump = "dyson";

          on_dyson."then" = [
            {
              lambda = lines [
                "id(rx_ok) = false;"
                "uint32_t now = millis();"
                "if (now < id(echo_until)) {"
                "  if ((int) x.code == id(last_sent)) ESP_LOGI(\"dyson\", \"loopback ok code=0x%x index=%d\", x.code, x.index);"
                "  return;"
                "}"
                "id(dy_idx) = (x.index + 1) & 3;"
                "static uint32_t t_last = 0;"
                "static int c_last = -1;"
                "static int i_last = -1;"
                "bool twin = ((int) x.code == c_last && (int) x.index == i_last && now - t_last < 400);"
                "t_last = now;"
                "c_last = x.code;"
                "i_last = x.index;"
                "id(rx_ok) = !twin;"
                "id(rx_code) = x.code;"
              ];
            }
            {
              "if" = {
                condition.lambda = "return id(rx_ok);";
                "then" = [
                  {
                    "script.execute" = {
                      id = "apply_code";
                      code = lambda "return id(rx_code);";
                      from_user = true;
                    };
                  }
                ];
              };
            }
          ];
        };

        globals = scratchGlobals ++ stateGlobals;

        script = [
          {
            id = "press";
            mode = "queued";
            max_runs = 100;
            parameters.code = "int";
            "then" = [
              {
                lambda = lines [
                  "id(last_sent) = code;"
                  "id(echo_until) = millis() + 600;"
                ];
              }
              {
                "remote_transmitter.transmit_dyson" = {
                  code = lambda "return code;";
                  index = lambda "return (uint8_t) id(dy_idx);";
                  repeat = {
                    times = 2;
                    wait_time = "100ms";
                  };
                };
              }
              { lambda = "id(dy_idx) = (id(dy_idx) + 1) & 3;"; }
              { delay = gap; }
            ];
          }

          {
            id = "apply_code";
            mode = "queued";
            max_runs = 20;
            parameters = {
              code = "int";
              from_user = "bool";
            };
            "then" = [
              {
                lambda = lines [
                  "if (code == ${codes.power}) {"
                  "  id(a_on) = !id(a_on);"
                  "} else if (code == ${codes.cool}) {"
                  "  id(a_heat) = false;"
                  "} else if (code == ${codes.tempUp} || code == ${codes.tempDown}) {"
                  "  if (id(a_heat)) id(a_temp) = std::max(${toString tempMin}, std::min(${toString tempMax}, id(a_temp) + (code == ${codes.tempUp} ? 1 : -1)));"
                  "  id(a_heat) = true;"
                  "} else if (code == ${codes.speedUp} || code == ${codes.speedDown}) {"
                  "  if (!id(a_heat)) id(a_speed) = std::max(${toString speedMin}, std::min(${toString speedMax}, id(a_speed) + (code == ${codes.speedUp} ? 1 : -1)));"
                  "} else if (code == ${codes.osc}) {"
                  "  id(a_osc) = !id(a_osc);"
                  "} else if (code == ${codes.focused}) {"
                  "  id(a_focused) = true;"
                  "} else if (code == ${codes.diffused}) {"
                  "  id(a_focused) = false;"
                  "}"
                  "if (from_user) {"
                  (mirrorAD "  ")
                  "}"
                ];
              }
            ];
          }

          {
            id = "reconcile";
            mode = "single";
            "then" = [
              {
                "while" = {
                  condition.lambda = lines [
                    "int c = 0;"
                    "if (id(busy)) {"
                    "  id(next_code) = 0;"
                    "  return false;"
                    "}"
                    "if (id(d_on) != id(a_on)) {"
                    "  c = ${codes.power};"
                    "} else if (id(a_on)) {"
                    "  if (id(d_heat) != id(a_heat)) {"
                    "    c = id(d_heat) ? ${codes.tempUp} : ${codes.cool};"
                    "  } else if (id(a_heat) && id(d_temp) != id(a_temp)) {"
                    "    c = id(d_temp) > id(a_temp) ? ${codes.tempUp} : ${codes.tempDown};"
                    "  } else if (!id(a_heat) && id(d_speed) != id(a_speed)) {"
                    "    c = id(d_speed) > id(a_speed) ? ${codes.speedUp} : ${codes.speedDown};"
                    "  } else if (id(d_osc) != id(a_osc)) {"
                    "    c = ${codes.osc};"
                    "  } else if (id(d_focused) != id(a_focused)) {"
                    "    c = id(d_focused) ? ${codes.focused} : ${codes.diffused};"
                    "  }"
                    "}"
                    "id(next_code) = c;"
                    "return c != 0;"
                  ];
                  "then" = [
                    {
                      "script.execute" = {
                        id = "press";
                        code = lambda "return id(next_code);";
                      };
                    }
                    { "script.wait" = "press"; }
                    {
                      "script.execute" = {
                        id = "apply_code";
                        code = lambda "return id(next_code);";
                        from_user = false;
                      };
                    }
                    { "script.wait" = "apply_code"; }
                  ];
                };
              }
            ];
          }
        ];

        interval = [
          {
            interval = "5s";
            "then" = [
              {
                "if" = {
                  condition.not."script.is_running" = "reconcile";
                  "then" = [ { "script.execute" = "reconcile"; } ];
                };
              }
            ];
          }
        ];

        switch = [
          (flagSwitch "Power" "on")
          (flagSwitch "Oscillation" "osc")
        ];

        select = [
          (flagSelect {
            label = "Mode";
            var = "heat";
            whenTrue = "Heat";
            whenFalse = "Cool";
          })
          (flagSelect {
            label = "Airflow Shape";
            var = "focused";
            whenTrue = "Focused";
            whenFalse = "Diffused";
          })
        ];

        number = [
          (intNumber {
            label = "Setpoint";
            var = "temp";
            min = tempMin;
            max = tempMax;
          })
          (intNumber {
            label = "Fan Speed";
            var = "speed";
            min = speedMin;
            max = speedMax;
          })
        ];

        button = [
          {
            platform = "template";
            name = "Dyson Test Frame";
            on_press = [
              {
                "script.execute" = {
                  id = "press";
                  code = lambda "return ${codes.timer};";
                };
              }
            ];
          }
        ]
        ++ assumeButtons
        ++ syncButtons
        ++ remoteButtons;

        sensor = [
          {
            platform = "homeassistant";
            id = "room_temp";
            entity_id = "sensor.living_room_temperature";
          }
        ];

        climate = [
          {
            platform = "thermostat";
            name = "Dyson";
            id = "dyson_climate";
            sensor = "room_temp";
            min_idle_time = "10s";
            min_heating_off_time = "10s";
            min_heating_run_time = "10s";
            min_cooling_off_time = "10s";
            min_cooling_run_time = "10s";
            visual = {
              min_temperature = tempMin;
              max_temperature = tempMax;
              temperature_step = 1;
            };
            heat_action = [ { delay = "0s"; } ];
            cool_action = [ { delay = "0s"; } ];
            idle_action = [ { delay = "0s"; } ];
            heat_mode = [
              {
                lambda = lines [
                  "id(d_on) = true;"
                  "id(d_heat) = true;"
                  "float t = id(dyson_climate).target_temperature;"
                  "if (!std::isnan(t)) ${clampTemp "(int) std::round(t)"}"
                ];
              }
              { "script.execute" = "reconcile"; }
            ];
            cool_mode = [
              {
                lambda = lines [
                  "id(d_on) = true;"
                  "id(d_heat) = false;"
                ];
              }
              { "script.execute" = "reconcile"; }
            ];
            off_mode = [
              { lambda = "id(d_on) = false;"; }
              { "script.execute" = "reconcile"; }
            ];
            on_control = [
              {
                "if" = {
                  condition.lambda = "return x.get_target_temperature().has_value();";
                  "then" = [
                    { lambda = clampTemp "(int) std::round(*x.get_target_temperature())"; }
                    { "script.execute" = "reconcile"; }
                  ];
                };
              }
            ];
          }
        ];
      };
    };
  };
}
