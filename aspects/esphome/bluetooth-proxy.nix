{ ... }:
# Relays BLE advertisements and active connections to Home Assistant over the
# native API. Active connections need ESP-IDF, so the framework is pinned here
# and a node on Arduino or a non-esp32 platform fails as a definition conflict.
{
  den.aspects.esphome-bluetooth-proxy = {
    takes.home-assistant.extraComponents = [ "bluetooth" ];

    esphome = {
      node.platform = "esp32";
      node.config = {
        esp32.framework.type = "esp-idf";
        esp32_ble_tracker.scan_parameters.active = true;
        bluetooth_proxy.active = true;
      };
    };
  };
}
