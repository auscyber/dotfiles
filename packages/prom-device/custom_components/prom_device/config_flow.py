"""Import-only config flow: one entry per device declared in YAML."""

from __future__ import annotations

from typing import Any

from homeassistant.config_entries import ConfigFlow, ConfigFlowResult

from . import DOMAIN


class PromDeviceConfigFlow(ConfigFlow, domain=DOMAIN):
    """There is no user step. Devices come from configuration.yaml only."""

    VERSION = 1

    async def async_step_import(
        self, import_data: dict[str, Any]
    ) -> ConfigFlowResult:
        await self.async_set_unique_id(import_data["name"])
        # Rewrites the entry's data and reloads it when the YAML changed, which
        # is what makes a query added in Nix appear without touching the UI.
        self._abort_if_unique_id_configured(updates=import_data)
        return self.async_create_entry(title=import_data["name"], data=import_data)
