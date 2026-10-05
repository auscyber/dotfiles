"""One sensor per series, created as the series appear."""

from __future__ import annotations

import logging

from homeassistant.components.sensor import (
    SensorDeviceClass,
    SensorEntity,
    SensorStateClass,
)
from homeassistant.core import HomeAssistant, callback
from homeassistant.helpers.device_registry import DeviceInfo
from homeassistant.helpers.entity_platform import AddConfigEntryEntitiesCallback
from homeassistant.helpers.update_coordinator import CoordinatorEntity

from . import DOMAIN, PromConfigEntry, PromCoordinator

_LOGGER = logging.getLogger(__name__)


async def async_setup_entry(
    hass: HomeAssistant,
    entry: PromConfigEntry,
    async_add_entities: AddConfigEntryEntitiesCallback,
) -> None:
    coordinator = entry.runtime_data
    device = DeviceInfo(
        identifiers={(DOMAIN, entry.entry_id)},
        name=entry.data["name"],
        manufacturer="Prometheus",
        model=entry.data.get("model"),
    )
    # No `configuration_url`: the only URL known here is the loopback one the
    # component polls, and a device page linking a phone at its own 127.0.0.1
    # is worse than no link.
    known: set[str] = set()

    @callback
    def _add_new_series() -> None:
        new = [
            PromSensor(coordinator, device, key)
            for key in coordinator.data or {}
            if key not in known
        ]
        if not new:
            return
        known.update(sensor.series_key for sensor in new)
        async_add_entities(new)

    # Entities are discovered from the data, not declared: a disk, NIC or hwmon
    # sensor that appears on the host later shows up on the next poll with no
    # configuration change. A series that stops being returned keeps its entity
    # and goes unavailable (see `available` below) rather than disappearing,
    # because a query can legitimately return nothing for one interval.
    _add_new_series()
    entry.async_on_unload(coordinator.async_add_listener(_add_new_series))


class PromSensor(CoordinatorEntity[PromCoordinator], SensorEntity):
    """A single Prometheus series."""

    _attr_has_entity_name = True
    _attr_state_class = SensorStateClass.MEASUREMENT

    def __init__(
        self, coordinator: PromCoordinator, device: DeviceInfo, key: str
    ) -> None:
        super().__init__(coordinator)
        self.series_key = key
        series = coordinator.data[key]
        self._attr_unique_id = f"{coordinator.config_entry.entry_id}_{key}"
        self._attr_device_info = device
        self._attr_name = series.name
        self._attr_native_unit_of_measurement = series.unit
        self._attr_suggested_display_precision = series.digits
        if series.device_class is not None:
            try:
                self._attr_device_class = SensorDeviceClass(series.device_class)
            except ValueError:
                _LOGGER.error(
                    "Unknown device_class %r on %r; leaving it unset",
                    series.device_class,
                    series.name,
                )

    @property
    def _series(self):
        return (self.coordinator.data or {}).get(self.series_key)

    @property
    def available(self) -> bool:
        return super().available and self._series is not None

    @property
    def native_value(self) -> float | None:
        series = self._series
        return None if series is None else series.value
