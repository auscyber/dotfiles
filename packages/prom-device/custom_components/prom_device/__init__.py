"""Prometheus queries as Home Assistant devices."""

from __future__ import annotations

import asyncio
import logging
import math
import string
from dataclasses import dataclass
from datetime import timedelta
from typing import Any

import voluptuous as vol
from aiohttp import ClientError, ClientTimeout
from homeassistant.config_entries import SOURCE_IMPORT, ConfigEntry
from homeassistant.const import Platform
from homeassistant.core import HomeAssistant
from homeassistant.helpers import config_validation as cv
from homeassistant.helpers.aiohttp_client import async_get_clientsession
from homeassistant.helpers.typing import ConfigType
from homeassistant.helpers.update_coordinator import DataUpdateCoordinator, UpdateFailed
from homeassistant.util import slugify

DOMAIN = "prom_device"
PLATFORMS = [Platform.SENSOR]

_LOGGER = logging.getLogger(__name__)

_REQUEST_TIMEOUT = ClientTimeout(total=30)

# Dropped when building a display name: they identify the device, which
# `has_entity_name` already prefixes, so repeating them is noise.
_NAME_EXCLUDED = frozenset({"__name__", "instance", "job"})

# Dropped when building an entity key. `job` only, and not `instance`: the job
# name is a property of the scrape configuration rather than of the series, so
# renaming a job would otherwise orphan every entity, while `instance` is the
# one label that distinguishes two hosts in a fleet-wide query.
_KEY_EXCLUDED = frozenset({"job"})

QUERY_SCHEMA = vol.Schema(
    {
        vol.Required("name"): cv.string,
        vol.Required("query"): cv.string,
        vol.Optional("unit"): cv.string,
        vol.Optional("device_class"): cv.string,
        vol.Optional("digits", default=1): vol.All(
            vol.Coerce(int), vol.Range(min=0, max=6)
        ),
    }
)

DEVICE_SCHEMA = vol.Schema(
    {
        vol.Required("name"): cv.string,
        vol.Required("url"): cv.url,
        vol.Optional("model", default="Prometheus"): cv.string,
        vol.Optional("interval", default=60): vol.All(
            vol.Coerce(int), vol.Range(min=5)
        ),
        vol.Required("queries"): vol.All(
            cv.ensure_list, [QUERY_SCHEMA], vol.Length(min=1)
        ),
    }
)

CONFIG_SCHEMA = vol.Schema(
    {DOMAIN: vol.All(cv.ensure_list, [DEVICE_SCHEMA])},
    extra=vol.ALLOW_EXTRA,
)


@dataclass(frozen=True)
class Series:
    """One series returned by one query, as an entity would see it."""

    key: str
    name: str
    unit: str | None
    device_class: str | None
    digits: int
    value: float | None


def _parts(template: str) -> list[tuple[str, str | None]]:
    return [(literal, field) for literal, field, _, _ in string.Formatter().parse(template)]


def _entity_name(template: str, labels: dict[str, str]) -> str:
    if any(field is not None for _, field in _parts(template)):
        try:
            return template.format(**labels)
        except (KeyError, IndexError):
            return template
    extra = [v for k, v in sorted(labels.items()) if k not in _NAME_EXCLUDED]
    return " ".join([template, *extra])


def _key_base(template: str) -> str:
    # The literal text of the name only. A `{label}` placeholder is dropped,
    # because the label it names is already in the key with its value -- and
    # because otherwise adding or removing a placeholder from a name would
    # rewrite the unique_id of every entity the query produces.
    return slugify("".join(literal for literal, _ in _parts(template))) or slugify(template)


def _entity_key(query_name: str, labels: dict[str, str], keep_metric_name: bool) -> str:
    used = {
        k: v
        for k, v in labels.items()
        if k not in _KEY_EXCLUDED and (k != "__name__" or keep_metric_name)
    }
    parts = [_key_base(query_name)]
    parts += [f"{slugify(k)}_{slugify(v)}" for k, v in sorted(used.items())]
    return "_".join(parts)


def _value(raw: Any) -> float | None:
    try:
        value = float(raw)
    except (TypeError, ValueError):
        return None
    # Prometheus renders a missing/undefined sample as NaN (0/0 on a host with
    # no swap, for one) and +Inf on division by zero. Neither is a state HA can
    # record, so they become "unknown" rather than an entity that errors.
    return None if math.isnan(value) or math.isinf(value) else value


def _series_for(query: dict[str, Any], result: list[dict[str, Any]]) -> dict[str, Series]:
    metric_names = {r.get("metric", {}).get("__name__") for r in result} - {None}
    # `__name__` enters the key only when one query really does return several
    # metrics (`{__name__=~"node_load(1|5|15)"}`). Including it unconditionally
    # would change the unique_id of every other entity.
    keep_metric_name = len(metric_names) > 1

    series: dict[str, Series] = {}
    for entry in result:
        labels = entry.get("metric", {})
        key = _entity_key(query["name"], labels, keep_metric_name)
        if key in series:
            _LOGGER.warning(
                "Query %r returned two series that slug to %r; keeping the first",
                query["name"],
                key,
            )
            continue
        series[key] = Series(
            key=key,
            name=_entity_name(query["name"], labels),
            unit=query.get("unit"),
            device_class=query.get("device_class"),
            digits=query["digits"],
            value=_value((entry.get("value") or [None, None])[1]),
        )
    return series


class PromCoordinator(DataUpdateCoordinator[dict[str, Series]]):
    """Polls every query of one device, once per interval."""

    def __init__(
        self, hass: HomeAssistant, entry: ConfigEntry, device: dict[str, Any]
    ) -> None:
        super().__init__(
            hass,
            _LOGGER,
            config_entry=entry,
            name=f"{DOMAIN} {device['name']}",
            update_interval=timedelta(seconds=device["interval"]),
        )
        self.device = device
        self._url = device["url"].rstrip("/") + "/api/v1/query"
        self._session = async_get_clientsession(hass)
        self._rejected: set[str] = set()
        # A device is tens of queries; in flight all at once they would queue
        # behind Prometheus's own query concurrency limit and behind the other
        # devices' polls, so a handful at a time keeps the round trips
        # overlapping without that.
        self._limit = asyncio.Semaphore(6)

    async def _async_update_data(self) -> dict[str, Series]:
        queries = self.device["queries"]
        results = await asyncio.gather(*(self._query(q) for q in queries))
        series: dict[str, Series] = {}
        for query, result in zip(queries, results):
            for key, found in _series_for(query, result).items():
                if key in series:
                    # Two queries on one device whose names slug the same and
                    # whose series carry the same labels. The later one would
                    # silently take the earlier one's entity, so say so rather
                    # than lose a sensor: rename one of them.
                    _LOGGER.error(
                        "Queries %r and %r both produce entity key %r on %r;"
                        " keeping the first",
                        series[key].name,
                        found.name,
                        key,
                        self.device["name"],
                    )
                    continue
                series[key] = found
        return series

    async def _query(self, query: dict[str, Any]) -> list[dict[str, Any]]:
        expr = query["query"]
        try:
            async with self._limit:
                async with self._session.get(
                    self._url, params={"query": expr}, timeout=_REQUEST_TIMEOUT
                ) as response:
                    status = response.status
                    payload = await response.json(content_type=None)
        except (ClientError, TimeoutError, ValueError) as err:
            raise UpdateFailed(f"{self._url}: {err}") from err

        if status in (400, 422):
            # A query Prometheus refuses to parse or execute is a configuration
            # error, not an outage: failing the whole update would take every
            # other sensor on the device down with it. Report it once and treat
            # it as an empty result, which leaves only its own entities
            # unavailable.
            if expr not in self._rejected:
                self._rejected.add(expr)
                _LOGGER.error(
                    "Prometheus rejected query %r (%s): %s",
                    query["name"],
                    status,
                    (payload or {}).get("error", "no detail"),
                )
            return []
        self._rejected.discard(expr)

        if status != 200:
            raise UpdateFailed(f"{self._url}: HTTP {status}")
        if not isinstance(payload, dict) or payload.get("status") != "success":
            raise UpdateFailed(
                f"{self._url}: {(payload or {}).get('error', 'malformed response')}"
            )

        data = payload.get("data") or {}
        if data.get("resultType") != "vector":
            raise UpdateFailed(
                f"query {query['name']!r} returned a {data.get('resultType')},"
                " not an instant vector"
            )
        return data.get("result") or []


PromConfigEntry = ConfigEntry[PromCoordinator]


async def async_setup(hass: HomeAssistant, config: ConfigType) -> bool:
    """Hand every device declared in YAML to the import config flow."""
    for device in config.get(DOMAIN) or []:
        hass.async_create_task(
            hass.config_entries.flow.async_init(
                DOMAIN, context={"source": SOURCE_IMPORT}, data=device
            )
        )
    return True


async def async_setup_entry(hass: HomeAssistant, entry: PromConfigEntry) -> bool:
    coordinator = PromCoordinator(hass, entry, dict(entry.data))
    await coordinator.async_config_entry_first_refresh()
    entry.runtime_data = coordinator
    await hass.config_entries.async_forward_entry_setups(entry, PLATFORMS)
    return True


async def async_unload_entry(hass: HomeAssistant, entry: PromConfigEntry) -> bool:
    return await hass.config_entries.async_unload_platforms(entry, PLATFORMS)
