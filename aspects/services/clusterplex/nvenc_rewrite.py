from __future__ import annotations

import os
import re
import sys
from collections.abc import Iterator, Sequence
from dataclasses import dataclass
from typing import Final, NewType

Arg = NewType("Arg", str)
OptionName = NewType("OptionName", str)
FilterGraph = NewType("FilterGraph", str)
DeviceName = NewType("DeviceName", str)

TRANSCODER: Final = "@transcoder@"
DRIVER_LIBS: Final = "@driverLibs@"
RELAY_FROM: Final = "@relayFrom@"
RELAY_TO: Final = "@relayTo@"
CUDA_DEVICE: Final = os.environ.get("NVENC_CUDA_DEVICE", "0")
UNMAPPABLE_EXIT: Final = 69

ENCODERS: Final[dict[str, str]] = {
    "h264_vaapi": "h264_nvenc",
    "hevc_vaapi": "hevc_nvenc",
}

RC_MODES: Final[dict[str, str]] = {
    "CQP": "constqp",
    "CBR": "cbr",
    "VBR": "vbr",
    "QVBR": "vbr",
    "AVBR": "vbr",
    "ICQ": "vbr",
}

VAAPI_ONLY_OPTIONS: Final[frozenset[str]] = frozenset(
    {
        "low_power",
        "compression_level",
        "async_depth",
        "b_depth",
        "idr_interval",
        "quality",
        "sei",
        "vaapi_device",
    }
)

CODEC_OPTIONS: Final[frozenset[str]] = frozenset({"c", "codec", "vcodec"})
FILTER_OPTIONS: Final[frozenset[str]] = frozenset(
    {"vf", "filter", "filter_complex", "lavfi"}
)
DEVICE_REF_OPTIONS: Final[frozenset[str]] = frozenset(
    {"hwaccel_device", "filter_hw_device"}
)

OPTION: Final = re.compile(r"^-(?P<name>[a-z0-9_]+)(?P<spec>:.+)?$")
FILTER: Final = re.compile(
    r"^(?P<inputs>(?:\[[^\]]*\])*)"
    r"(?P<name>[A-Za-z0-9_]+)"
    r"(?:=(?P<args>.*?))?"
    r"(?P<outputs>(?:\[[^\]]*\])*)$",
    re.DOTALL,
)


class Unmappable(Exception):
    pass


@dataclass(frozen=True)
class Option:
    name: OptionName
    spec: str

    @staticmethod
    def parse(token: Arg) -> Option | None:
        m = OPTION.match(token)
        if m is None:
            return None
        return Option(OptionName(m["name"]), m["spec"] or "")

    def render(self, name: str | None = None) -> Arg:
        return Arg(f"-{name or self.name}{self.spec}")


@dataclass(frozen=True)
class Filter:
    inputs: str
    name: str
    args: str | None
    outputs: str

    def render(self) -> str:
        body = self.name if self.args is None else f"{self.name}={self.args}"
        return f"{self.inputs}{body}{self.outputs}"


def split_unquoted(s: str, sep: str) -> list[str]:
    parts: list[str] = []
    buf: list[str] = []
    quoted = False
    depth = 0
    chars = iter(s)
    for c in chars:
        if c == "\\":
            buf.append(c)
            buf.append(next(chars, ""))
            continue
        if c == "'":
            quoted = not quoted
        elif not quoted and c == "[":
            depth += 1
        elif not quoted and c == "]":
            depth -= 1
        if c == sep and not quoted and depth == 0:
            parts.append("".join(buf))
            buf = []
        else:
            buf.append(c)
    parts.append("".join(buf))
    return parts


def filter_args(args: str | None) -> tuple[list[str], dict[str, str]]:
    positional: list[str] = []
    named: dict[str, str] = {}
    for part in split_unquoted(args or "", ":"):
        if part == "":
            continue
        key, eq, value = part.partition("=")
        if eq:
            named[key] = value
        else:
            positional.append(part)
    return positional, named


def render_args(named: dict[str, str]) -> str | None:
    return ":".join(f"{k}={v}" for k, v in named.items()) or None


def scale_cuda(f: Filter) -> Filter:
    positional, named = filter_args(f.args)
    keys = ("w", "h")
    kept = dict(zip(keys, positional))
    kept |= {k: v for k, v in named.items() if k in {"w", "h", "format"}}
    kept |= {
        alias: named[full]
        for full, alias in (("width", "w"), ("height", "h"))
        if full in named
    }
    return Filter(f.inputs, "scale_cuda", render_args(kept), f.outputs)


def yadif_cuda(f: Filter) -> Filter:
    _, named = filter_args(f.args)
    mode = "send_field" if named.get("rate") == "field" else "send_frame"
    return Filter(f.inputs, "yadif_cuda", f"mode={mode}", f.outputs)


def pix_fmts(f: Filter) -> Filter:
    fmts = (f.args or "").split("|")
    swapped = "|".join("cuda" if p == "vaapi" else p for p in fmts)
    return Filter(f.inputs, f.name, swapped, f.outputs)


def rewrite_filter(raw: str) -> str:
    m = FILTER.match(raw.strip())
    if m is None:
        if "vaapi" in raw:
            raise Unmappable(raw)
        return raw
    f = Filter(m["inputs"], m["name"], m["args"], m["outputs"])
    match f.name:
        case "scale_vaapi":
            return scale_cuda(f).render()
        case "deinterlace_vaapi":
            return yadif_cuda(f).render()
        case "format":
            return pix_fmts(f).render()
        case name if "vaapi" in name or "vaapi" in (f.args or ""):
            raise Unmappable(raw)
        case _:
            return raw


def rewrite_graph(graph: FilterGraph) -> FilterGraph:
    chains = (
        ",".join(rewrite_filter(f) for f in split_unquoted(chain, ","))
        for chain in split_unquoted(graph, ";")
    )
    return FilterGraph(";".join(chains))


@dataclass(frozen=True)
class Device:
    kind: str
    name: DeviceName
    source: str

    @staticmethod
    def parse(value: str) -> Device:
        kind, _, rest = value.partition("=")
        name, _, source = rest.partition(":")
        name = name.partition("@")[0]
        return Device(kind, DeviceName(name or kind), source)

    @property
    def cuda_name(self) -> DeviceName:
        return DeviceName("cuda") if self.name == "vaapi" else self.name


def renames(args: Sequence[Arg]) -> dict[str, str]:
    devices = (
        Device.parse(v)
        for flag, v in zip(args, args[1:])
        if flag == "-init_hw_device"
    )
    return {d.name: d.cuda_name for d in devices if d.kind == "vaapi"}


@dataclass(frozen=True)
class Rewriter:
    renamed: dict[str, str]

    def device(self, value: str) -> Arg | None:
        d = Device.parse(value)
        match d.kind:
            case "drm":
                return None
            case "vaapi":
                return Arg(f"cuda={d.cuda_name}:{CUDA_DEVICE}")
            case _:
                return Arg(value)

    def option(self, opt: Option, value: Arg) -> Iterator[Arg]:
        match opt.name:
            case "hwaccel" | "hwaccel_output_format" if value == "vaapi":
                yield from (opt.render(), Arg("cuda"))
            case "init_hw_device":
                dev = self.device(value)
                if dev is not None:
                    yield from (opt.render(), dev)
            case name if name in DEVICE_REF_OPTIONS:
                yield from (opt.render(), Arg(self.renamed.get(value, value)))
            case name if name in CODEC_OPTIONS and value in ENCODERS:
                yield from (opt.render(), Arg(ENCODERS[value]))
            case name if name in FILTER_OPTIONS:
                yield from (opt.render(), Arg(rewrite_graph(FilterGraph(value))))
            case "rc_mode":
                mode = RC_MODES.get(value.upper())
                if mode is None:
                    raise Unmappable(f"-rc_mode {value}")
                yield from (opt.render("rc"), Arg(mode))
            case name if name in VAAPI_ONLY_OPTIONS:
                return
            case _:
                yield from (opt.render(), value)


VALUED: Final[frozenset[str]] = frozenset(
    {
        "hwaccel",
        "hwaccel_output_format",
        "init_hw_device",
        "rc_mode",
    }
    | DEVICE_REF_OPTIONS
    | CODEC_OPTIONS
    | FILTER_OPTIONS
    | VAAPI_ONLY_OPTIONS
)


def rewrite(args: Sequence[Arg]) -> list[Arg]:
    r = Rewriter(renames(args))
    out: list[Arg] = []
    i = 0
    while i < len(args):
        token = args[i]
        opt = Option.parse(token)
        if opt is not None and opt.name in VALUED and i + 1 < len(args):
            out.extend(r.option(opt, args[i + 1]))
            i += 2
            continue
        if "vaapi" in token or "/dev/dri" in token:
            raise Unmappable(token)
        out.append(token)
        i += 1
    return out


def relay(arg: str, src: str = RELAY_FROM, dst: str = RELAY_TO) -> Arg:
    return Arg(arg.replace(src, dst))


def main(argv: Sequence[str]) -> None:
    try:
        args = rewrite([relay(a) for a in argv])
    except Unmappable as e:
        print(f"nvenc-rewrite: no NVENC mapping for {e}", file=sys.stderr)
        sys.exit(UNMAPPABLE_EXIT)
    prior = os.environ.get("LD_LIBRARY_PATH")
    os.environ["LD_LIBRARY_PATH"] = ":".join(filter(None, (DRIVER_LIBS, prior)))
    os.execv(TRANSCODER, [TRANSCODER, *args])


if __name__ == "__main__":
    main(sys.argv[1:])
