"""Build an unsigned iOS configuration profile of web clips from a device manifest."""

import json
import plistlib
import sys
import uuid

manifest_path, suffix, selfdns, out = sys.argv[1:5]

with open(manifest_path) as f:
    manifest = json.load(f)

ident = "in.ivymect.mobile." + manifest["name"]

payloads = []
for key, clip in sorted(manifest["webClips"].items()):
    url = clip.get("url")
    if not url:
        host = clip.get("servedBy")
        base = f"{host}.{suffix}" if host else selfdns
        url = f"https://{base}{clip.get('tailnetPath') or '/'}"
    payload_id = f"{ident}.webclip.{key}"
    payloads.append(
        {
            "PayloadType": "com.apple.webClip.managed",
            "PayloadVersion": 1,
            "PayloadIdentifier": payload_id,
            "PayloadUUID": str(uuid.uuid5(uuid.NAMESPACE_URL, payload_id)).upper(),
            "PayloadDisplayName": clip["label"],
            "URL": url,
            "Label": clip["label"],
            "IsRemovable": bool(clip["removable"]),
            "FullScreen": bool(clip["fullScreen"]),
        }
    )

profile = {
    "PayloadType": "Configuration",
    "PayloadVersion": 1,
    "PayloadIdentifier": ident,
    "PayloadUUID": str(uuid.uuid5(uuid.NAMESPACE_URL, ident)).upper(),
    "PayloadDisplayName": f"{manifest['name']} web clips",
    "PayloadDescription": "Home-screen links to tailnet services.",
    "PayloadRemovalDisallowed": False,
    "PayloadContent": payloads,
}

with open(out, "wb") as f:
    plistlib.dump(profile, f)

for payload in payloads:
    print(f"  {payload['Label']}: {payload['URL']}", file=sys.stderr)
