#!/usr/bin/env python3
"""
_syncValueSets.py

Keeps the ValueSets that are maintained outside this repository (ART-DECOR, a
FHIR terminology server such as the Nationale Terminologieserver) in step with
the version this repository has pinned.

The pinned state lives in input/fsh/definitions/external-valuesets.json and is
committed. KPI and dataset definitions refer to an entry by its key, for example
{vs:ibd-diagnosis}; the generator resolves that to the pinned canonical.

  py -3 _syncValueSets.py fetch     download the pinned version of every entry
                                    marked "download" into input/resources/
                                    (what the build runs); a version already
                                    present is skipped, --force downloads again
  py -3 _syncValueSets.py check     ask the source for the latest version of
                                    every entry; exit 1 when one has moved on
  py -3 _syncValueSets.py update    move drifted entries to the latest version
                                    and download them; then regenerate
  py -3 _syncValueSets.py add URL   register a ValueSet (see --help)

Modes
  download   the definition has enumerable content (extensional); it is
             downloaded so the IG renders it and the build needs no server.
  reference  the content cannot be downloaded (for example a SNOMED ECL
             query on the terminology server); the canonical is only referenced
             and resolved by the terminology server at validation time.
             `add` chooses this automatically when the ValueSet is intensional.

Sources
  decor      ART-DECOR FHIR endpoint. ValueSet/<oid> returns the latest
             version, ValueSet/<oid>--<effectiveDate> a fixed one.
  fhir       Any FHIR terminology server (NTS). Searched by canonical url and
             version. `auth: nts` signs in to the Nationale Terminologieserver the
             way HipsETL does (OpenID password grant, client cli_client) with the
             environment variables NTS_USERNAME and NTS_PASSWORD, which are never
             stored in this repository. Alternatively `tokenEnv` names an environment
             variable that holds a ready bearer token.
             An implicit value set (a canonical containing "?fhir_vs=", for example a
             SNOMED ECL query) has no resource and no version of its own: it is
             "reference" mode and is skipped by check.

Standard library only.
"""

import argparse
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent
MANIFEST = ROOT / "input" / "fsh" / "definitions" / "external-valuesets.yaml"
OUT_DIR = ROOT / "input" / "resources"
DECOR_BASE = "https://decor.nictiz.nl/fhir/4.0/san-gen-"
NTS_TOKEN_URL = "https://terminologieserver.nl/authorisation/auth/realms/nictiz/protocol/openid-connect/token"
TIMEOUT = 30


class SyncError(Exception):
    pass


# ---------------------------------------------------------------------------
# The manifest is a small YAML subset: one top-level key per ValueSet, each with
# flat `field: value` lines (optionally quoted) and # comments. Writes edit single
# lines in place, so comments and layout survive an update.
BLOCK = re.compile(r"^([A-Za-z0-9._-]+):\s*(#.*)?$")
FIELD = re.compile(r"^  ([A-Za-z0-9_]+):\s*(.*?)\s*$")


def _value(raw):
    if raw[:1] in ("'", '"') and raw[-1:] == raw[:1] and len(raw) >= 2:
        return raw[1:-1]
    return re.sub(r"\s+#.*$", "", raw)


def parse_manifest(text):
    out, cur = [], None
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        m = BLOCK.match(line)
        if m:
            cur = {"key": m.group(1)}
            out.append(cur)
            continue
        m = FIELD.match(line)
        if m and cur is not None:
            cur[m.group(1)] = _value(m.group(2))
            continue
        raise SyncError(f"{MANIFEST.name}: cannot read line {line!r}")
    for e in out:
        e.setdefault("source", "decor")
        e.setdefault("mode", "download")
        if e["source"] == "decor":
            e.setdefault("base", DECOR_BASE)
        e.setdefault("version", "")
        if "canonical" not in e or (not e["version"] and e["mode"] != "reference"):
            raise SyncError(f"{MANIFEST.name}: '{e['key']}' needs a canonical and, unless mode is reference, a version")
    return out


def load():
    text = MANIFEST.read_text(encoding="utf-8") if MANIFEST.exists() else ""
    return {"valuesets": parse_manifest(text)}


def _fmt(field, value):
    return f'  {field}: "{value}"' if field == "version" else f"  {field}: {value}"


def set_fields(key, fields):
    """Edit one entry's lines in place; a field that is absent is added at the end of its block."""
    lines = MANIFEST.read_text(encoding="utf-8").splitlines()
    start = next((i for i, l in enumerate(lines) if (m := BLOCK.match(l)) and m.group(1) == key), None)
    if start is None:
        raise SyncError(f"'{key}' not found in {MANIFEST.name}")
    end = start + 1
    while end < len(lines) and (lines[end].startswith("  ") or not lines[end].strip()):
        end += 1
    while end > start + 1 and not lines[end - 1].strip():
        end -= 1
    for field, value in fields.items():
        for i in range(start + 1, end):
            if lines[i].startswith(f"  {field}:"):
                lines[i] = _fmt(field, value)
                break
        else:
            lines.insert(end, _fmt(field, value))
            end += 1
    MANIFEST.write_text("\n".join(lines) + "\n", encoding="utf-8", newline="\n")


def append_entry(e):
    text = MANIFEST.read_text(encoding="utf-8") if MANIFEST.exists() else ""
    block = [f"{e['key']}:"] + [_fmt(f, e[f]) for f in ("source", "base", "canonical", "version", "mode", "auth", "tokenEnv") if f in e]
    sep = "\n\n" if text.strip() else ""
    MANIFEST.write_text(text.rstrip("\n") + sep + "\n".join(block) + "\n", encoding="utf-8", newline="\n")


_NTS_TOKEN = {}


def nts_token():
    """Bearer token for the Nationale Terminologieserver (OpenID password grant, as HipsETL does)."""
    if "t" in _NTS_TOKEN:
        return _NTS_TOKEN["t"]
    user, pwd = os.environ.get("NTS_USERNAME"), os.environ.get("NTS_PASSWORD")
    if not user or not pwd:
        raise SyncError("NTS_USERNAME and NTS_PASSWORD are not set (an account at terminologieserver.nl is needed)")
    body = urllib.parse.urlencode({"grant_type": "password", "client_id": "cli_client",
                                   "username": user, "password": pwd}).encode()
    req = urllib.request.Request(NTS_TOKEN_URL, data=body, headers={"User-Agent": "sim-on-fhir-sync/1.0"})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
            _NTS_TOKEN["t"] = json.loads(r.read().decode("utf-8"))["access_token"]
    except urllib.error.HTTPError as e:
        raise SyncError(f"NTS sign-in failed (HTTP {e.code})")
    except (urllib.error.URLError, TimeoutError, OSError, KeyError, ValueError) as e:
        raise SyncError(f"NTS sign-in failed: {type(e).__name__}")
    return _NTS_TOKEN["t"]


def bearer(entry):
    if entry and entry.get("auth") == "nts":
        return nts_token()
    env = entry.get("tokenEnv") if entry else None
    if env:
        token = os.environ.get(env)
        if not token:
            raise SyncError(f"environment variable {env} is not set")
        return token
    return None


def get_json(url, entry=None):
    req = urllib.request.Request(url, headers={"Accept": "application/fhir+json, application/json",
                                               "User-Agent": "sim-on-fhir-sync/1.0"})
    token = bearer(entry)
    if token:
        req.add_header("Authorization", f"Bearer {token}")
    for attempt in (1, 2):
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
                return json.loads(r.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            raise SyncError(f"HTTP {e.code} for {url}")
        except (urllib.error.URLError, TimeoutError, OSError) as e:
            if attempt == 2:
                raise SyncError(f"cannot reach {url}: {e}")


def split_decor(canonical):
    """'http://decor.../ValueSet/<oid>[--<effective>]' -> (oid, effective or None)."""
    last = canonical.rstrip("/").split("/")[-1]
    oid, _, eff = last.partition("--")
    return oid, eff or None


def unpinned(canonical):
    return canonical.split("|")[0].split("--")[0]


def is_intensional(vs):
    """True when the ValueSet cannot be enumerated from its definition alone (filters, ECL, nested sets)."""
    for inc in vs.get("compose", {}).get("include", []):
        if inc.get("filter") or inc.get("valueSet"):
            return True
        if "fhir_vs" in inc.get("system", ""):
            return True
    return not vs.get("compose", {}).get("include")


# ---------------------------------------------------------------------------
def latest(entry, full=False):
    """The source's current ValueSet for an entry: (pinned canonical, version, resource).

    Without `full`, a FHIR server is asked for metadata only (_elements), so a version
    check does not transfer the codes. ART-DECOR ignores _elements and _summary and
    sends no ETag, so there the resource itself is the smallest answer.
    """
    if entry["source"] == "decor":
        oid, _ = split_decor(entry["canonical"])
        r = get_json(f"{entry['base']}/ValueSet/{oid}?_format=json", entry)
        return r["url"], r.get("version", ""), r
    base = unpinned(entry["canonical"])
    params = {"url": base, "_sort": "-date", "_count": "1"}
    if not full:
        params["_elements"] = "url,version,date"
    q = urllib.parse.urlencode(params)
    b = get_json(f"{entry['base']}/ValueSet?{q}", entry)
    if not b.get("entry"):
        raise SyncError(f"{base} not found on {entry['base']}")
    r = b["entry"][0]["resource"]
    return base, r.get("version", ""), r


def pinned_fetch(entry):
    if entry["source"] == "decor":
        oid, eff = split_decor(entry["canonical"])
        vid = f"{oid}--{eff}" if eff else oid
        return get_json(f"{entry['base']}/ValueSet/{vid}?_format=json", entry)
    q = urllib.parse.urlencode({"url": unpinned(entry["canonical"]), "version": entry["version"]})
    b = get_json(f"{entry['base']}/ValueSet?{q}", entry)
    if not b.get("entry"):
        raise SyncError(f"{entry['canonical']} version {entry['version']} not found")
    return b["entry"][0]["resource"]


def out_file(entry):
    """Where an entry's pinned ValueSet is stored. ART-DECOR: the versioned id, as _fetchValueSets.ps1 names it."""
    if entry["source"] == "decor":
        oid, eff = split_decor(entry["canonical"])
        name = f"{oid}--{eff}" if eff else oid
    else:
        name = entry["key"]
    return OUT_DIR / ("ValueSet-" + re.sub(r"[^a-zA-Z0-9._-]", "-", name) + ".json")


def is_current(entry, f):
    """Is the stored file already the pinned version? Then it is not downloaded again."""
    if not f.exists():
        return False
    if entry["source"] == "decor":
        return True  # the file name carries the effective date
    try:
        return json.loads(f.read_text(encoding="utf-8")).get("version") == entry["version"]
    except (OSError, ValueError):
        return False


def write_vs(entry, r):
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    f = out_file(entry)
    f.write_text(json.dumps(r, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")
    return f


# ---------------------------------------------------------------------------
def cmd_fetch(args):
    failed = 0
    for e in load()["valuesets"]:
        if e["mode"] != "download":
            print(f"  {e['key']}: reference only, nothing to download")
            continue
        if not getattr(args, "force", False) and is_current(e, out_file(e)):
            print(f"  {e['key']}: already downloaded ({e['version']})")
            continue
        try:
            f = write_vs(e, pinned_fetch(e))
            print(f"  {e['key']}: {f.relative_to(ROOT)}")
        except SyncError as ex:
            failed += 1
            print(f"  {e['key']}: FAILED - {ex}")
    return 1 if failed and args.strict else 0


def cmd_check(args):
    drift = err = 0
    for e in load()["valuesets"]:
        if "fhir_vs=" in e["canonical"]:
            print(f"  {e['key']}: implicit value set (no version of its own), nothing to check")
            continue
        try:
            canon, ver, _ = latest(e)
        except SyncError as ex:
            err += 1
            print(f"  {e['key']}: UNREACHABLE - {ex}")
            continue
        if canon == e["canonical"] and ver == e["version"]:
            print(f"  {e['key']}: up to date ({e['version']})")
        else:
            drift += 1
            print(f"  {e['key']}: OUTDATED  repo {e['version']}  ->  source {ver}")
    print(f"{drift} outdated, {err} unreachable")
    return 1 if drift or err else 0


def cmd_update(args):
    data = load()
    changed = []
    for e in data["valuesets"]:
        if "fhir_vs=" in e["canonical"]:
            continue
        try:
            canon, ver, r = latest(e)
        except SyncError as ex:
            print(f"  {e['key']}: UNREACHABLE - {ex}")
            continue
        if canon == e["canonical"] and ver == e["version"]:
            continue
        if e["mode"] == "download" and "compose" not in r:
            r = latest(e, full=True)[2]  # the light check had no content; classify on the full resource
        if e["mode"] == "download" and is_intensional(r):
            print(f"  {e['key']}: now intensional, switching to reference")
            e["mode"] = "reference"
        print(f"  {e['key']}: {e['version']} -> {ver}")
        set_fields(e["key"], {"canonical": canon, "version": ver, "mode": e["mode"]})
        changed.append(e["key"])
    if changed:
        cmd_fetch(argparse.Namespace(strict=False))
        print("Updated: " + ", ".join(changed) + "\nNow run: py -3 _generateFromDefinitions.py   (pins changed)")
    else:
        print("Nothing to update.")
    return 0


def cmd_add(args):
    data = load()
    canonical = args.url
    if args.source == "decor":
        oid, _ = split_decor(canonical)
        base = args.base or DECOR_BASE
        entry = dict(key=args.key or oid, source="decor", base=base, canonical=canonical, version="", mode="download")
    else:
        if not args.base:
            raise SyncError("--base is required for --source fhir")
        entry = dict(key=args.key or canonical.rstrip("/").split("/")[-1], source="fhir", base=args.base.rstrip("/"),
                     canonical=canonical, version="", mode="download")
    if args.nts:
        entry["auth"] = "nts"
    if args.token_env:
        entry["tokenEnv"] = args.token_env
    if any(e["key"] == entry["key"] for e in data["valuesets"]):
        raise SyncError(f"key '{entry['key']}' already exists")
    if "fhir_vs=" in canonical:  # implicit value set (e.g. a SNOMED ECL query): no resource, no version
        canon, ver, r = canonical, "", {}
        args.reference = True
    else:
        canon, ver, r = latest(entry, full=True)  # classifying needs the content
    entry["canonical"], entry["version"] = canon, ver
    if args.reference or is_intensional(r):
        entry["mode"] = "reference"
        print(f"  {entry['key']}: intensional or forced, registered as reference only")
    if entry["source"] == "decor" and entry["base"] == DECOR_BASE:
        del entry["base"]  # the default; keeps the file short
    append_entry(entry)
    if entry["mode"] == "download":
        write_vs(entry, r)
    print(f"  added {entry['key']} ({ver}), mode {entry['mode']}")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fetch")
    f.add_argument("--strict", action="store_true", help="exit 1 when a download fails")
    f.add_argument("--force", action="store_true", help="download again even when the pinned version is already present")
    sub.add_parser("check")
    sub.add_parser("update")
    a = sub.add_parser("add")
    a.add_argument("url", help="canonical URL of the ValueSet")
    a.add_argument("--source", choices=["decor", "fhir"], default="decor")
    a.add_argument("--base", help="server base URL (default for decor: " + DECOR_BASE + ")")
    a.add_argument("--key", help="short key used in {vs:<key>} (default: last URL segment)")
    a.add_argument("--token-env", help="environment variable holding a bearer token")
    a.add_argument("--nts", action="store_true", help="sign in to the NTS with NTS_USERNAME / NTS_PASSWORD")
    a.add_argument("--reference", action="store_true", help="never download; reference only")
    args = ap.parse_args()
    try:
        sys.exit({"fetch": cmd_fetch, "check": cmd_check, "update": cmd_update, "add": cmd_add}[args.cmd](args))
    except SyncError as e:
        sys.exit(f"ERROR: {e}")


if __name__ == "__main__":
    main()
