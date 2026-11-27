#!/usr/bin/env python3
"""
_syncDefinitions.py

Keeps the definitions that are maintained outside this repository (ValueSets and
Questionnaires in ART-DECOR, on the Nationale Terminologieserver or on another
FHIR server) in step with the version this repository has pinned.

The pinned state lives in input/fsh/definitions/external-definitions.yaml and is
committed. Data requirements and dataset definitions refer to an entry by its key,
for example {vs:ibd-diagnosis} or {q:luscii-prems}; the generator resolves that to
the pinned canonical.

  py -3 _syncDefinitions.py fetch     download the pinned version of every entry
                                    marked "download" into input/resources/
                                    (what the build runs); a version already
                                    present is skipped, --force downloads again
  py -3 _syncDefinitions.py show nza-zorgtype [text]
                                    list what a registered CodeSystem holds (as
                                    converted to FHIR): code, display, validity;
                                    `text` filters on code, display or system
  py -3 _syncDefinitions.py check     ask the source for the latest version of
                                    every entry; exit 1 when one has moved on
  py -3 _syncDefinitions.py update    move drifted entries to the latest version
                                    and download them; then regenerate
  py -3 _syncDefinitions.py add URL   register a ValueSet (see --help)
  py -3 _syncDefinitions.py add URL --type Questionnaire --key luscii-prems
                                    register a Questionnaire (ART-DECOR); data
                                    requirements refer to it as {q:<key>}

Types
  ValueSet, Questionnaire and CodeSystem; every entry states its `type`. It decides the endpoint
  (<base>/<type>/<id>) and the file name in input/resources/. A Questionnaire
  from ART-DECOR is pinned on the effective date in its canonical, because its
  `version` element is the ART-DECOR release, not the questionnaire's.

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
  nts        The Nationale Terminologieserver (base defaults to
             https://terminologieserver.nl/fhir). Searched by canonical url and
             version. Signs in the way HipsETL does (OpenID password grant, client
             cli_client) with the environment variables NTS_USERNAME and
             NTS_PASSWORD, which are never stored in this repository.
  fhir       Any other FHIR server (base required). Searched by canonical url and
             version. `tokenEnv` names an environment variable that holds a ready
             bearer token, when the server needs one.
             An implicit value set (a canonical containing "?fhir_vs=", for example a
             SNOMED ECL query) has no resource and no version of its own: it is
             "reference" mode and is skipped by check.
CodeSystems (validation of the codes the definitions use)
  An entry of type CodeSystem names where the codes of one code system
  (`canonical` = the system URL) are checked. _generateFromDefinitions.py checks
  every coding of the definitions whose system is registered; a system that is
  not registered is reported as not validated.
    mode download (default for decor; also for fhir/nts when set) the pinned
                 CodeSystem is imported into input/resources/, like a ValueSet,
                 and the codes are checked locally. For servers without
                 $lookup, such as ART-DECOR.
    mode lookup  (default for nts and fhir) every code is asked once with
                 CodeSystem/$lookup on the server; the answers are kept as a
                 FHIR CodeSystem (content fragment) in terminology-cache/, so
                 later runs need no server.
                 `version` is optional.
    mode table   (source nza) the whole NZa table is downloaded; see below.
  `display: error` makes a wrong display an error (default for table and
  download; for lookup a warning, servers often hold other wordings).
  CodeSystems defined in the FSH of this repository are checked against
  without an entry.

  nza        A code table of the NZa dbc-pakket (puc.overheid.nl), for code systems
             that no FHIR server offers: DBC-diagnoses and zorgactiviteiten.
             Only for type CodeSystem. Fields: `puc` (the document identifier,
             for example PUC_789350_22), `table` (typeringslijst-diagnose |
             typeringslijst-zorgtype | zorgactiviteiten) and `version` (the file version, for example
             20250501, as in "... v20250501.zip"). `canonical` is the code system
             URL; for DBC-diagnoses it contains {specialisme}, which becomes the
             specialism code without leading zeros (one code system per specialism).
             fetch converts the table into complete FHIR CodeSystems, with the
             validity of every code, kept as a Bundle in terminology-cache/
             (not committed). _generateFromDefinitions.py
             validates the codes of the definitions against it and generates a
             fragment CodeSystem with only the codes in use. check cannot ask the
             NZa for a newer release (no API): compare by hand with the current
             dbc-pakket.

Standard library only.
"""

import argparse
import csv
import io
import json
import os
import re
import sys
import zipfile
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent
MANIFEST = ROOT / "input" / "fsh" / "definitions" / "external-definitions.yaml"
OUT_DIR = ROOT / "input" / "resources"
DECOR_BASE = "https://decor.nictiz.nl/fhir/4.0/san-gen-"
NTS_BASE = "https://terminologieserver.nl/fhir"
TYPES = ("ValueSet", "Questionnaire", "CodeSystem")
SOURCES = ("decor", "nts", "fhir", "nza")
PUC_BASE = "https://puc.overheid.nl/PUC/Handlers/DownloadDocument.ashx"
NZA_TABLES = ("typeringslijst-diagnose", "typeringslijst-zorgtype", "zorgactiviteiten")
TYPERING_AS = {"typeringslijst-zorgtype": "1", "typeringslijst-diagnose": "3"}  # As code in the typeringslijst
CACHE_DIR = ROOT / "terminology-cache"
NTS_TOKEN_URL = "https://terminologieserver.nl/authorisation/auth/realms/nictiz/protocol/openid-connect/token"
TIMEOUT = 30


class SyncError(Exception):
    pass


def load_dotenv(path=ROOT / ".env"):
    """KEY=value lines from .env (not committed) as environment variables, e.g. NTS_USERNAME and
    NTS_PASSWORD; a variable that is already set wins."""
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return
    for line in lines:
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        k, _, v = line.removeprefix("export ").partition("=")
        v = v.strip()
        if len(v) >= 2 and v[0] == v[-1] and v[0] in "'\"":
            v = v[1:-1]
        os.environ.setdefault(k.strip(), v)


load_dotenv()


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
        explicit = set(e)
        if e.get("type") not in TYPES:
            raise SyncError(f"{MANIFEST.name}: '{e['key']}' needs a type ({' | '.join(TYPES)})")
        e.setdefault("source", "decor")
        if e["source"] not in SOURCES:
            raise SyncError(f"{MANIFEST.name}: '{e['key']}' has an unknown source '{e['source']}' ({' | '.join(SOURCES)})")
        e.setdefault("mode", "download")
        if e["source"] == "nza" and e["type"] != "CodeSystem":
            raise SyncError(f"{MANIFEST.name}: '{e['key']}': source nza is only for type CodeSystem")
        if e["type"] == "CodeSystem":
            if e["source"] == "nza":
                e["mode"] = "table"
            elif "mode" not in explicit:
                e["mode"] = "download" if e["source"] == "decor" else "lookup"
            if e["mode"] not in ("table", "download", "lookup"):
                raise SyncError(f"{MANIFEST.name}: '{e['key']}': a CodeSystem has mode download or lookup")
            e.setdefault("display", "warning" if e["mode"] == "lookup" else "error")
        if e["source"] == "nza":
            e.setdefault("base", PUC_BASE)
            if e.get("table") not in NZA_TABLES or not e.get("puc"):
                raise SyncError(f"{MANIFEST.name}: '{e['key']}' needs a puc and a table ({' | '.join(NZA_TABLES)})")
        elif e["source"] == "decor":
            e.setdefault("base", DECOR_BASE)
        elif e["source"] == "nts":
            e.setdefault("base", NTS_BASE)
        elif "base" not in e:
            raise SyncError(f"{MANIFEST.name}: '{e['key']}' has source fhir and needs a base")
        e.setdefault("version", "")
        if "canonical" not in e or (not e["version"] and e["mode"] not in ("reference", "lookup")):
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
    block = [f"{e['key']}:"] + [_fmt(f, e[f]) for f in ("type", "source", "base", "canonical", "version", "mode", "tokenEnv") if f in e]
    sep = "\n\n" if text.strip() else ""
    MANIFEST.write_text(text.rstrip("\n") + sep + "\n".join(block) + "\n", encoding="utf-8", newline="\n")


_NTS_TOKEN = {}


def nts_token():
    """Bearer token for the Nationale Terminologieserver (OpenID password grant, as HipsETL does)."""
    if "t" in _NTS_TOKEN:
        return _NTS_TOKEN["t"]
    if "error" in _NTS_TOKEN:  # sign in once per run: repeated failures can lock the account
        raise SyncError(_NTS_TOKEN["error"])
    try:
        return _nts_sign_in()
    except SyncError as e:
        _NTS_TOKEN["error"] = str(e)
        raise


def _nts_sign_in():
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
        try:
            why = json.loads(e.read().decode("utf-8")).get("error_description", "")
        except (ValueError, OSError):
            why = ""
        raise SyncError(f"NTS sign-in failed (HTTP {e.code}{': ' + why if why else ''})")
    except (urllib.error.URLError, TimeoutError, OSError, KeyError, ValueError) as e:
        raise SyncError(f"NTS sign-in failed: {type(e).__name__}")
    return _NTS_TOKEN["t"]


def bearer(entry):
    if entry and entry.get("source") == "nts":
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


def effective_version(canonical):
    """'...--20250714064425' -> '2025-07-14T06:44:25' (the ART-DECOR effective date), or ''."""
    eff = split_decor(canonical)[1] or ""
    if not re.fullmatch(r"\d{14}", eff):
        return ""
    return f"{eff[0:4]}-{eff[4:6]}-{eff[6:8]}T{eff[8:10]}:{eff[10:12]}:{eff[12:14]}"


def resource_version(entry, r):
    if entry["type"] != "ValueSet" and entry["source"] == "decor":
        return effective_version(r["url"])
    return r.get("version", "")


def split_decor(canonical):
    """'http://decor.../<Type>/<oid>[--<effective>]' -> (oid, effective or None)."""
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
    t = entry["type"]
    if entry["source"] == "decor":
        oid, _ = split_decor(entry["canonical"])
        r = get_json(f"{entry['base']}/{t}/{oid}?_format=json", entry)
        return r["url"], resource_version(entry, r), r
    base = unpinned(entry["canonical"])
    params = {"url": base, "_sort": "-date", "_count": "1"}
    if not full:
        params["_elements"] = "url,version,date"
    q = urllib.parse.urlencode(params)
    b = get_json(f"{entry['base']}/{t}?{q}", entry)
    if not b.get("entry"):
        raise SyncError(f"{base} not found on {entry['base']}")
    r = b["entry"][0]["resource"]
    return base, r.get("version", ""), r


def pinned_fetch(entry):
    if entry["source"] == "decor":
        oid, eff = split_decor(entry["canonical"])
        vid = f"{oid}--{eff}" if eff else oid
        return get_json(f"{entry['base']}/{entry['type']}/{vid}?_format=json", entry)
    q = urllib.parse.urlencode({"url": unpinned(entry["canonical"]), "version": entry["version"]})
    b = get_json(f"{entry['base']}/{entry['type']}?{q}", entry)
    if not b.get("entry"):
        raise SyncError(f"{entry['canonical']} version {entry['version']} not found")
    return b["entry"][0]["resource"]


def out_file(entry):
    """Where an entry's pinned resource is stored. ART-DECOR: the versioned id, as _fetchValueSets.ps1 names it."""
    if entry["source"] == "decor":
        oid, eff = split_decor(entry["canonical"])
        name = f"{oid}--{eff}" if eff else oid
    else:
        name = entry["key"]
    return OUT_DIR / (f"{entry['type']}-" + re.sub(r"[^a-zA-Z0-9._-]", "-", name) + ".json")


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


def fix_contained_ids(r):
    """ART-DECOR can export a contained resource whose id is several ids separated by spaces (a value set
    reused by several items). A FHIR id has no spaces, and the IG Publisher names a page after it, which makes
    Jekyll fail. Keep the first id and point every '#old' reference at it."""
    renamed = {}
    for c in r.get("contained", []):
        old = c.get("id", "")
        if re.search(r"\s", old):
            c["id"] = old.split()[0]
            renamed["#" + old] = "#" + c["id"]
    if not renamed:
        return r

    def walk(x):
        if isinstance(x, dict):
            return {k: walk(v) for k, v in x.items()}
        if isinstance(x, list):
            return [walk(v) for v in x]
        return renamed.get(x, x) if isinstance(x, str) else x
    return walk(r)


def write_vs(entry, r):
    r = fix_contained_ids(r)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    f = out_file(entry)
    f.write_text(json.dumps(r, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")
    return f


# ---------------------------------------------------------------------------
# NZa code tables (source nza): DBC-diagnoses and zorgactiviteiten with their validity periods
def cache_file(entry):
    """The FHIR CodeSystems converted from an NZa table, as one Bundle: terminology-cache/<key>-<version>.json
    (not committed; the full tables are only validation input)."""
    return CACHE_DIR / f"{entry['key']}-{entry['version']}.json"


def _nza_date(s):
    s = (s or "").strip()
    if not s:
        return None
    if not re.fullmatch(r"\d{8}", s):
        raise SyncError(f"unexpected date {s!r} in the NZa table")
    return f"{s[0:4]}-{s[4:6]}-{s[6:8]}"


def get_bytes(url):
    req = urllib.request.Request(url, headers={"User-Agent": "sim-on-fhir-sync/1.0"})
    for attempt in (1, 2):
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT * 4) as r:
                return r.headers.get("Content-Disposition", ""), r.read()
        except urllib.error.HTTPError as e:
            raise SyncError(f"HTTP {e.code} for {url}")
        except (urllib.error.URLError, TimeoutError, OSError) as e:
            if attempt == 2:
                raise SyncError(f"cannot reach {url}: {e}")


def nza_rows(entry):
    """(file name, rows) of the csv in the pinned NZa zip; fails when the zip holds another version than pinned."""
    disposition, data = get_bytes(f"{entry['base']}?identifier={entry['puc']}&versienummer=1")
    try:
        z = zipfile.ZipFile(io.BytesIO(data))
    except zipfile.BadZipFile:
        raise SyncError(f"{entry['puc']} is not a zip file ({disposition or 'no file name'})")
    names = [n for n in z.namelist() if n.lower().endswith(".csv")]
    if len(names) != 1:
        raise SyncError(f"{entry['puc']}: expected one csv in the zip, found {names}")
    name = names[0]
    if f"v{entry['version']}" not in name:
        raise SyncError(f"{entry['puc']} holds '{name}', not version v{entry['version']}")
    raw = z.read(name)
    try:
        text = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        text = raw.decode("cp1252", errors="replace")  # the NZa writes Windows-1252
    return name, list(csv.DictReader(io.StringIO(text), delimiter=";"))


def _column(row, *names):
    for n in names:
        if n in row:
            return (row[n] or "").strip()
    raise SyncError(f"column {names[0]!r} not found in the NZa table (columns: {', '.join(row)})")


def nza_fetch(entry):
    """Convert a pinned NZa table into FHIR CodeSystems (one per specialism for DBC-diagnoses), kept as a Bundle."""
    name, rows = nza_rows(entry)
    systems = {}
    for row in rows:
        if entry["table"].startswith("typeringslijst-"):
            # As 1 = zorgtype, 2 zorgvraag, 3 diagnose, 4 behandeling
            if _column(row, "As code") != TYPERING_AS[entry["table"]]:
                continue
            spec = _column(row, "Specialisme code AGB")
            if "{specialisme}" in entry["canonical"]:  # one code system per specialism (diagnoses)
                system, label = entry["canonical"].replace("{specialisme}", str(int(spec))), spec
            else:  # the same codes in every specialism (zorgtype): one code system, rows merged
                system, label = entry["canonical"], ""
            code = _column(row, "Component Code")
            display = _column(row, "Component omschrijving lang") or _column(row, "Component omschrijving kort")
            start, end = _column(row, "Ingangsdatum"), _column(row, "Afloopdatum")
        else:
            system, label = entry["canonical"], ""
            code = _column(row, "Zorgactiviteit_code")
            display = _column(row, "Zorgactiviteit_omschrijving")
            start, end = _column(row, "Ingangsdatum"), _column(row, "Einddatum")
        if not code:
            continue
        s = systems.setdefault(system, {"label": label, "concepts": {}})
        period = [_nza_date(start), _nza_date(end), display]
        if period not in s["concepts"].setdefault(code, []):  # merged specialisms repeat the same rows
            s["concepts"][code].append(period)
    if not systems:
        raise SyncError(f"no codes found in '{name}'")
    bundle = {"resourceType": "Bundle", "type": "collection",
              "entry": [{"resource": nza_codesystem(entry, system, s["label"], s["concepts"], name)}
                        for system, s in sorted(systems.items())]}
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    f = cache_file(entry)
    f.write_text(json.dumps(bundle, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")
    return f, sum(len(s["concepts"]) for s in systems.values())


CONCEPT_PROPERTIES = "http://hl7.org/fhir/concept-properties"
PREVIOUS_DESCRIPTION = "eerdere NZa-omschrijving"  # designation.use.display of an older description


def nza_codesystem(entry, system, label, concepts, file_name):
    """One complete FHIR CodeSystem from the rows of an NZa table. Per concept: the current description
    as display, older descriptions as designations, the first start date (effectiveDate), the last end
    date (retirementDate) and the status on the day of conversion."""
    import datetime
    today = datetime.date.today().isoformat()
    what = {"typeringslijst-diagnose": f"DBC-diagnosen specialisme {label}",
            "typeringslijst-zorgtype": "Zorgtypen", "zorgactiviteiten": "Zorgactiviteiten"}[entry["table"]]
    cid = entry["key"] + (f"-{label}" if label else "")
    out = []
    for code, periods in sorted(concepts.items()):
        periods.sort(key=lambda p: (p[0] or "", p[1] or "9999"))
        start, display = periods[0][0], periods[-1][2]
        end = None if any(p[1] is None for p in periods) else max(p[1] for p in periods)
        c = {"code": code, "display": display}
        older = []
        for _, _, d in periods[:-1]:
            if d and d != display and d not in older:
                older.append(d)
        if older:
            c["designation"] = [{"use": {"display": PREVIOUS_DESCRIPTION}, "value": d} for d in older]
        props = [{"code": "effectiveDate", "valueDateTime": start}] if start else []
        if end:
            props.append({"code": "retirementDate", "valueDateTime": end})
        props.append({"code": "status", "valueCode": "retired" if end and end < today else "active"})
        c["property"] = props
        out.append(c)
    return {
        "resourceType": "CodeSystem", "id": cid, "url": system, "version": entry["version"],
        "name": "".join(w[:1].upper() + w[1:] for w in re.split(r"[^A-Za-z0-9]+", cid) if w),
        "title": f"{what} (NZa)", "status": "active", "experimental": False,
        "publisher": "Nederlandse Zorgautoriteit (NZa)",
        "description": f"{what} uit het NZa dbc-pakket, bestand '{file_name}' ({entry['puc']}). "
                       f"Omgezet door _syncDefinitions.py. De OID is een aanname.",
        "caseSensitive": True, "content": "complete", "count": len(out),
        "property": [
            {"code": "effectiveDate", "uri": f"{CONCEPT_PROPERTIES}#effectiveDate", "type": "dateTime",
             "description": "Eerste ingangsdatum van de code in de NZa-tabel"},
            {"code": "retirementDate", "uri": f"{CONCEPT_PROPERTIES}#retirementDate", "type": "dateTime",
             "description": "Laatste afloopdatum van de code in de NZa-tabel (ontbreekt: nog geldig)"},
            {"code": "status", "uri": f"{CONCEPT_PROPERTIES}#status", "type": "code",
             "description": "active of retired, op de dag van omzetten"}],
        "concept": out}


def lookup(entry, code):
    """CodeSystem/$lookup of one code on the entry's server.

    Returns dict(found, display, designations, inactive, message). Raises SyncError when the
    server cannot answer (unreachable, no sign-in, 5xx), so the code stays not validated."""
    params = {"system": entry["canonical"], "code": code}
    if entry.get("version"):
        params["version"] = entry["version"]
    url = f"{entry['base']}/CodeSystem/$lookup?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"Accept": "application/fhir+json, application/json",
                                               "User-Agent": "sim-on-fhir-sync/1.0"})
    token = bearer(entry)
    if token:
        req.add_header("Authorization", f"Bearer {token}")
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
            body = json.loads(r.read().decode("utf-8"))
    except urllib.error.HTTPError as e:
        if e.code in (400, 404, 422):  # the server answered: the code is not in the code system
            try:
                oo = json.loads(e.read().decode("utf-8"))
                msg = "; ".join(i.get("diagnostics") or i.get("details", {}).get("text", "") for i in oo.get("issue", []))
            except (ValueError, OSError):
                msg = ""
            return dict(found=False, display=None, designations=[], inactive=False, message=msg or f"HTTP {e.code}")
        raise SyncError(f"HTTP {e.code} for {url}")
    except (urllib.error.URLError, TimeoutError, OSError, ValueError) as e:
        raise SyncError(f"cannot reach {entry['base']}: {e}")
    if body.get("resourceType") != "Parameters":  # e.g. ART-DECOR answers $lookup with a search Bundle
        raise SyncError(f"{entry['base']} does not support CodeSystem/$lookup (answered with {body.get('resourceType')})")
    out = dict(found=True, display=None, designations=[], inactive=False, message="")
    for prm in body.get("parameter", []):
        name = prm.get("name")
        if name == "display":
            out["display"] = prm.get("valueString")
        elif name == "designation":
            out["designations"] += [x.get("valueString") for x in prm.get("part", []) if x.get("name") == "value"]
        elif name == "property":
            parts = {x.get("name"): x for x in prm.get("part", [])}
            pc = parts.get("code", {}).get("valueCode")
            val = parts.get("value", {})
            if pc == "inactive" and val.get("valueBoolean") is True:
                out["inactive"] = True
            if pc in ("status", "STATUS") and str(val.get("valueCode", val.get("valueString", ""))).upper() in (
                    "RETIRED", "DEPRECATED", "INACTIVE"):
                out["inactive"] = True
    return out


# ---------------------------------------------------------------------------
def cmd_fetch(args):
    failed = 0
    for e in load()["valuesets"]:
        if e["mode"] == "lookup":
            print(f"  {e['key']}: codes are looked up one by one by the generator, nothing to download")
            continue
        if e["mode"] == "table":
            if not getattr(args, "force", False) and cache_file(e).exists():
                print(f"  {e['key']}: already downloaded ({e['version']})")
                continue
            try:
                f, n = nza_fetch(e)
                print(f"  {e['key']}: {f.relative_to(ROOT)} ({n} codes)")
            except SyncError as ex:
                failed += 1
                print(f"  {e['key']}: FAILED - {ex}")
            continue
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


def cmd_show(args):
    """List the codes of a registered CodeSystem, as the generator validates against them."""
    entries = {e["key"]: e for e in load()["valuesets"] if e["type"] == "CodeSystem"}
    e = entries.get(args.key)
    if not e:
        raise SyncError(f"no CodeSystem '{args.key}' in {MANIFEST.name} (registered: {', '.join(entries) or 'none'})")
    if e["mode"] == "table":
        f = cache_file(e)
        if not f.exists():
            raise SyncError(f"'{e['key']}' is not downloaded: run py -3 _syncDefinitions.py fetch")
        systems = [x["resource"] for x in json.loads(f.read_text(encoding="utf-8"))["entry"]]
    elif e["mode"] == "download":
        f = out_file(e)
        if not f.exists():
            raise SyncError(f"'{e['key']}' is not downloaded: run py -3 _syncDefinitions.py fetch")
        systems = [json.loads(f.read_text(encoding="utf-8"))]
    else:
        f = CACHE_DIR / f"CodeSystem-lookup-{e['key']}.json"
        if not f.exists():
            print(f"  {e['key']}: looked up per code on {e['base']}; nothing looked up yet")
            return 0
        systems = [json.loads(f.read_text(encoding="utf-8"))]
        print(f"  {e['key']}: only the codes looked up so far (fragment)")
    text = (args.text or "").lower()
    shown = 0
    for cs in systems:
        rows = []
        for c in cs.get("concept", []):
            props = {p["code"]: next((v for k, v in p.items() if k.startswith("value")), None)
                     for p in c.get("property", [])}
            hay = f"{cs['url']} {c['code']} {c.get('display', '')}".lower()
            if text and text not in hay:
                continue
            valid = f"{props.get('effectiveDate') or '?'} to {props.get('retirementDate') or 'now'}"
            status = props.get("status") or ("inactive" if props.get("inactive") else "")
            rows.append((c["code"], valid, status, c.get("display", "")))
        if not rows:
            continue
        print(f"\n{cs.get('title') or cs.get('id')}  {cs['url']}  (version {cs.get('version', '-')}, {len(rows)} codes)")
        w = max(len(r[0]) for r in rows)
        for code, valid, status, display in rows:
            print(f"  {code:<{w}}  {valid:<26}  {status:<8}  {display}")
        shown += len(rows)
    if not shown:
        print(f"  no codes{' matching ' + repr(args.text) if args.text else ''}")
    return 0


def cmd_check(args):
    drift = err = 0
    for e in load()["valuesets"]:
        if "fhir_vs=" in e["canonical"]:
            print(f"  {e['key']}: implicit value set (no version of its own), nothing to check")
            continue
        if e["mode"] == "lookup":
            print(f"  {e['key']}: looked up per code, nothing to check")
            continue
        if e["source"] == "nza":
            print(f"  {e['key']}: NZa has no version API; pinned v{e['version']} ({e['puc']}), "
                  f"compare by hand with the current dbc-pakket on puc.overheid.nl")
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
        if "fhir_vs=" in e["canonical"] or e["mode"] in ("lookup", "table"):
            continue
        try:
            canon, ver, r = latest(e)
        except SyncError as ex:
            print(f"  {e['key']}: UNREACHABLE - {ex}")
            continue
        if canon == e["canonical"] and ver == e["version"]:
            continue
        if e["type"] == "ValueSet" and e["mode"] == "download" and "compose" not in r:
            r = latest(e, full=True)[2]  # the light check had no content; classify on the full resource
        if e["type"] == "ValueSet" and e["mode"] == "download" and is_intensional(r):
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
        entry = dict(key=args.key or oid, type=args.type, source="decor", base=base, canonical=canonical, version="",
                     mode="download")
    else:
        base = args.base or (NTS_BASE if args.source == "nts" else None)
        if not base:
            raise SyncError("--base is required for --source fhir")
        entry = dict(key=args.key or canonical.rstrip("/").split("/")[-1], type=args.type, source=args.source,
                     base=base.rstrip("/"), canonical=canonical, version="", mode="download")
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
    if args.reference or (entry["type"] == "ValueSet" and is_intensional(r)):
        entry["mode"] = "reference"
        print(f"  {entry['key']}: intensional or forced, registered as reference only")
    if (entry["source"], entry["base"]) in (("decor", DECOR_BASE), ("nts", NTS_BASE)):
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
    sh = sub.add_parser("show", help="list the codes of a registered CodeSystem")
    sh.add_argument("key", help="key of the CodeSystem in external-definitions.yaml, e.g. nza-zorgtype")
    sh.add_argument("text", nargs="?", help="only codes whose code, display or system contains this text")
    sub.add_parser("check")
    sub.add_parser("update")
    a = sub.add_parser("add")
    a.add_argument("url", help="canonical URL of the ValueSet or Questionnaire")
    a.add_argument("--type", choices=["ValueSet", "Questionnaire"], default="ValueSet")
    a.add_argument("--source", choices=list(SOURCES), default="decor")
    a.add_argument("--base", help=f"server base URL (default for decor: {DECOR_BASE}, for nts: {NTS_BASE})")
    a.add_argument("--key", help="short key used in {vs:<key>} (default: last URL segment)")
    a.add_argument("--token-env", help="environment variable holding a bearer token")
    a.add_argument("--reference", action="store_true", help="never download; reference only")
    args = ap.parse_args()
    import _log
    _log.install()
    try:
        sys.exit({"fetch": cmd_fetch, "show": cmd_show, "check": cmd_check, "update": cmd_update, "add": cmd_add}[args.cmd](args))
    except SyncError as e:
        sys.exit(f"ERROR: {e}")


if __name__ == "__main__":
    main()
