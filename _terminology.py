#!/usr/bin/env python3
"""
_terminology.py

Validates every code the definitions use against the code system it claims to
come from. Used by _generateFromDefinitions.py.

Everything is translated to FHIR first; validation only reads FHIR CodeSystems:

  source                         FHIR CodeSystem it validates against
  NZa table (source nza)         complete CodeSystems converted by
                                 `_syncDefinitions.py fetch` (Bundle in terminology-cache/)
  ART-DECOR / FHIR server        the pinned CodeSystem imported by fetch into
  (mode download)                input/resources/, like a ValueSet
  NTS / FHIR server              a fragment CodeSystem in terminology-cache/, grown from
  (mode lookup)                  the $lookup answers; a code not in it yet is looked up
                                 once (--offline: never)
  this repository                every complete CodeSystem defined in the FSH
  FHIR packages                  every complete CodeSystem in hl7.fhir.r4.core and the
                                 dependencies of sushi-config.yaml (e.g. hl7.terminology.r4),
                                 exactly the versions SUSHI uses; no entry needed

  A system with none of these is reported as not validated (one warning per system).

Which codes
  - the codes in the search parameters of data requirements and datasets, with the
    criterion's lower date bound (a `ge<date>` parameter) when it has one;
  - every coding in the ObservationDefinitions (code, unit, components);
  - the codes listed in the local ValueSets.

Outcome per code
  error    the code is not in the code system (for a fragment: the server said so)
  error    the code is valid at no moment between the lower date bound and today
           (effectiveDate / retirementDate of the concept)
  error    the display is neither the concept's display nor one of its designations,
           when the entry has `display: error` (default, except for lookups: warning)
  warning  the display is an older description (designation "eerdere NZa-omschrijving")
  warning  the code has expired, or is retired / inactive
  warning  the code could not be checked (server unreachable, no sign-in)
A coding without display in a data requirement gets the code system's display.

For the NZa code systems a fragment CodeSystem with the codes in use is generated
(FSH), so the IG Publisher can validate against it too; no FHIR server offers them.

Standard library only.
"""

import datetime
import json
import re
from pathlib import Path

DATE_LOWER = re.compile(r"^ge(\d{4}-\d{2}-\d{2})")
PREVIOUS_DESCRIPTION = "eerdere NZa-omschrijving"


class TerminologyError(Exception):
    pass


# ---------------------------------------------------------------------------
# One FHIR CodeSystem as validation source
# ---------------------------------------------------------------------------
def _prop(concept, name):
    for p in concept.get("property", []):
        if p.get("code") == name:
            return next((v for k, v in p.items() if k.startswith("value")), None)
    return None


def _index(concepts, out=None):
    """{code: concept}, nested concepts included."""
    out = {} if out is None else out
    for c in concepts:
        out[c["code"]] = c
        _index(c.get("concept", []), out)
    return out


class CodeSystemSource:
    """Validates codes against one FHIR CodeSystem. With `lookup`, a code that is not in it is asked
    from the server and added (the CodeSystem is then a fragment that grows, saved by save())."""

    def __init__(self, cs, entry, where, lookup=None, cache_path=None, offline=False):
        self.cs, self.entry, self.where = cs, entry, where
        self.concepts = _index(cs.get("concept", []))
        self.lookup, self.cache_path, self.offline = lookup, cache_path, offline
        self.failure, self.dirty = None, False

    def name(self):
        return self.cs.get("id") or self.entry["key"]

    def fragment(self):
        return self.entry.get("source") == "nza"

    def get(self, code):
        c = self.concepts.get(code)
        if c is None and self.lookup:
            if self.offline or self.failure:
                return dict(unchecked=self.failure or "offline, not looked up yet")
            try:
                r = self.lookup(self.entry, code)
            except Exception as e:  # SyncError from _syncDefinitions: unreachable, no sign-in, no $lookup
                self.failure = f"{self.entry['base']}: {e}"
                return dict(unchecked=self.failure)
            if not r["found"]:
                return dict(found=False, message=r.get("message") or f"not found on {self.entry['base']}")
            c = {"code": code, "display": r.get("display") or ""}
            if r.get("designations"):
                c["designation"] = [{"value": d} for d in r["designations"] if d]
            c["property"] = [{"code": "inactive", "valueBoolean": bool(r.get("inactive"))},
                             {"code": "checked", "valueDateTime": datetime.date.today().isoformat()}]
            self.cs.setdefault("concept", []).append(c)
            self.concepts[code] = c
            self.dirty = True
        if c is None:
            return dict(found=False, message=f"not in {self.where}")
        older, other = [], []
        for d in c.get("designation", []):
            (older if d.get("use", {}).get("display") == PREVIOUS_DESCRIPTION else other).append(d.get("value"))
        start, end = _prop(c, "effectiveDate"), _prop(c, "retirementDate") or _prop(c, "expirationDate")
        status = str(_prop(c, "status") or "").lower()
        return dict(found=True, display=c.get("display"), older=set(older), designations=other,
                    start=(start or "")[:10] or None, end=(end or "")[:10] or None,
                    inactive=_prop(c, "inactive") is True or status in ("retired", "deprecated", "inactive"))

    def save(self):
        if self.dirty and self.cache_path:
            self.cs["count"] = len(self.cs.get("concept", []))
            self.cache_path.parent.mkdir(parents=True, exist_ok=True)
            self.cache_path.write_text(json.dumps(self.cs, ensure_ascii=False, indent=1) + "\n",
                                       encoding="utf-8", newline="\n")


def _lookup_fragment(entry, cache_dir):
    """The CodeSystem that holds the $lookup answers of an entry (empty fragment when new)."""
    path = Path(cache_dir) / f"CodeSystem-lookup-{entry['key']}.json"
    try:
        cs = json.loads(path.read_text(encoding="utf-8"))
        if cs.get("url") == entry["canonical"] and cs.get("version", "") == entry.get("version", ""):
            return cs, path
    except (OSError, ValueError):
        pass
    cs = {"resourceType": "CodeSystem", "id": f"lookup-{entry['key']}", "url": entry["canonical"],
          "status": "active", "content": "fragment",
          "description": f"Codes checked with CodeSystem/$lookup on {entry['base']}; built by _terminology.py.",
          "property": [{"code": "inactive", "uri": "http://hl7.org/fhir/concept-properties#inactive", "type": "boolean"},
                       {"code": "checked", "type": "dateTime", "description": "Date of the $lookup"}],
          "concept": []}
    if entry.get("version"):
        cs["version"] = entry["version"]
    return cs, path


def package_codesystems(package_dirs, needed):
    """{url: CodeSystem} of the complete CodeSystems in FHIR packages (the IG's FHIR version and its
    dependencies, as SUSHI downloaded them) whose url is in `needed`; read via each package's .index.json."""
    out = {}
    for d in package_dirs:
        try:
            index = json.loads((Path(d) / ".index.json").read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        for f in index.get("files", []):
            if f.get("resourceType") == "CodeSystem" and f.get("url") in needed and f["url"] not in out:
                cs = json.loads((Path(d) / f["filename"]).read_text(encoding="utf-8"))
                if cs.get("content", "complete") == "complete":
                    out[f["url"]] = (cs, Path(d).parent.name)
    return out


def load_sources(entries, cache_file, cache_dir, lookup, downloaded, local=(), offline=False,
                 package_dirs=(), needed=()):
    """{system: CodeSystemSource} for every registered CodeSystem, every complete CodeSystem defined in
    this repository and, for the systems in `needed`, every complete CodeSystem in the FHIR packages.
    A registered entry wins over a package, a package over nothing."""
    sources = {}
    for url, (cs, pkg) in package_codesystems(package_dirs, set(needed)).items():
        sources[url] = CodeSystemSource(cs, dict(key=cs.get("id", url), display="error"), f"package {pkg}")
    for cs in local:
        if cs.get("url") and cs.get("content", "complete") == "complete":
            sources[cs["url"]] = CodeSystemSource(cs, dict(key=cs["id"], display="error"),
                                                  f"local CodeSystem {cs['id']}")
    for e in entries:
        if e["mode"] == "lookup":
            cs, path = _lookup_fragment(e, cache_dir)
            sources[e["canonical"]] = CodeSystemSource(cs, e, f"{e['base']}", lookup, path, offline)
        elif e["mode"] == "download":
            cs = downloaded(e)
            if cs is None:
                raise TerminologyError(f"CodeSystem '{e['key']}' is registered but not downloaded: "
                                       f"run py -3 _syncDefinitions.py fetch")
            system = cs.get("url") or e["canonical"]
            sources[system] = CodeSystemSource(cs, e, f"CodeSystem {system} ({e['source']}, {e['version']})")
        else:  # table: the Bundle of CodeSystems converted from the NZa table
            f = cache_file(e)
            if not f.exists():
                raise TerminologyError(f"CodeSystem '{e['key']}' (v{e['version']}) is not downloaded: "
                                       f"run py -3 _syncDefinitions.py fetch")
            for be in json.loads(f.read_text(encoding="utf-8")).get("entry", []):
                cs = be["resource"]
                sources[cs["url"]] = CodeSystemSource(cs, e, f"NZa v{e['version']}")
    return sources


def save_sources(sources):
    for s in sources.values():
        s.save()


# ---------------------------------------------------------------------------
# Collecting the codes of the definitions
# ---------------------------------------------------------------------------
def _lower_bound(criterion):
    for p in criterion.get("param", []):
        m = DATE_LOWER.match(p.get("value", ""))
        if m:
            return m.group(1)
    return None


def _codings(x):
    """Every Coding-like {system, code} in a resource, walked recursively."""
    if isinstance(x, dict):
        if isinstance(x.get("system"), str) and isinstance(x.get("code"), str):
            yield x
        for v in x.values():
            yield from _codings(v)
    elif isinstance(x, list):
        for v in x:
            yield from _codings(v)


def collect(defs, split_token, ref):
    """[(owner, system, code, display or None, lower date bound or None, display key or None)].

    The display key is set for the codes of search parameters: their display lives in
    defs['display'] and is filled in there when missing."""
    items = []
    owners = [(f"requirement {i}", r.get("criteria", [])) for i, r in defs["req"].items()] + \
             [(f"dataset {i}", d.get("include", [])) for i, d in defs["dataset"].items()]
    for owner, criteria in owners:
        for c in criteria:
            lower = _lower_bound(c)
            for p in c.get("param", []):
                for tok in ref.sub("", p.get("value", "")).split(","):
                    tok = tok.strip()
                    system, code = split_token(tok) if tok else (None, None)
                    if system and ("://" in system or system.startswith("urn:")):
                        items.append((f"{owner} > {c['resource']}.{p['name']}", system, code,
                                      defs["display"].get((system, code)), lower, (system, code)))
    for od_id, od in defs["od"].items():
        for c in _codings(od):
            items.append((f"ObservationDefinition {od_id}", c["system"], c["code"], c.get("display"), None, None))
    for vs_id, vs in defs["vs"].items():
        for inc in vs.get("compose", {}).get("include", []) + vs.get("compose", {}).get("exclude", []):
            for con in inc.get("concept", []):
                if inc.get("system"):
                    items.append((f"ValueSet {vs_id}", inc["system"], con["code"], con.get("display"), None, None))
    return items


# ---------------------------------------------------------------------------
# Validating
# ---------------------------------------------------------------------------
def validate(items, sources, display, today=None):
    """Returns (errors, warnings, used) where used = {system: set of codes} of the NZa code systems."""
    today = (today or datetime.date.today()).isoformat()
    errors, warnings, used, seen = [], [], {}, set()
    unvalidated, unchecked = {}, {}
    # registered families of code systems, e.g. urn:oid:...27.15.4.{specialisme}: a system that fits the
    # pattern but is not in the table (a specialism that does not exist) is an error, not "not validated"
    families = {}
    for s in sources.values():
        canon = s.entry.get("canonical", "")
        if "{" in canon:
            pat = "^" + re.sub(r"\\\{[^}]*\\\}", "[^/|]+", re.escape(canon)) + "$"
            families[pat] = s.entry
    for owner, system, code, given, lower, dkey in items:
        src = sources.get(system)
        if not src:
            fam = next((e for pat, e in families.items() if re.match(pat, system)), None)
            if fam:
                errors.append(f"{owner} > {system}: not a code system of {fam['key']} "
                              f"v{fam['version']}; check the specialism in the system URL")
            else:
                unvalidated.setdefault(system, set()).add(code)
            continue
        where = f"{owner} > {system}|{code} ({src.name()}):"
        info = src.get(code)
        if "unchecked" in info:
            unchecked.setdefault(info["unchecked"], set()).add(f"{system}|{code}")
            continue
        if not info["found"]:
            errors.append(f"{where} does not exist, {info['message']}")
            continue
        if src.fragment():
            used.setdefault(system, set()).add(code)
        start, end = info["start"], info["end"]
        if (end and lower and end < lower) or (start and start > today):
            errors.append(f"{where} not valid between {lower or 'ever'} and today (valid {start or '?'} to {end or 'now'})")
        if (owner, system, code) in seen:
            continue
        seen.add((owner, system, code))
        current = info["display"]
        if not given:
            if dkey and current:
                display[dkey] = current
        elif current and given != current and given not in info["designations"]:
            if given in info["older"]:
                warnings.append(f"{where} display is an older description; current: \"{current}\"")
            elif given.casefold() in {d.casefold() for d in [current, *info["designations"]] if d}:
                warnings.append(f"{where} display \"{given}\" differs only in case from \"{current}\"")
            else:
                msg = f"{where} display \"{given}\" does not match \"{current}\""
                (errors if src.entry.get("display", "error") == "error" else warnings).append(msg)
        if end and end < today:
            warnings.append(f"{where} expired on {end}")
        elif info["inactive"]:
            warnings.append(f"{where} inactive in the code system")
    for system, codes in sorted(unvalidated.items()):
        warnings.append(f"not validated: {system} ({len(codes)} codes), no source; register it in "
                        f"external-definitions.yaml")
    for reason, codes in unchecked.items():
        warnings.append(f"not validated: {len(codes)} codes, {reason}")
    return errors, warnings, used


# ---------------------------------------------------------------------------
# Fragment CodeSystems (FSH) for the NZa code systems
# ---------------------------------------------------------------------------
def _q(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def gen_fragments(sources, used, header):
    """FSH with one fragment of each used NZa CodeSystem: the codes in use, as converted."""
    L = [f"// {header}", "",
         "// " + "=" * 69,
         "// Code systems maintained by the NZa (dbc-pakket), as far as the definitions use them.",
         "// content = fragment: the complete CodeSystem is converted from the pinned NZa file",
         "// (external-definitions.yaml) by _syncDefinitions.py fetch.",
         "// " + "=" * 69, ""]
    for system in sorted(used):
        cs = sources[system].cs
        L += [f"CodeSystem: {cs['name']}", f"Id: {cs['id']}",
              f"Title: {_q(cs['title'] + ', fragment')}",
              f"Description: {_q(cs['description'] + ' Alleen de codes die de definities gebruiken.')}",
              f"* ^url = {_q(cs['url'])}",
              f"* ^version = {_q(cs['version'])}",
              f"* ^status = #{cs['status']}",
              "* ^experimental = false",
              f"* ^publisher = {_q(cs['publisher'])}",
              "* ^caseSensitive = true",
              "* ^content = #fragment",
              f"* ^count = {len(used[system])}"]
        for i, p in enumerate(cs.get("property", [])):
            L += [f"* ^property[{i}].code = #{p['code']}", f"* ^property[{i}].uri = {_q(p['uri'])}",
                  f"* ^property[{i}].description = {_q(p['description'])}", f"* ^property[{i}].type = #{p['type']}"]
        for code in sorted(used[system]):
            c = sources[system].concepts[code]
            L.append(f"* #{code} {_q(c['display'])}")
            for i, d in enumerate(c.get("designation", [])):
                L += [f"* #{code} ^designation[{i}].use.display = {_q(d['use']['display'])}",
                      f"* #{code} ^designation[{i}].value = {_q(d['value'])}"]
            for i, p in enumerate(c.get("property", [])):
                key = next(k for k in p if k.startswith("value"))
                val = f"#{p[key]}" if key == "valueCode" else _q(p[key])
                L += [f"* #{code} ^property[{i}].code = #{p['code']}", f"* #{code} ^property[{i}].{key} = {val}"]
        L.append("")
    return "\n".join(L)
