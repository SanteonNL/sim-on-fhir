#!/usr/bin/env python3
"""
_release.py

GoReleaser-style versioning and packaging for the IG. Go injects the version at
build time (-X main.version={{.Version}}); a FHIR IG gets it from sushi-config.yaml,
so this script writes the version there before SUSHI runs, and after the build it
renames the output into versioned release files, as GoReleaser does in dist/.

Where the version comes from (the git tag is the single source of truth):

  tag v0.2.0         -> 0.2.0                      releaseLabel: release
  tag v0.2.0-rc.1    -> 0.2.0-rc.1                 releaseLabel: release candidate
  no tag (snapshot)  -> 0.2.0-snapshot-<shortsha>  releaseLabel: ci-build
                        (base = version in sushi-config.yaml)

  py -3 _release.py version          print the version this commit builds as
  py -3 _release.py stamp            write that version into sushi-config.yaml
                                     (CI does this; locally, revert it afterwards
                                     with `git checkout sushi-config.yaml`)
  py -3 _release.py dist             package output/ into dist/:
      santeon.sim-on-fhir-<version>.tgz   the FHIR package (output/package.tgz)
      sim-on-fhir_<version>_site.zip      the rendered IG website
      checksums.txt                       sha256 of the files above

  --tag v0.2.0-rc.1   use this tag instead of detecting it (GITHUB_REF_NAME, or a
                      tag that points exactly at HEAD)

`dist` takes the version from the package itself, so the file name always says what
is inside. With a tag it also fails when the package version differs from the tag.

Standard library only.
"""

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tarfile
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
CONFIG = ROOT / "sushi-config.yaml"
OUTPUT = ROOT / "output"
DIST = ROOT / "dist"
PROJECT = "sim-on-fhir"
TAG_RE = re.compile(r"^v(\d+\.\d+\.\d+)(-rc\.\d+)?$")
# Files in output/ that are only useful locally (same list as _prepare_publish_output.ps1)
SITE_EXCLUDE = {"qa.json", "qa-time-report.json", "qa-time-report.tsv", "package.db", "full-ig.zip"}


def git(*args):
    r = subprocess.run(["git", *args], cwd=ROOT, capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else ""


def detect_tag(explicit):
    if explicit:
        return explicit
    if os.environ.get("GITHUB_REF_TYPE") == "tag":
        return os.environ.get("GITHUB_REF_NAME", "")
    return git("describe", "--tags", "--exact-match", "HEAD")


def config_version():
    m = re.search(r"^version:\s*(.+?)\s*$", CONFIG.read_text(encoding="utf-8"), re.M)
    if not m:
        sys.exit("ERROR: no version: in sushi-config.yaml")
    return m.group(1).strip("\"'")


def resolve(explicit_tag=None):
    """Return (version, releaseLabel, tag or None)."""
    tag = detect_tag(explicit_tag)
    if tag:
        m = TAG_RE.match(tag)
        if not m:
            sys.exit(f"ERROR: tag '{tag}' is not vMAJOR.MINOR.PATCH or vMAJOR.MINOR.PATCH-rc.N")
        version = tag[1:]
        return version, ("release candidate" if m.group(2) else "release"), tag
    base = config_version().split("-")[0].split("+")[0]
    sha = os.environ.get("GITHUB_SHA", "")[:7] or git("rev-parse", "--short=7", "HEAD") or "local"
    return f"{base}-snapshot-{sha}", "ci-build", None


def set_key(text, key, value):
    line = f"{key}: {value}"
    pattern = re.compile(rf"^{key}:.*$", re.M)
    if pattern.search(text):
        return pattern.sub(line, text, count=1)
    return text.rstrip() + f"\n{line}\n"


def cmd_version(args):
    print(resolve(args.tag)[0])


def cmd_stamp(args):
    version, label, tag = resolve(args.tag)
    with open(CONFIG, encoding="utf-8", newline="") as f:  # keep the file's line endings
        text = f.read()
    text = set_key(text, "version", version)
    text = set_key(text, "releaseLabel", label)
    with open(CONFIG, "w", encoding="utf-8", newline="") as f:
        f.write(text)
    print(f"sushi-config.yaml: version {version}, releaseLabel {label}" + (f" (tag {tag})" if tag else " (snapshot)"))
    gh_out = os.environ.get("GITHUB_OUTPUT")
    if gh_out:
        with open(gh_out, "a", encoding="utf-8") as f:
            f.write(f"version={version}\n")


def package_meta(tgz):
    with tarfile.open(tgz, "r:gz") as t:
        member = t.extractfile("package/package.json")
        if member is None:
            sys.exit(f"ERROR: {tgz} has no package/package.json")
        meta = json.load(member)
    return meta["name"], meta["version"]


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def cmd_dist(args):
    tgz = OUTPUT / "package.tgz"
    if not tgz.exists():
        sys.exit("ERROR: output/package.tgz not found. Build the IG first.")
    name, version = package_meta(tgz)

    tag = detect_tag(args.tag)
    if tag and tag[1:] != version:
        sys.exit(f"ERROR: package version {version} does not match tag {tag}. Run `_release.py stamp` before building.")

    if DIST.exists():
        shutil.rmtree(DIST)
    DIST.mkdir()

    files = []
    pkg = DIST / f"{name}-{version}.tgz"
    shutil.copy2(tgz, pkg)
    files.append(pkg)

    site = DIST / f"{PROJECT}_{version}_site.zip"
    with zipfile.ZipFile(site, "w", zipfile.ZIP_DEFLATED) as z:
        for p in sorted(OUTPUT.rglob("*")):
            if p.is_file() and p.relative_to(OUTPUT).as_posix() not in SITE_EXCLUDE:
                z.write(p, p.relative_to(OUTPUT).as_posix())
    files.append(site)

    checksums = DIST / "checksums.txt"
    checksums.write_text("".join(f"{sha256(f)}  {f.name}\n" for f in files), encoding="utf-8")

    print(f"dist/ for {name} {version}:")
    for f in files + [checksums]:
        print(f"  {f.name}  ({f.stat().st_size / 1e6:.1f} MB)")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("command", choices=["version", "stamp", "dist"])
    ap.add_argument("--tag", help="release tag to use instead of detecting it")
    args = ap.parse_args()
    {"version": cmd_version, "stamp": cmd_stamp, "dist": cmd_dist}[args.command](args)


if __name__ == "__main__":
    main()
