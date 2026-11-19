#!/usr/bin/env python3
"""
_buildIG.py

Builds the IG with the HL7 IG Publisher and fails fast when something goes wrong,
instead of hanging.

Why this exists: the publisher does not report a failed Jekyll step. When it
cannot render one resource it logs "Exception generating resource ...", carries on,
and then waits for a Jekyll process that failed on the missing file. The build seems
to hang for an hour. This script reads the publisher log as it is written and

  * stops at the first "Exception generating resource" and prints it;
  * stops when the Jekyll step shows no progress (default 4 minutes), runs Jekyll
    itself on the generated pages and prints the real error;
  * removes stale temp/ and output/ first, so pages of an earlier or aborted build
    cannot break this one;
  * checks that output/index.html exists and prints the publisher's QA totals.

  py -3 _buildIG.py                 build with the terminology server
  py -3 _buildIG.py --no-tx         build without a terminology server (faster, fewer checks)
  py -3 _buildIG.py --no-sushi      skip SUSHI (run `sushi .` yourself first)
  py -3 _buildIG.py --keep          do not clean temp/ and output/ first

Exit code 0: built. 1: failed (the reason is printed). QA errors and warnings in the
IG itself are reported but do not fail the build; read output/qa.html for those.

Standard library only.
"""

import argparse
import os
import queue
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
JAR = ROOT / "input-cache" / "publisher.jar"
LOG = ROOT / "ig-build.log"
ANSI = re.compile(r"\x1b\[[0-9;]*m")
FAIL_FAST = re.compile(r"Exception generating resource|java\.lang\.OutOfMemoryError|^FATAL")


def kill_tree(p):
    if os.name == "nt":
        subprocess.run(["taskkill", "/F", "/T", "/PID", str(p.pid)], capture_output=True)
    else:
        p.kill()


def read_lines(p, q):
    for raw in iter(p.stdout.readline, ""):
        q.put(raw)
    q.put(None)


def diagnose_jekyll():
    """Run Jekyll on the generated pages and print what it complains about."""
    pages = ROOT / "temp" / "pages"
    jekyll = shutil.which("jekyll")
    if not pages.exists() or not jekyll:
        print("  (no temp/pages or no jekyll on PATH: cannot diagnose)")
        return
    out = Path(tempfile.mkdtemp(prefix="jk-"))
    try:
        r = subprocess.run([jekyll, "build", "-s", str(pages), "-d", str(out), "--trace"],
                           capture_output=True, text=True, timeout=240)
        text = ANSI.sub("", r.stdout + r.stderr)
        hits = [l.strip() for l in text.splitlines() if "Liquid Exception" in l or "Error:" in l]
        print("  Jekyll says:")
        for l in (hits[:3] or ["(no error found; Jekyll exit code %d)" % r.returncode]):
            print("   ", l[:400])
    except subprocess.TimeoutExpired:
        print("  Jekyll itself did not finish within 4 minutes.")
    finally:
        shutil.rmtree(out, ignore_errors=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--no-tx", action="store_true", help="build without a terminology server")
    ap.add_argument("--no-sushi", action="store_true", help="skip SUSHI")
    ap.add_argument("--keep", action="store_true", help="do not remove temp/ and output/ first")
    ap.add_argument("--stall", type=int, default=240, help="seconds without progress in the Jekyll step before giving up")
    ap.add_argument("--timeout", type=int, default=1800, help="overall limit in seconds")
    args = ap.parse_args()

    if not JAR.exists():
        sys.exit("ERROR: input-cache/publisher.jar not found. Run _updatePublisher.bat first.")
    if not args.keep:
        for d in ("temp", "output"):
            shutil.rmtree(ROOT / d, ignore_errors=True)

    cmd = ["java", "-jar", str(JAR), "-ig", "."]
    if args.no_tx:
        cmd += ["-tx", "n/a"]
    if args.no_sushi:
        cmd += ["-no-sushi"]
    env = dict(os.environ, JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF-8")
    start = time.time()
    print("Building the IG (" + " ".join(cmd[3:]) + ")")
    p = subprocess.Popen(cmd, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
                         encoding="utf-8", errors="replace", env=env)
    q = queue.Queue()
    threading.Thread(target=read_lines, args=(p, q), daemon=True).start()

    last_line, last_progress, qa = "", time.time(), ""
    with LOG.open("w", encoding="utf-8") as log:
        while True:
            try:
                raw = q.get(timeout=5)
            except queue.Empty:
                raw = ""
                waiting = time.time() - last_progress
                if time.time() - start > args.timeout:
                    kill_tree(p)
                    sys.exit(f"FAILED: the build took longer than {args.timeout} s. Log: {LOG.name}")
                if last_line.startswith("Jekyll:") and waiting > args.stall:
                    kill_tree(p)
                    print(f"FAILED: no progress in the Jekyll step for {int(waiting)} s. The publisher does not report a failed Jekyll step.")
                    diagnose_jekyll()
                    sys.exit(f"Log: {LOG.name}")
                continue
            if raw is None:
                break
            line = ANSI.sub("", raw).rstrip()
            log.write(line + "\n")
            if line.strip():
                last_line, last_progress = line, time.time()
            if "Errors:" in line and "Warnings:" in line:
                qa = line.strip()
            if FAIL_FAST.search(line):
                context = [line]
                t_end = time.time() + 3
                while len(context) < 8 and time.time() < t_end:
                    try:
                        more = q.get(timeout=1)
                    except queue.Empty:
                        break
                    if more is None:
                        break
                    context.append(ANSI.sub("", more).rstrip())
                    log.write(context[-1] + "\n")
                kill_tree(p)
                print("FAILED: the publisher could not render a resource. The IG would not build.")
                for c in context:
                    print("  " + c[:240])
                sys.exit(f"Fix the resource above and build again. Log: {LOG.name}")

    p.wait()
    took = int(time.time() - start)
    if not (ROOT / "output" / "index.html").exists():
        sys.exit(f"FAILED: the publisher finished (exit {p.returncode}) but output/index.html is missing. Log: {LOG.name}")
    print(f"Built in {took // 60}:{took % 60:02d}. {qa[:160]}")
    print("Open output/index.html. QA details: output/qa.html")


if __name__ == "__main__":
    main()
