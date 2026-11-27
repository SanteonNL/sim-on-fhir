"""
_log.py

Coloured terminal output for the scripts in this repository, without changing their print calls:
install() wraps stdout and stderr and colours each line by what it says.

  red     ERROR, FAILED, UNREACHABLE, and the indented lines that follow an ERROR
  yellow  WARNING, OUTDATED
  green   "up to date", "ok", "added"
  dim     "wrote ...", "removed ...", "already downloaded"

No colour when the output is not a terminal (a pipe or a file) or when NO_COLOR is set;
FORCE_COLOR forces it on. Standard library only.
"""

import os
import re
import sys

RESET, RED, YELLOW, GREEN, DIM, BOLD = "\033[0m", "\033[31m", "\033[33m", "\033[32m", "\033[2m", "\033[1m"

RULES = [
    (re.compile(r"\bERROR\b|\bFAILED\b|\bUNREACHABLE\b"), BOLD + RED),
    (re.compile(r"\bWARNING\b|\bOUTDATED\b"), YELLOW),
    (re.compile(r"\bup to date\b|^\s*(ok|added)\b"), GREEN),
    (re.compile(r"^\s+(wrote|removed)\s|already downloaded"), DIM),
]


def _enable_windows_ansi():
    """Turn on ANSI escape handling in a classic Windows console (Windows Terminal has it already)."""
    if os.name != "nt":
        return True
    try:
        import ctypes
        k = ctypes.windll.kernel32
        ok = True
        for handle in (-11, -12):  # STD_OUTPUT_HANDLE, STD_ERROR_HANDLE
            h = k.GetStdHandle(handle)
            mode = ctypes.c_uint32()
            if k.GetConsoleMode(h, ctypes.byref(mode)):
                ok &= bool(k.SetConsoleMode(h, mode.value | 0x0004))  # ENABLE_VIRTUAL_TERMINAL_PROCESSING
        return ok
    except Exception:
        return False


class _Colouring:
    """A text stream wrapper that colours complete lines."""

    def __init__(self, stream):
        self.stream, self.buf, self.in_error = stream, "", False

    def write(self, s):
        self.buf += s
        *lines, self.buf = self.buf.split("\n")
        for line in lines:
            self.stream.write(self._colour(line) + "\n")
        return len(s)

    def _colour(self, line):
        if self.in_error and line.startswith(" "):  # detail lines under "ERROR: ...:"
            return RED + line + RESET
        self.in_error = False
        for pat, col in RULES:
            if pat.search(line):
                self.in_error = col.endswith(RED) and line.startswith("ERROR")
                return col + line + RESET
        return line

    def flush(self):
        if self.buf:
            self.stream.write(self._colour(self.buf))
            self.buf = ""
        self.stream.flush()

    def __getattr__(self, name):
        return getattr(self.stream, name)


def install():
    if os.environ.get("NO_COLOR"):
        return
    if not os.environ.get("FORCE_COLOR") and not sys.stdout.isatty():
        return
    if not _enable_windows_ansi() and not os.environ.get("FORCE_COLOR"):
        return
    for name in ("stdout", "stderr"):
        stream = getattr(sys, name)
        try:
            stream.reconfigure(encoding="utf-8")  # NZa descriptions contain characters outside cp1252
        except (AttributeError, ValueError):
            pass
        setattr(sys, name, _Colouring(stream))
