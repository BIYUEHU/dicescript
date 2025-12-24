#!/usr/bin/env python3
import subprocess
import sys
from pathlib import Path

commands: list[str] = [
    'python scripts/watch.py --root ./ --ext .idr --cmd "npm run build:web-only',
    "cd web && npx vite --force",
]

procs: list[tuple[str, subprocess.Popen]] = []
cwd: Path = Path.cwd()

for command in commands:
    try:
        proc = subprocess.Popen(command, cwd=str(cwd), shell=True)
        procs.append((command, proc))
    except Exception as err:
        print(f"Failed to start {command}:", err, file=sys.stderr)

for command, proc in procs:
    try:
        proc.wait()
    except Exception as err:
        print(f"Failed to start {command}:", err, file=sys.stderr)
