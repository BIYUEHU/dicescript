#!/usr/bin/env python3
import os
import shutil
import subprocess
import sys
from pathlib import Path


def run_command(cmd: list[str], cwd: Path | None = None) -> None:
    print(f">>> Running: {' '.join(cmd)}")
    try:
        result = subprocess.run(
            cmd, cwd=cwd, check=True, text=True, capture_output=True, shell=True
        )
        if result.stdout:
            print(result.stdout)
    except subprocess.CalledProcessError as e:
        print(
            f"❌ Error: Command failed with exit code {e.returncode}", file=sys.stderr
        )
        if e.stdout:
            print(f"stdout: {e.stdout}", file=sys.stderr)
        if e.stderr:
            print(f"stderr: {e.stderr}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError:
        print(f"❌ Error: Command '{cmd[0]}' not found", file=sys.stderr)
        sys.exit(1)


def ensure_clean_dir(path: Path) -> None:
    if path.exists():
        print(f"🗑️  Removing existing directory: {path}")
        shutil.rmtree(path)

    print(f"📁 Creating directory: {path}")
    path.mkdir(parents=True, exist_ok=True)


def copy_file(src: Path, dst: Path) -> None:
    if not src.exists():
        print(f"❌ Error: Source file not found: {src}", file=sys.stderr)
        sys.exit(1)

    print(f"📄 Copying: {src} -> {dst}")
    shutil.copy2(src, dst)


def main() -> None:
    root_dir = Path.cwd()
    js_pack_dir = root_dir / "js-pack"
    ipkg_file = root_dir / "dice.ipkg"
    lib_dir = root_dir / "lib"

    build_output = js_pack_dir / "build" / "exec" / "index.js"
    ts_definition = js_pack_dir / "index.d.ts"

    print("🎲 Starting Idris2 JavaScript build process...")
    print(f"Root directory: {root_dir}")
    print(f"JS Pack directory: {js_pack_dir}")

    if not js_pack_dir.exists():
        print(f"❌ Error: Directory 'js-pack' not found", file=sys.stderr)
        sys.exit(1)

    if not ipkg_file.exists():
        print(f"❌ Error: IPkg file not found: {ipkg_file}", file=sys.stderr)
        sys.exit(1)

    print("\n📦 Step 1: Installing Idris2 package...")
    os.chdir(js_pack_dir)

    run_command(["idris2", "--install", "../dice.ipkg"])

    print("\n🔨 Step 2: Building JavaScript output...")
    run_command(["idris2", "--cg", "javascript", "--build"])

    os.chdir(root_dir)

    print("\n🧹 Step 3: Preparing lib directory...")
    ensure_clean_dir(lib_dir)

    print("\n📋 Step 4: Copying output files...")
    copy_file(build_output, lib_dir / "index.js")

    if ts_definition.exists():
        copy_file(ts_definition, lib_dir / "index.d.ts")
    else:
        print(f"⚠️  Warning: TypeScript definition file not found: {ts_definition}")

    print("\n✅ Build and pack completed successfully!")
    print(f"Output files are in: {lib_dir}")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n⚠️  Build interrupted by user", file=sys.stderr)
        sys.exit(130)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}", file=sys.stderr)
        sys.exit(1)
