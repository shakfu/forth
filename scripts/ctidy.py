#!/usr/bin/env python3
"""Run clang-tidy on all project C files."""

import shutil
import subprocess
import sys
from pathlib import Path

CHECKS = ",".join([
    "-*",
    "clang-analyzer-*",
    "bugprone-*",
    "performance-*",
    "portability-*",
    "-clang-analyzer-security.insecureAPI.DeprecatedOrUnsafeBufferHandling",
    "-bugprone-easily-swappable-parameters",
])

CLANG_TIDY_PATHS = [
    # macOS Homebrew (Apple Silicon)
    "/opt/homebrew/opt/llvm/bin/clang-tidy",
    # macOS Homebrew (Intel)
    "/usr/local/opt/llvm/bin/clang-tidy",
    # Linux/Windows common
    "/usr/bin/clang-tidy",
    "/usr/local/bin/clang-tidy",
    # Versioned (Linux)
    "/usr/bin/clang-tidy-18",
    "/usr/bin/clang-tidy-17",
    "/usr/bin/clang-tidy-16",
]


def find_clang_tidy() -> str | None:
    # Check PATH first
    if path := shutil.which("clang-tidy"):
        return path
    # Check common locations
    for candidate in CLANG_TIDY_PATHS:
        if Path(candidate).is_file():
            return candidate
    return None


def find_local_include() -> Path | None:
    # Try brew --prefix on macOS
    if shutil.which("brew"):
        try:
            result = subprocess.run(
                ["brew", "--prefix"],
                capture_output=True, text=True, check=True
            )
            inc = Path(result.stdout.strip()) / "include"
            if inc.is_dir():
                return inc
        except subprocess.CalledProcessError:
            pass
    # Fallback
    if Path("/usr/local/include").is_dir():
        return Path("/usr/local/include")
    return None


def run_tidy(ctidy: str, project: str, sources: list[Path], includes: list[Path]) -> None:
    print(f"\n{'=' * 80}\n{project}\n")

    args = [ctidy, f"--checks={CHECKS}"] + [str(s) for s in sources] + ["--"]
    for inc in includes:
        args.append(f"-I{inc}")
    args.append("-std=c11")

    result = subprocess.run(args, capture_output=True, text=True)
    output = result.stdout + result.stderr

    # Filter to only warnings from this project
    warnings = [
        line for line in output.splitlines()
        if project in line and ": note:" not in line
    ]

    if warnings:
        print("\n".join(warnings))
    else:
        print("No warnings found")


def main() -> int:
    ctidy = find_clang_tidy()
    if not ctidy:
        print("Error: clang-tidy not found. Install LLVM/Clang tools.", file=sys.stderr)
        print("  macOS:  brew install llvm", file=sys.stderr)
        print("  Ubuntu: apt install clang-tools", file=sys.stderr)
        print("  Fedora: dnf install clang-tools-extra", file=sys.stderr)
        return 1

    print(f"Using: {ctidy}")

    project_dir = Path(__file__).resolve().parent.parent
    thirdparty = project_dir / "thirdparty"
    common = project_dir / "projects" / "common"

    local_inc = find_local_include()
    base_includes = [inc for inc in [local_inc] if inc]

    projects = [
        ("alda-midi", ["alda-midi/include"], ["TinySoundFont", "miniaudio"]),
        ("forth-midi", ["forth-midi/include"], []),
        ("lua-midi", ["lua-midi/include"], ["lua-5.5.0/src"]),
        ("s7-midi", ["s7-midi/include"], ["s7"]),
        ("pktpy-midi", ["pktpy-midi/include"], ["pocketpy"]),
    ]

    for name, proj_incs, tp_incs in projects:
        proj_path = project_dir / "projects" / name
        if name == "alda-midi":
            sources = list((proj_path / "src").glob("*.c"))
        else:
            sources = list(proj_path.glob("*.c"))

        if not sources:
            continue

        includes = [common] + [project_dir / "projects" / p for p in proj_incs]
        includes += [thirdparty / p for p in tp_incs]
        includes.append(thirdparty / "libremidi" / "include")
        includes += base_includes

        run_tidy(ctidy, name, sources, includes)

    return 0


if __name__ == "__main__":
    sys.exit(main())
