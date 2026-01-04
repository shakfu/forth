#!/usr/bin/env python3
"""Start FluidSynth with General MIDI SoundFont for use with alda-midi.

Environment variables:
    ALDA_SF2_DIR      Directory containing SoundFont files
    ALDA_SF2_DEFAULT  Default SoundFont filename (default: FluidR3_GM.sf2)
"""

import argparse
import os
import subprocess
import sys
from pathlib import Path

PLATFORM_DRIVERS: dict[str, tuple[str, str]] = {
    "darwin": ("coreaudio", "coremidi"),
    "win32": ("dsound", "winmidi"),
    "linux": ("pulseaudio", "alsa_seq"),
}


def get_sf2_dir() -> Path | None:
    """Get SoundFont directory from environment variable."""
    if sf2_dir := os.environ.get("ALDA_SF2_DIR"):
        return Path(sf2_dir).expanduser()
    return None


def get_default_sf2() -> Path | None:
    """Get default SoundFont path from environment variables."""
    sf2_dir = get_sf2_dir()
    if sf2_dir is None:
        return None
    default_name = os.environ.get("ALDA_SF2_DEFAULT", "FluidR3_GM.sf2")
    return sf2_dir / default_name


def find_soundfonts(directory: Path) -> list[Path]:
    """Find all SoundFont files in a directory."""
    soundfonts = []
    if directory.exists():
        soundfonts.extend(directory.glob("*.sf2"))
        soundfonts.extend(directory.glob("*.sf3"))
    return sorted(soundfonts)


def get_platform_drivers() -> tuple[str, str]:
    """Return (audio_driver, midi_driver) for the current platform."""
    return PLATFORM_DRIVERS.get(sys.platform, ("pulseaudio", "alsa_seq"))


def parse_args() -> argparse.Namespace:
    default_sf2 = get_default_sf2()
    sf2_dir = get_sf2_dir()

    parser = argparse.ArgumentParser(
        description="Start FluidSynth with General MIDI SoundFont for use with alda-midi.",
        epilog=(
            "Environment variables:\n"
            "  ALDA_SF2_DIR      Directory containing SoundFont files\n"
            "  ALDA_SF2_DEFAULT  Default SoundFont filename (default: FluidR3_GM.sf2)\n\n"
            "Once running, start alda-midi in another terminal: alda_midi"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    sf2_help = "path to SoundFont file"
    if default_sf2:
        sf2_help += f" (default: {default_sf2})"
    parser.add_argument(
        "soundfont",
        nargs="?",
        type=Path,
        default=default_sf2,
        help=sf2_help,
    )
    parser.add_argument(
        "-g", "--gain",
        type=float,
        default=1.0,
        help="audio gain (default: 1.0)",
    )
    parser.add_argument(
        "-a", "--audio-driver",
        help="audio driver (default: auto-detect based on platform)",
    )
    parser.add_argument(
        "-m", "--midi-driver",
        help="MIDI driver (default: auto-detect based on platform)",
    )
    list_help = "list available SoundFonts"
    if sf2_dir:
        list_help += f" in {sf2_dir}"
    parser.add_argument(
        "--list",
        action="store_true",
        help=list_help,
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    sf2_dir = get_sf2_dir()

    if args.list:
        if sf2_dir is None:
            print("ALDA_SF2_DIR environment variable is not set.")
            print("Set it to the directory containing your SoundFont files.")
            return 1
        available = find_soundfonts(sf2_dir)
        if available:
            print(f"Available SoundFonts in {sf2_dir}:")
            for sf in available:
                print(f"  {sf.name}")
        else:
            print(f"No SoundFonts found in {sf2_dir}")
        return 0

    if args.soundfont is None:
        print("No SoundFont specified and ALDA_SF2_DIR is not set.")
        print()
        print("Either provide a SoundFont path as an argument:")
        print("  python fluidsynth-gm.py /path/to/soundfont.sf2")
        print()
        print("Or set the environment variable:")
        print("  export ALDA_SF2_DIR=~/Music/sf2")
        return 1

    sf2_path = args.soundfont.expanduser().resolve()

    if not sf2_path.is_file():
        print(f"SoundFont not found: {sf2_path}")
        if sf2_dir:
            available = find_soundfonts(sf2_dir)
            if available:
                print(f"Available SoundFonts in {sf2_dir}:")
                for sf in available:
                    print(f"  {sf.name}")
            else:
                print(f"No SoundFonts found in {sf2_dir}")
        print("Download a SoundFont (e.g., FluidR3_GM.sf2) and place it in your SF2 directory.")
        return 1

    auto_audio, auto_midi = get_platform_drivers()
    audio_driver = args.audio_driver or auto_audio
    midi_driver = args.midi_driver or auto_midi

    print(f"Starting FluidSynth with: {sf2_path.name}")
    print(f"Audio driver: {audio_driver}, MIDI driver: {midi_driver}")
    print("Connect alda-midi via: alda_midi")
    print("Press Ctrl+C to stop")
    print()

    try:
        subprocess.run(
            [
                "fluidsynth",
                "-a", audio_driver,
                "-m", midi_driver,
                "-g", str(args.gain),
                str(sf2_path),
            ],
            check=True,
        )
    except FileNotFoundError:
        print("Error: fluidsynth not found. Install it first:")
        if sys.platform == "darwin":
            print("  brew install fluidsynth")
        elif sys.platform == "win32":
            print("  Download from https://github.com/FluidSynth/fluidsynth/releases")
        else:
            print("  sudo apt install fluidsynth  # Debian/Ubuntu")
            print("  sudo dnf install fluidsynth  # Fedora")
        return 1
    except KeyboardInterrupt:
        print("\nStopped.")
    except subprocess.CalledProcessError as e:
        print(f"FluidSynth exited with error: {e.returncode}")
        return e.returncode

    return 0


if __name__ == "__main__":
    sys.exit(main())
