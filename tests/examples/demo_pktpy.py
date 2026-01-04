# README.md example for pktpy-midi
# Tests the quick example from the project README

import midi

# Generator-based async voices
def arpeggio():
    with midi.open() as m:
        for p in midi.scale(midi.c4, "dorian"):
            yield from midi.play(m, p, midi.mf, midi.eighth)

def drone():
    with midi.open() as m:
        yield from midi.play(m, midi.c2, midi.pp, midi.whole * 4)

# Concurrent playback
midi.spawn(arpeggio, "arp")
midi.spawn(drone, "drone")
midi.run()
