import midi
import time as _time

# ============================================================================
# Dynamics (velocity values)
# ============================================================================
midi.ppp = 16
midi.pp = 33
midi.p = 49
midi.mp = 64
midi.mf = 80
midi.f = 96
midi.ff = 112
midi.fff = 127

# ============================================================================
# Durations (milliseconds at 120 BPM)
# ============================================================================
midi.whole = 2000
midi.half = 1000
midi.quarter = 500
midi.eighth = 250
midi.sixteenth = 125

def _dotted(duration):
    '''Return dotted duration (1.5x)'''
    return int(duration * 1.5)
midi.dotted = _dotted

# ============================================================================
# Tempo
# ============================================================================
midi._bpm = 120

def _set_tempo(bpm):
    '''Set tempo in BPM and update duration constants'''
    midi._bpm = bpm
    beat_ms = 60000 // bpm
    midi.quarter = beat_ms
    midi.half = beat_ms * 2
    midi.whole = beat_ms * 4
    midi.eighth = beat_ms // 2
    midi.sixteenth = beat_ms // 4
midi.set_tempo = _set_tempo

def _get_tempo():
    '''Get current tempo in BPM'''
    return midi._bpm
midi.get_tempo = _get_tempo

def _bpm_to_ms(tempo):
    '''Return quarter note duration in ms for given tempo'''
    return 60000 // tempo
midi.bpm = _bpm_to_ms

# ============================================================================
# Timing helpers
# ============================================================================
def _sleep(ms):
    '''Sleep for given milliseconds'''
    _time.sleep(ms / 1000.0)
midi.sleep = _sleep

def _rest(duration=None):
    '''Rest (silence) for given duration, default is quarter note'''
    if duration is None:
        duration = midi.quarter
    _time.sleep(duration / 1000.0)
midi.rest = _rest

# ============================================================================
# Scale intervals (semitones from root)
# ============================================================================

# Diatonic modes
midi.SCALE_MAJOR = (0, 2, 4, 5, 7, 9, 11)
midi.SCALE_IONIAN = midi.SCALE_MAJOR
midi.SCALE_DORIAN = (0, 2, 3, 5, 7, 9, 10)
midi.SCALE_PHRYGIAN = (0, 1, 3, 5, 7, 8, 10)
midi.SCALE_LYDIAN = (0, 2, 4, 6, 7, 9, 11)
midi.SCALE_MIXOLYDIAN = (0, 2, 4, 5, 7, 9, 10)
midi.SCALE_MINOR = (0, 2, 3, 5, 7, 8, 10)
midi.SCALE_AEOLIAN = midi.SCALE_MINOR
midi.SCALE_LOCRIAN = (0, 1, 3, 5, 6, 8, 10)

# Other minor scales
midi.SCALE_HARMONIC_MINOR = (0, 2, 3, 5, 7, 8, 11)
midi.SCALE_MELODIC_MINOR = (0, 2, 3, 5, 7, 9, 11)

# Pentatonic
midi.SCALE_PENTATONIC = (0, 2, 4, 7, 9)
midi.SCALE_PENTATONIC_MAJOR = midi.SCALE_PENTATONIC
midi.SCALE_PENTATONIC_MINOR = (0, 3, 5, 7, 10)

# Blues
midi.SCALE_BLUES = (0, 3, 5, 6, 7, 10)

# Symmetric
midi.SCALE_WHOLE_TONE = (0, 2, 4, 6, 8, 10)
midi.SCALE_CHROMATIC = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
midi.SCALE_DIMINISHED_HW = (0, 1, 3, 4, 6, 7, 9, 10)
midi.SCALE_DIMINISHED_WH = (0, 2, 3, 5, 6, 8, 9, 11)
midi.SCALE_AUGMENTED = (0, 3, 4, 7, 8, 11)

# Bebop
midi.SCALE_BEBOP_DOMINANT = (0, 2, 4, 5, 7, 9, 10, 11)
midi.SCALE_BEBOP_MAJOR = (0, 2, 4, 5, 7, 8, 9, 11)
midi.SCALE_BEBOP_MINOR = (0, 2, 3, 5, 7, 8, 9, 10)

# Exotic/World
midi.SCALE_HUNGARIAN_MINOR = (0, 2, 3, 6, 7, 8, 11)
midi.SCALE_DOUBLE_HARMONIC = (0, 1, 4, 5, 7, 8, 11)
midi.SCALE_NEAPOLITAN_MAJOR = (0, 1, 3, 5, 7, 9, 11)
midi.SCALE_NEAPOLITAN_MINOR = (0, 1, 3, 5, 7, 8, 11)
midi.SCALE_PHRYGIAN_DOMINANT = (0, 1, 4, 5, 7, 8, 10)
midi.SCALE_PERSIAN = (0, 1, 4, 5, 6, 8, 11)
midi.SCALE_ALTERED = (0, 1, 3, 4, 6, 8, 10)
midi.SCALE_ENIGMATIC = (0, 1, 4, 6, 8, 10, 11)

# Japanese
midi.SCALE_HIRAJOSHI = (0, 2, 3, 7, 8)
midi.SCALE_IN_SEN = (0, 1, 5, 7, 10)
midi.SCALE_IWATO = (0, 1, 5, 6, 10)
midi.SCALE_KUMOI = (0, 2, 3, 7, 9)

# Other world scales
midi.SCALE_EGYPTIAN = (0, 2, 5, 7, 10)
midi.SCALE_ROMANIAN_MINOR = (0, 2, 3, 6, 7, 9, 10)
midi.SCALE_SPANISH_8_TONE = (0, 1, 3, 4, 5, 6, 8, 10)

# Arabic Maqamat (12-TET approximations)
midi.SCALE_MAQAM_HIJAZ = (0, 1, 4, 5, 7, 8, 10)
midi.SCALE_MAQAM_NAHAWAND = (0, 2, 3, 5, 7, 8, 11)
midi.SCALE_MAQAM_NIKRIZ = (0, 2, 3, 6, 7, 9, 10)
midi.SCALE_MAQAM_ATHAR_KURD = (0, 1, 3, 5, 6, 8, 10)
midi.SCALE_MAQAM_SHAWQ_AFZA = (0, 2, 3, 6, 7, 9, 11)
midi.SCALE_MAQAM_JIHARKAH = (0, 2, 4, 5, 7, 9, 10)

# Indian Ragas (12-TET approximations)
midi.SCALE_RAGA_BHAIRAV = (0, 1, 4, 5, 7, 8, 11)
midi.SCALE_RAGA_TODI = (0, 1, 3, 6, 7, 8, 11)
midi.SCALE_RAGA_MARWA = (0, 1, 4, 6, 7, 9, 11)
midi.SCALE_RAGA_PURVI = (0, 1, 4, 6, 7, 8, 11)
midi.SCALE_RAGA_CHARUKESHI = (0, 2, 4, 5, 7, 8, 10)
midi.SCALE_RAGA_ASAVARI = (0, 2, 3, 5, 7, 8, 10)
midi.SCALE_RAGA_BILAWAL = (0, 2, 4, 5, 7, 9, 11)
midi.SCALE_RAGA_KHAMAJ = (0, 2, 4, 5, 7, 9, 10)
midi.SCALE_RAGA_KALYAN = (0, 2, 4, 6, 7, 9, 11)
midi.SCALE_RAGA_BHIMPALASI = (0, 3, 5, 7, 10)
midi.SCALE_RAGA_DARBARI = (0, 2, 3, 5, 7, 8, 9)

# ============================================================================
# Microtonal scales (cents-based, for use with pitch_bend)
# ============================================================================

# Arabic Maqamat with quarter tones
midi.SCALE_MAQAM_BAYATI_CENTS = (0, 150, 300, 500, 700, 800, 1000)
midi.SCALE_MAQAM_RAST_CENTS = (0, 200, 350, 500, 700, 900, 1050)
midi.SCALE_MAQAM_SABA_CENTS = (0, 150, 300, 400, 500, 700, 800)
midi.SCALE_MAQAM_SIKAH_CENTS = (0, 150, 350, 500, 650, 850, 1000)
midi.SCALE_MAQAM_HUZAM_CENTS = (0, 150, 350, 500, 700, 850, 1050)
midi.SCALE_MAQAM_IRAQ_CENTS = (0, 150, 350, 500, 700, 850, 1000)
midi.SCALE_MAQAM_BASTANIKAR_CENTS = (0, 150, 350, 500, 700, 800, 1000)

# Turkish Makamlar
midi.SCALE_MAKAM_USSAK_CENTS = (0, 150, 300, 500, 700, 800, 1000)
midi.SCALE_MAKAM_HUSEYNI_CENTS = (0, 150, 300, 500, 700, 900, 1000)

# Indian 22-Shruti scale
midi.SCALE_SHRUTI_CENTS = (0, 90, 112, 182, 204, 294, 316, 386, 408, 498, 520, 590,
                           612, 702, 792, 814, 884, 906, 996, 1018, 1088, 1110)

# ============================================================================
# Scale name lookup table
# ============================================================================
midi.scales = {
    'major': midi.SCALE_MAJOR,
    'ionian': midi.SCALE_IONIAN,
    'dorian': midi.SCALE_DORIAN,
    'phrygian': midi.SCALE_PHRYGIAN,
    'lydian': midi.SCALE_LYDIAN,
    'mixolydian': midi.SCALE_MIXOLYDIAN,
    'minor': midi.SCALE_MINOR,
    'aeolian': midi.SCALE_AEOLIAN,
    'locrian': midi.SCALE_LOCRIAN,
    'harmonic_minor': midi.SCALE_HARMONIC_MINOR,
    'melodic_minor': midi.SCALE_MELODIC_MINOR,
    'pentatonic': midi.SCALE_PENTATONIC,
    'pentatonic_major': midi.SCALE_PENTATONIC_MAJOR,
    'pentatonic_minor': midi.SCALE_PENTATONIC_MINOR,
    'blues': midi.SCALE_BLUES,
    'whole_tone': midi.SCALE_WHOLE_TONE,
    'chromatic': midi.SCALE_CHROMATIC,
    'diminished_hw': midi.SCALE_DIMINISHED_HW,
    'diminished_wh': midi.SCALE_DIMINISHED_WH,
    'augmented': midi.SCALE_AUGMENTED,
    'bebop_dominant': midi.SCALE_BEBOP_DOMINANT,
    'bebop_major': midi.SCALE_BEBOP_MAJOR,
    'bebop_minor': midi.SCALE_BEBOP_MINOR,
    'hungarian_minor': midi.SCALE_HUNGARIAN_MINOR,
    'double_harmonic': midi.SCALE_DOUBLE_HARMONIC,
    'neapolitan_major': midi.SCALE_NEAPOLITAN_MAJOR,
    'neapolitan_minor': midi.SCALE_NEAPOLITAN_MINOR,
    'phrygian_dominant': midi.SCALE_PHRYGIAN_DOMINANT,
    'persian': midi.SCALE_PERSIAN,
    'altered': midi.SCALE_ALTERED,
    'enigmatic': midi.SCALE_ENIGMATIC,
    'hirajoshi': midi.SCALE_HIRAJOSHI,
    'in_sen': midi.SCALE_IN_SEN,
    'iwato': midi.SCALE_IWATO,
    'kumoi': midi.SCALE_KUMOI,
    'egyptian': midi.SCALE_EGYPTIAN,
    'romanian_minor': midi.SCALE_ROMANIAN_MINOR,
    'spanish_8_tone': midi.SCALE_SPANISH_8_TONE,
    'maqam_hijaz': midi.SCALE_MAQAM_HIJAZ,
    'maqam_nahawand': midi.SCALE_MAQAM_NAHAWAND,
    'maqam_nikriz': midi.SCALE_MAQAM_NIKRIZ,
    'maqam_athar_kurd': midi.SCALE_MAQAM_ATHAR_KURD,
    'maqam_shawq_afza': midi.SCALE_MAQAM_SHAWQ_AFZA,
    'maqam_jiharkah': midi.SCALE_MAQAM_JIHARKAH,
    'raga_bhairav': midi.SCALE_RAGA_BHAIRAV,
    'raga_todi': midi.SCALE_RAGA_TODI,
    'raga_marwa': midi.SCALE_RAGA_MARWA,
    'raga_purvi': midi.SCALE_RAGA_PURVI,
    'raga_charukeshi': midi.SCALE_RAGA_CHARUKESHI,
    'raga_asavari': midi.SCALE_RAGA_ASAVARI,
    'raga_bilawal': midi.SCALE_RAGA_BILAWAL,
    'raga_khamaj': midi.SCALE_RAGA_KHAMAJ,
    'raga_kalyan': midi.SCALE_RAGA_KALYAN,
    'raga_bhimpalasi': midi.SCALE_RAGA_BHIMPALASI,
    'raga_darbari': midi.SCALE_RAGA_DARBARI,
}

# Microtonal scales lookup
midi.scales_cents = {
    'maqam_bayati': midi.SCALE_MAQAM_BAYATI_CENTS,
    'maqam_rast': midi.SCALE_MAQAM_RAST_CENTS,
    'maqam_saba': midi.SCALE_MAQAM_SABA_CENTS,
    'maqam_sikah': midi.SCALE_MAQAM_SIKAH_CENTS,
    'maqam_huzam': midi.SCALE_MAQAM_HUZAM_CENTS,
    'maqam_iraq': midi.SCALE_MAQAM_IRAQ_CENTS,
    'maqam_bastanikar': midi.SCALE_MAQAM_BASTANIKAR_CENTS,
    'makam_ussak': midi.SCALE_MAKAM_USSAK_CENTS,
    'makam_huseyni': midi.SCALE_MAKAM_HUSEYNI_CENTS,
    'shruti': midi.SCALE_SHRUTI_CENTS,
}

# ============================================================================
# Scale helper functions
# ============================================================================

def _get_scale(name):
    '''Look up scale intervals by name'''
    if name not in midi.scales:
        raise ValueError(f"Unknown scale: {name}")
    return midi.scales[name]

def _scale(root, name):
    '''Build a scale from root and scale name. E.g. scale(60, "major")'''
    if isinstance(root, str):
        root = midi.note(root)
    return midi.build_scale(root, _get_scale(name))
midi.scale = _scale

def _degree(root, name, n):
    '''Get the nth degree of a named scale. E.g. degree(60, "major", 5)'''
    if isinstance(root, str):
        root = midi.note(root)
    return midi.scale_degree(root, _get_scale(name), n)
midi.degree = _degree

def _in_scale_named(pitch, root, name):
    '''Check if pitch is in named scale. E.g. in_scale_named(64, 60, "major")'''
    if isinstance(root, str):
        root = midi.note(root)
    return midi.in_scale(pitch, root, _get_scale(name))
midi.in_scale_named = _in_scale_named

def _quantize(pitch, root, name):
    '''Quantize pitch to named scale. E.g. quantize(66, 60, "major")'''
    if isinstance(root, str):
        root = midi.note(root)
    return midi.quantize_to_scale(pitch, root, _get_scale(name))
midi.quantize = _quantize

def _cents_to_note(root, cents):
    '''Convert cents interval to (note, bend_cents) tuple'''
    if isinstance(root, str):
        root = midi.note(root)
    semitones = cents // 100
    bend_cents = cents % 100
    if bend_cents > 50:
        semitones += 1
        bend_cents -= 100
    return (root + semitones, bend_cents)
midi.cents_to_note = _cents_to_note

# ============================================================================
# Chord builders (return list of MIDI pitches)
# ============================================================================
def _major(root):
    '''Build major triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7]
midi.major = _major

def _minor(root):
    '''Build minor triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 7]
midi.minor = _minor

def _dim(root):
    '''Build diminished triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 6]
midi.dim = _dim

def _aug(root):
    '''Build augmented triad from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 8]
midi.aug = _aug

def _dom7(root):
    '''Build dominant 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7, root + 10]
midi.dom7 = _dom7

def _maj7(root):
    '''Build major 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 4, root + 7, root + 11]
midi.maj7 = _maj7

def _min7(root):
    '''Build minor 7th chord from root'''
    if isinstance(root, str):
        root = midi.note(root)
    return [root, root + 3, root + 7, root + 10]
midi.min7 = _min7

# ============================================================================
# Pitch helpers
# ============================================================================
def _transpose(pitch, semitones):
    '''Transpose pitch by semitones'''
    if isinstance(pitch, str):
        pitch = midi.note(pitch)
    return pitch + semitones
midi.transpose = _transpose

def _octave_up(pitch):
    '''Transpose pitch up one octave'''
    return _transpose(pitch, 12)
midi.octave_up = _octave_up

def _octave_down(pitch):
    '''Transpose pitch down one octave'''
    return _transpose(pitch, -12)
midi.octave_down = _octave_down

# ============================================================================
# Pitch constants (all octaves)
# ============================================================================
for _oct in range(9):
    setattr(midi, f'c{_oct}', 12 + _oct * 12)
    setattr(midi, f'cs{_oct}', 13 + _oct * 12)
    setattr(midi, f'db{_oct}', 13 + _oct * 12)
    setattr(midi, f'd{_oct}', 14 + _oct * 12)
    setattr(midi, f'ds{_oct}', 15 + _oct * 12)
    setattr(midi, f'eb{_oct}', 15 + _oct * 12)
    setattr(midi, f'e{_oct}', 16 + _oct * 12)
    setattr(midi, f'f{_oct}', 17 + _oct * 12)
    setattr(midi, f'fs{_oct}', 18 + _oct * 12)
    setattr(midi, f'gb{_oct}', 18 + _oct * 12)
    setattr(midi, f'g{_oct}', 19 + _oct * 12)
    setattr(midi, f'gs{_oct}', 20 + _oct * 12)
    setattr(midi, f'ab{_oct}', 20 + _oct * 12)
    setattr(midi, f'a{_oct}', 21 + _oct * 12)
    setattr(midi, f'as{_oct}', 22 + _oct * 12)
    setattr(midi, f'bb{_oct}', 22 + _oct * 12)
    setattr(midi, f'b{_oct}', 23 + _oct * 12)
del _oct

# ============================================================================
# CC helpers (common control changes)
# ============================================================================
midi.CC_MODULATION = 1
midi.CC_BREATH = 2
midi.CC_VOLUME = 7
midi.CC_PAN = 10
midi.CC_EXPRESSION = 11
midi.CC_SUSTAIN = 64
midi.CC_REVERB = 91
midi.CC_CHORUS = 93

# ============================================================================
# MidiOut extensions
# ============================================================================
def _arpeggio(self, pitches, velocity=80, note_duration=None, spacing=None, channel=1):
    '''Play notes sequentially (arpeggiated)'''
    if note_duration is None:
        note_duration = midi.eighth
    if spacing is None:
        spacing = note_duration
    for p in pitches:
        self.note(p, velocity, note_duration, channel)
        if spacing > note_duration:
            midi.sleep(spacing - note_duration)
midi.MidiOut.arpeggio = _arpeggio

def _modulation(self, value, channel=1):
    '''Set modulation wheel (CC 1)'''
    self.cc(1, value, channel)
midi.MidiOut.modulation = _modulation

def _volume(self, value, channel=1):
    '''Set channel volume (CC 7)'''
    self.cc(7, value, channel)
midi.MidiOut.volume = _volume

def _pan(self, value, channel=1):
    '''Set pan position (CC 10): 0=left, 64=center, 127=right'''
    self.cc(10, value, channel)
midi.MidiOut.pan = _pan

def _sustain(self, on=True, channel=1):
    '''Set sustain pedal (CC 64)'''
    self.cc(64, 127 if on else 0, channel)
midi.MidiOut.sustain = _sustain
