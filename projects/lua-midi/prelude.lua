-- Pitch constants
for oct = 0, 8 do
  midi['c'..oct] = 12 + oct * 12
  midi['cs'..oct] = 13 + oct * 12
  midi['d'..oct] = 14 + oct * 12
  midi['ds'..oct] = 15 + oct * 12
  midi['e'..oct] = 16 + oct * 12
  midi['f'..oct] = 17 + oct * 12
  midi['fs'..oct] = 18 + oct * 12
  midi['g'..oct] = 19 + oct * 12
  midi['gs'..oct] = 20 + oct * 12
  midi['a'..oct] = 21 + oct * 12
  midi['as'..oct] = 22 + oct * 12
  midi['b'..oct] = 23 + oct * 12
  -- Flat aliases
  midi['db'..oct] = 13 + oct * 12
  midi['eb'..oct] = 15 + oct * 12
  midi['gb'..oct] = 18 + oct * 12
  midi['ab'..oct] = 20 + oct * 12
  midi['bb'..oct] = 22 + oct * 12
end

-- Dynamics (velocity values)
midi.ppp = 16
midi.pp = 33
midi.p = 49
midi.mp = 64
midi.mf = 80
midi.f = 96
midi.ff = 112
midi.fff = 127

-- Duration constants (ms at 120 BPM)
midi.whole = 2000
midi.half = 1000
midi.quarter = 500
midi.eighth = 250
midi.sixteenth = 125

-- Tempo state
midi._tempo = 120

function midi.set_tempo(bpm)
  midi._tempo = bpm
  local q = math.floor(60000 / bpm)
  midi.quarter = q
  midi.whole = q * 4
  midi.half = q * 2
  midi.eighth = math.floor(q / 2)
  midi.sixteenth = math.floor(q / 4)
end

function midi.get_tempo()
  return midi._tempo
end

function midi.bpm(tempo)
  return math.floor(60000 / tempo)
end

function midi.dotted(dur)
  return math.floor(dur * 1.5)
end

function midi.rest(dur)
  midi.sleep(dur or midi.quarter)
end

-- Scale intervals (semitones from root)
midi.scales = {
  -- Diatonic modes
  major = {0, 2, 4, 5, 7, 9, 11},
  ionian = {0, 2, 4, 5, 7, 9, 11},
  dorian = {0, 2, 3, 5, 7, 9, 10},
  phrygian = {0, 1, 3, 5, 7, 8, 10},
  lydian = {0, 2, 4, 6, 7, 9, 11},
  mixolydian = {0, 2, 4, 5, 7, 9, 10},
  minor = {0, 2, 3, 5, 7, 8, 10},
  aeolian = {0, 2, 3, 5, 7, 8, 10},
  locrian = {0, 1, 3, 5, 6, 8, 10},

  -- Other minor scales
  harmonic_minor = {0, 2, 3, 5, 7, 8, 11},
  melodic_minor = {0, 2, 3, 5, 7, 9, 11},

  -- Pentatonic
  pentatonic = {0, 2, 4, 7, 9},
  pentatonic_major = {0, 2, 4, 7, 9},
  pentatonic_minor = {0, 3, 5, 7, 10},

  -- Blues
  blues = {0, 3, 5, 6, 7, 10},

  -- Symmetric
  whole_tone = {0, 2, 4, 6, 8, 10},
  chromatic = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11},
  diminished_hw = {0, 1, 3, 4, 6, 7, 9, 10},
  diminished_wh = {0, 2, 3, 5, 6, 8, 9, 11},
  augmented = {0, 3, 4, 7, 8, 11},

  -- Bebop
  bebop_dominant = {0, 2, 4, 5, 7, 9, 10, 11},
  bebop_major = {0, 2, 4, 5, 7, 8, 9, 11},
  bebop_minor = {0, 2, 3, 5, 7, 8, 9, 10},

  -- Exotic/World
  hungarian_minor = {0, 2, 3, 6, 7, 8, 11},
  double_harmonic = {0, 1, 4, 5, 7, 8, 11},
  neapolitan_major = {0, 1, 3, 5, 7, 9, 11},
  neapolitan_minor = {0, 1, 3, 5, 7, 8, 11},
  phrygian_dominant = {0, 1, 4, 5, 7, 8, 10},
  persian = {0, 1, 4, 5, 6, 8, 11},
  altered = {0, 1, 3, 4, 6, 8, 10},
  enigmatic = {0, 1, 4, 6, 8, 10, 11},

  -- Japanese
  hirajoshi = {0, 2, 3, 7, 8},
  in_sen = {0, 1, 5, 7, 10},
  iwato = {0, 1, 5, 6, 10},
  kumoi = {0, 2, 3, 7, 9},

  -- Other world scales
  egyptian = {0, 2, 5, 7, 10},
  romanian_minor = {0, 2, 3, 6, 7, 9, 10},
  spanish_8_tone = {0, 1, 3, 4, 5, 6, 8, 10},

  -- Arabic Maqamat (12-TET approximations)
  maqam_hijaz = {0, 1, 4, 5, 7, 8, 10},
  maqam_nahawand = {0, 2, 3, 5, 7, 8, 11},
  maqam_nikriz = {0, 2, 3, 6, 7, 9, 10},
  maqam_athar_kurd = {0, 1, 3, 5, 6, 8, 10},
  maqam_shawq_afza = {0, 2, 3, 6, 7, 9, 11},
  maqam_jiharkah = {0, 2, 4, 5, 7, 9, 10},

  -- Indian Ragas (12-TET approximations)
  raga_bhairav = {0, 1, 4, 5, 7, 8, 11},
  raga_todi = {0, 1, 3, 6, 7, 8, 11},
  raga_marwa = {0, 1, 4, 6, 7, 9, 11},
  raga_purvi = {0, 1, 4, 6, 7, 8, 11},
  raga_charukeshi = {0, 2, 4, 5, 7, 8, 10},
  raga_asavari = {0, 2, 3, 5, 7, 8, 10},
  raga_bilawal = {0, 2, 4, 5, 7, 9, 11},
  raga_khamaj = {0, 2, 4, 5, 7, 9, 10},
  raga_kalyan = {0, 2, 4, 6, 7, 9, 11},
  raga_bhimpalasi = {0, 3, 5, 7, 10},
  raga_darbari = {0, 2, 3, 5, 7, 8, 9},
}

-- Microtonal scales (cents-based, for use with pitch_bend)
midi.scales_cents = {
  -- Arabic Maqamat with quarter tones
  maqam_bayati = {0, 150, 300, 500, 700, 800, 1000},
  maqam_rast = {0, 200, 350, 500, 700, 900, 1050},
  maqam_saba = {0, 150, 300, 400, 500, 700, 800},
  maqam_sikah = {0, 150, 350, 500, 650, 850, 1000},
  maqam_huzam = {0, 150, 350, 500, 700, 850, 1050},
  maqam_iraq = {0, 150, 350, 500, 700, 850, 1000},
  maqam_bastanikar = {0, 150, 350, 500, 700, 800, 1000},

  -- Turkish Makamlar
  makam_ussak = {0, 150, 300, 500, 700, 800, 1000},
  makam_huseyni = {0, 150, 300, 500, 700, 900, 1000},

  -- Indian 22-Shruti scale
  shruti = {0, 90, 112, 182, 204, 294, 316, 386, 408, 498, 520, 590,
            612, 702, 792, 814, 884, 906, 996, 1018, 1088, 1110},
}

-- Helper: convert cents interval to note + bend
function midi.cents_to_note(root, cents)
  local semitones = math.floor(cents / 100)
  local bend_cents = cents % 100
  if bend_cents > 50 then
    semitones = semitones + 1
    bend_cents = bend_cents - 100
  end
  return root + semitones, bend_cents
end

-- Helper: build scale with name lookup
function scale(root, name)
  local intervals = midi.scales[name]
  if not intervals then
    error('Unknown scale: ' .. tostring(name))
  end
  return midi.build_scale(root, intervals)
end

-- Helper: get scale degree with name lookup
function degree(root, name, deg)
  local intervals = midi.scales[name]
  if not intervals then
    error('Unknown scale: ' .. tostring(name))
  end
  return midi.scale_degree(root, intervals, deg)
end

-- Helper: check if pitch is in named scale
function in_scale(pitch, root, name)
  local intervals = midi.scales[name]
  if not intervals then
    error('Unknown scale: ' .. tostring(name))
  end
  return midi.in_scale(pitch, root, intervals)
end

-- Helper: quantize to named scale
function quantize(pitch, root, name)
  local intervals = midi.scales[name]
  if not intervals then
    error('Unknown scale: ' .. tostring(name))
  end
  return midi.quantize(pitch, root, intervals)
end

-- Global aliases for concise syntax
-- Pitch constants as globals
for oct = 0, 8 do
  _G['c'..oct] = midi['c'..oct]
  _G['cs'..oct] = midi['cs'..oct]
  _G['d'..oct] = midi['d'..oct]
  _G['ds'..oct] = midi['ds'..oct]
  _G['e'..oct] = midi['e'..oct]
  _G['f'..oct] = midi['f'..oct]
  _G['fs'..oct] = midi['fs'..oct]
  _G['g'..oct] = midi['g'..oct]
  _G['gs'..oct] = midi['gs'..oct]
  _G['a'..oct] = midi['a'..oct]
  _G['as'..oct] = midi['as'..oct]
  _G['b'..oct] = midi['b'..oct]
  _G['db'..oct] = midi['db'..oct]
  _G['eb'..oct] = midi['eb'..oct]
  _G['gb'..oct] = midi['gb'..oct]
  _G['ab'..oct] = midi['ab'..oct]
  _G['bb'..oct] = midi['bb'..oct]
end

-- Dynamics as globals
ppp, pp, p, mp, mf, f, ff, fff = midi.ppp, midi.pp, midi.p, midi.mp, midi.mf, midi.f, midi.ff, midi.fff

-- Durations as globals
whole, half, quarter, eighth, sixteenth = midi.whole, midi.half, midi.quarter, midi.eighth, midi.sixteenth

-- Chord builders as globals
major, minor, dim, aug, dom7, maj7, min7 = midi.major, midi.minor, midi.dim, midi.aug, midi.dom7, midi.maj7, midi.min7

-- Other utilities as globals
transpose, octave_up, octave_down = midi.transpose, midi.octave_up, midi.octave_down
dotted, rest, sleep = midi.dotted, midi.rest, midi.sleep

-- Global help function
function help() midi.help() end

-- REPL convenience: global midi output
midi._out = nil

function open(arg)
  if midi._out then midi._out:close() end
  midi._out = midi.open(arg)
  return midi._out
end

function close()
  if midi._out then
    midi._out:close()
    midi._out = nil
  end
end

function n(pitch, vel, dur, ch)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:note(pitch, vel or midi.mf, dur or midi.quarter, ch or 1)
end

function ch(pitches, vel, dur, channel)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:chord(pitches, vel or midi.mf, dur or midi.quarter, channel or 1)
end

function arp(pitches, vel, dur, channel)
  if not midi._out then error('No MIDI port open. Use open() first.') end
  midi._out:arpeggio(pitches, vel or midi.mf, dur or midi.eighth, channel or 1)
end
