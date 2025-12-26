# MIDI Forth TODO

## Implemented

- [x] Pitch names (`c4`, `C#4`, `Db5`, MIDI numbers)
- [x] Comma as play trigger (`,`)
- [x] Chords with parentheses (`(c4 e4 g4),`)
- [x] Explicit parameters (`1 c4 100 500,`)
- [x] Context variables (`ch!`, `vel!`, `dur!`)
- [x] Tokenizer handles `,` `(` `)` as word boundaries
- [x] MIDI output (virtual and hardware ports)
- [x] Control change messages (`cc`)
- [x] Panic / all notes off
- [x] Word definitions (`: name ... ;`)
- [x] Rests (`r`, `r 250,`)
- [x] Loops (`name N times`)
- [x] Probability (`c4 75%,`)
- [x] Alternatives (`c4|e4|g4,`)
- [x] Dynamics (`ppp` through `fff`)
- [x] Explicit brackets (`[1 c4 100 500],`)
- [x] Relative intervals (`+N`, `-N`)
- [x] Octave shifts (`^`, `v`)
- [x] Program change (`pc`)
- [x] Removed legacy note words (`C`, `D`, etc., `note-on`, `octave`)
- [x] Error recovery (stack checks, `clear` word)

---

## Priority 1: Core (COMPLETE)

Foundation for composition and reuse.

### Word Definitions

- [x] Implement `: ... ;` colon definitions
- [x] Compile mode vs interpret mode
- [x] Store word body as token list
- [x] Execute user-defined words by interpreting stored tokens
- [x] Warn if stack not empty after `;` (runtime check when word is executed)

```forth
: cmaj (c4 e4 g4), ;
: melody c4, e4, g4, c5, ;
cmaj melody

: broken c4, e4, g4, c5 ;
broken
Note: 'broken' left 1 item(s) on stack
```

### Rests

- [x] Implement `r` as rest (silence for default duration)
- [x] Support explicit duration: `r 250,`

```forth
c4, r, e4, r, g4,
```

### Loops

- [x] Implement `times` word
- [x] Works with user-defined words

```forth
: phrase c4, e4, g4, ;
phrase 4 times
```

---

## Priority 2: Generative / Expression (COMPLETE)

Enable generative music and expressive notation.

### Probability with `%`

- [x] Implement `%` as probabilistic play trigger
- [x] Pops probability (0-100), plays if random < probability

```forth
c4 75%,                 \ 75% chance to play
c4, e4 50%, g4,         \ C always, E maybe, G always
```

### Alternatives with `|`

- [x] Tokenizer treats `|` as word boundary
- [x] Implement alternative selection (equal probability)

```forth
c4|e4,                  \ 50% C4, 50% E4
c4|e4|g4,               \ 33.33% each
c4|r,                   \ 50% play, 50% silence
```

### Dynamics

- [x] Implement velocity presets: `ppp` `pp` `p` `mp` `mf` `f` `ff` `fff`
- [x] Each sets `default_velocity`

```forth
mf c4, e4, ff g4,
```

| Symbol | Velocity |
|--------|----------|
| `ppp`  | 16       |
| `pp`   | 32       |
| `p`    | 48       |
| `mp`   | 64       |
| `mf`   | 80       |
| `f`    | 96       |
| `ff`   | 112      |
| `fff`  | 127      |

### Square Brackets for Explicit Form

- [x] Tokenizer treats `[` `]` as word boundaries
- [x] `[` pushes marker, `]` is no-op
- [x] `,` detects bracket marker for explicit params

```forth
[1 c4 100 500],
[(c4 e4 g4) 1 80 500],
```

---

## Priority 3: Convenience (COMPLETE)

Quality of life improvements.

### Relative Intervals

- [x] Track "current pitch" state
- [x] Implement `+N` and `-N` for semitone movement

```forth
c4, +2, +2, +1,         \ C D E F
```

### Octave Shifts

- [x] Implement `^` (up octave) and `v` (down octave)
- [x] Operates on current pitch

```forth
c4, ^, v,               \ C4, C5, C4
```

### Program Change

- [x] Implement `pc` word for program/instrument change

```forth
1 0 pc                  \ Channel 1, program 0 (piano)
```

---

## Priority 4: Advanced

Extended features for complex compositions.

### Articulation

- [ ] Staccato (reduced duration)
- [ ] Accent (increased velocity)
- [ ] Tenuto (full duration)

```forth
c4.,                    \ Staccato
c4>,                    \ Accent
```

**Note**: May require tokenizer changes for suffixes.

### Pitch Bend

- [ ] Implement `pb` word for pitch bend messages

```forth
1 8192 pb               \ Center
1 16383 pb              \ Full bend up
```

### Anonymous Blocks

- [ ] Implement `{ ... }` for deferred execution
- [ ] Combine with multiplier for repetition

```forth
{ c4, e4, g4, } 4 *
```

**Note**: Less idiomatic than named word definitions.

### Conditionals

- [ ] Implement `if` `else` `then`
- [ ] Implement `random` word

```forth
: maybe-play
    random 50 > if c4, else r, then
;
```

---

## Refactoring / Technical Debt (COMPLETE)

- [x] Remove redundant old note name words (`C`, `D`, etc.) - superseded by pitch parsing
- [x] Removed `note-on` word - superseded by `,` notation
- [x] Unified packed note system with new notation (`note!` updates `current_pitch`)
- [x] Added error recovery (stack depth checks, cleanup on errors, `clear` word)

---

## Documentation

- [x] `docs/syntax.md` - syntax reference
- [ ] Update `CLAUDE.md` with new notation examples
- [ ] Add tutorial/getting started guide
- [ ] Document sequence system (`seq-*` words)

---

## Testing

- [ ] Add automated tests for pitch parsing
- [ ] Add tests for chord notation
- [ ] Add tests for probability (statistical)
- [ ] Add tests for word definitions (once implemented)
