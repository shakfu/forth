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

---

## Priority 1: Core

Foundation for composition and reuse.

### Word Definitions

- [ ] Implement `: ... ;` colon definitions
- [ ] Compile mode vs interpret mode
- [ ] Store word body as token list
- [ ] Execute user-defined words by interpreting stored tokens
- [ ] Warn if stack not empty after `;` (catches missing trailing comma)

```forth
: cmaj (c4 e4 g4), ;
: melody c4, e4, g4, c5, ;
cmaj melody

: broken c4, e4, g4, c5 ;
Warning: definition 'broken' ended with 1 item(s) on stack
```

### Rests

- [ ] Implement `r` as rest (silence for default duration)
- [ ] Support explicit duration: `r 250,`

```forth
c4, r, e4, r, g4,
```

### Loops

- [ ] Implement `do ... loop` or simpler `times` word
- [ ] Depends on word definitions

```forth
: phrase c4, e4, g4, ;
phrase 4 times
```

---

## Priority 2: Generative / Expression

Enable generative music and expressive notation.

### Probability with `%`

- [ ] Implement `%` as probabilistic play trigger
- [ ] Pops probability (0-100), plays if random < probability

```forth
c4 75%,                 \ 75% chance to play
c4, e4 50%, g4,         \ C always, E maybe, G always
```

### Alternatives with `|`

- [ ] Tokenizer treats `|` as word boundary
- [ ] Implement alternative selection (equal probability)

```forth
c4|e4,                  \ 50% C4, 50% E4
c4|e4|g4,               \ 33.33% each
c4|r,                   \ 50% play, 50% silence
```

### Dynamics

- [ ] Implement velocity presets: `ppp` `pp` `p` `mp` `mf` `f` `ff` `fff`
- [ ] Each sets `default_velocity`

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

- [ ] Tokenizer treats `[` `]` as word boundaries
- [ ] `[` pushes marker, `]` is no-op
- [ ] `,` detects bracket marker for explicit params

```forth
[1 c4 100 500],
[(c4 e4 g4) 1 80 500],
```

---

## Priority 3: Convenience

Quality of life improvements.

### Relative Intervals

- [ ] Track "current pitch" state
- [ ] Implement `+N` and `-N` for semitone movement

```forth
c4, +2, +2, +1,         \ C D E F
```

### Octave Shifts

- [ ] Implement `^` (up octave) and `v` (down octave)
- [ ] Operates on current pitch

```forth
c4, ^, v,               \ C4, C5, C4
```

### Program Change

- [ ] Implement `pc` word for program/instrument change

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

## Refactoring / Technical Debt

- [ ] Remove redundant old note name words (`C`, `D`, etc.) - superseded by pitch parsing
- [ ] Consider removing `note-on` word - superseded by `,` notation
- [ ] Unify packed note system with new notation
- [ ] Add error recovery (don't leave stack in bad state)

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
