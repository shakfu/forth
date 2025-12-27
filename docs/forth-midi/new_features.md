# Potential Features for Forth Implementation

Features ordered by implementation complexity and value.

## High Value, Lower Complexity

### 1. User-defined words (`:` and `;`)

The most essential missing feature. Currently all words are primitives.

```forth
: square dup * ;
5 square .  \ prints 25
```

Requires: storing compiled word bodies (token lists or bytecode), a separate compilation mode.

### 2. Stack inspection (`.s`)

Display stack contents without modification. Trivial to add, very useful for debugging.

### 3. Additional stack words

`2dup`, `2drop`, `2swap`, `nip`, `tuck`, `?dup` (dup if non-zero), `pick`, `depth`

### 4. Comments

`( ... )` for inline comments, `\` for line comments. Requires tokenizer modification.

### 5. More arithmetic

`mod`, `/mod`, `negate`, `abs`, `min`, `max`, `1+`, `1-`, `2*`, `2/`

## Medium Complexity

### 6. Variables and memory

```forth
variable counter
5 counter !     \ store 5
counter @       \ fetch value
```

Requires: a data space (heap), `@` (fetch), `!` (store), `variable`, `constant`, `allot`

### 7. Control structures

- `if...else...then` for conditionals
- `begin...until`, `begin...while...repeat` for indefinite loops
- `do...loop`, `+loop` with `i`, `j` for counted loops

Requires: forward references or a compilation pass.

### 8. Return stack

`>r`, `r>`, `r@` - essential for temporary storage and loop indices.

### 9. String output

`."` for printing strings, `emit` for single characters

```forth
." Hello World" cr
65 emit  \ prints 'A'
```

## Higher Complexity

### 10. Immediate words and compile-time execution

`immediate` flag, `[` and `]` for switching modes, `literal`, `'` (tick), `execute`

### 11. `create`/`does>` metaprogramming

Enables defining new defining words (arrays, structs, etc.)

### 12. File I/O

`include` to load Forth source files, or standard file words

### 13. Exception handling

`catch` and `throw`

### 14. Floating point

Separate floating-point stack and words

## Implementation Targets

Rather than adding features incrementally, consider aiming for a specific target:

- **ANS Forth Core** - A well-defined subset (~130 words) with a compliance test suite
- **jonesforth-style** - Minimal, educational, but with `:` definitions and basic control flow
- **Application-specific** - Just enough for a particular domain (e.g., embedded scripting)
