# Missing Forth Features Analysis

This document catalogs features that standard Forth implementations provide but forth-midi lacks, and analyzes whether adding them would benefit the MIDI composition use case.

## Executive Summary

forth-midi is a Forth-flavored DSL, not a full Forth. Several missing features would genuinely enhance musical expressiveness (variables, loops, recursion). Others are implementation machinery that would add complexity without clear benefit for the target users (musicians, not language implementers).

**Recommended additions**: Variables, `DO`/`LOOP`, `BEGIN`/`UNTIL`, recursion support.

**Not recommended**: Full memory model, `CREATE`/`DOES>`, execution tokens.

---

## Feature Analysis

### 1. Return Stack

**What it is**: A second stack used for control flow, loop indices, and temporary storage. Words like `>R`, `R>`, `R@` move values between data and return stacks.

**Standard Forth usage**:

```forth
: example  5 >R  R@ .  R> . ;   \ Push 5 to return stack, fetch, pop
```

**Relevance to forth-midi**: Low. The return stack's primary purpose is supporting nested word calls and loop control. forth-midi's interpreted approach (storing word bodies as strings and re-parsing) sidesteps the need for explicit return address management.

**Recommendation**: Skip. Would require significant architectural changes for marginal benefit. If `DO`/`LOOP` is added, loop indices can be managed differently.

---

### 2. Memory Access (`@` and `!`)

**What it is**: Fetch (`@`) reads from a memory address; store (`!`) writes to it. Foundation for variables and data structures.

**Standard Forth usage**:

```forth
VARIABLE counter
5 counter !      \ Store 5 in counter
counter @ .      \ Fetch and print: 5
```

**Relevance to forth-midi**: Medium-high for the variable use case, low for raw memory access.

Musicians would benefit from named state:

```forth
VARIABLE transpose
-2 transpose !
c4 transpose @ + ,   \ Play Bb4
```

But raw pointer arithmetic (`1000 @`) has no musical meaning.

**Recommendation**: Implement variables (see below) without exposing a raw memory model. The existing domain-specific accessors (`pitch@`, `vel@`, `bpm!`) already establish this pattern.

---

### 3. `VARIABLE` and `CONSTANT`

**What it is**: `VARIABLE name` allocates storage; `CONSTANT name` creates an immutable binding.

**Standard Forth usage**:

```forth
VARIABLE tempo
120 tempo !

60 CONSTANT middle-c
```

**Relevance to forth-midi**: High. Currently there's no way to store persistent state across word invocations except through global defaults (`ch!`, `vel!`, etc.).

**Use cases**:

- Store a transposition offset
- Count loop iterations
- Toggle between modes (e.g., major/minor)
- Store a root note for chord progressions

**Implementation approach**:

```c
// Add to Word struct:
int32_t value;      // For variables/constants
int is_variable;    // 1=variable, 2=constant

// VARIABLE pushes address (word index), @ fetches, ! stores
```

**Recommendation**: Add. High value, moderate implementation effort. Consider a simplified form where `VARIABLE x` makes `x` push its value and `x!` stores (avoiding the address indirection).

---

### 4. `CREATE` and `DOES>`

**What it is**: Metaprogramming primitives. `CREATE` makes a new dictionary entry. `DOES>` defines runtime behavior for words created by a defining word.

**Standard Forth usage**:

```forth
: CONSTANT  CREATE , DOES> @ ;
: ARRAY  CREATE CELLS ALLOT DOES> SWAP CELLS + ;
```

**Relevance to forth-midi**: Low. This is language-implementation machinery. Musicians don't need to define new defining words.

**Recommendation**: Skip. The complexity-to-benefit ratio is poor for the target audience. If specific patterns emerge (e.g., "I need arrays"), implement them directly in C rather than exposing the meta-level.

---

### 5. `IMMEDIATE` Words

**What it is**: Marks a word to execute at compile time rather than being compiled into definitions.

**Standard Forth usage**:

```forth
: [CHAR]  CHAR POSTPONE LITERAL ; IMMEDIATE
```

**Relevance to forth-midi**: Low. The current compile mode is simple (accumulate tokens as a string). Immediate words would require distinguishing compile-time vs. runtime execution during definition parsing.

**Potential use case**: Compile-time computation of pitch values. But this optimization is unnecessary given the interpreted approach.

**Recommendation**: Skip. Not worth the complexity.

---

### 6. Tick (`'`) and `EXECUTE`

**What it is**: `'` returns the execution token (address) of the following word. `EXECUTE` runs a token from the stack.

**Standard Forth usage**:

```forth
' dup EXECUTE   \ Same as: dup
: apply  ' EXECUTE ;
```

**Relevance to forth-midi**: Medium. Could enable higher-order patterns:

```forth
: map-notes  ( xt n1 n2 n3 -- ) ... ;
' transpose 3  c4 e4 g4  map-notes
```

However, anonymous blocks (`{ ... }`) already provide deferred execution:

```forth
{ 2 + } c4 e4 g4 3 apply-to-each
```

**Recommendation**: Defer. Anonymous blocks cover most use cases. Revisit if users request explicit word references.

---

### 7. `DO`/`LOOP` and `BEGIN`/`UNTIL`/`WHILE`/`REPEAT`

**What it is**: Standard loop constructs.

**Standard Forth usage**:

```forth
: scale  8 0 DO  I 60 + ,  LOOP ;           \ Chromatic scale
: wait-for-note  BEGIN key? UNTIL ;          \ Poll until keypress
: countdown  10 BEGIN DUP . 1- DUP 0= UNTIL DROP ;
```

**Current forth-midi alternatives**:

- `N times` repeats the previous word N times
- `{ ... } N *` repeats a block N times

**Gaps**:

- No loop index access (`I`, `J`)
- No conditional exit (`LEAVE`)
- No indefinite loops (repeat until condition)

**Use cases**:

```forth
\ With DO/LOOP:
: arpeggio  4 0 DO  I 4 * 60 + ,  200 ms  LOOP ;

\ With BEGIN/UNTIL:
: fade-out  127 BEGIN  DUP vel! c4,  5 - DUP 0< UNTIL DROP ;
```

**Recommendation**: Add. High value for generative/algorithmic music. Start with `DO`/`LOOP` (bounded iteration with index), then consider `BEGIN`/`UNTIL` for reactive/conditional patterns.

**Implementation sketch**:

- `DO`: Push limit and index to a loop stack
- `I`: Push current index
- `LOOP`: Increment index, branch back if < limit
- Requires tracking loop entry points (could reuse block mechanism)

---

### 8. Addressable Memory (`HERE`, `ALLOT`, `CELLS`)

**What it is**: Linear memory allocation. `HERE` returns the next free address. `ALLOT` reserves space. `CELLS` converts count to bytes.

**Standard Forth usage**:

```forth
CREATE buffer 100 CELLS ALLOT
buffer 10 + @ .   \ Read 10th cell
```

**Relevance to forth-midi**: Low. Musicians don't think in terms of memory addresses. Specific data structures (sequences, note arrays) are better exposed as high-level abstractions.

The existing sequence system (`seq-new`, `seq-note`, `seq-play`) demonstrates this approach: structured data without exposing memory layout.

**Recommendation**: Skip. Continue adding domain-specific data structures as needed rather than exposing raw memory.

---

### 9. `RECURSE`

**What it is**: Calls the word currently being defined (since the word isn't in the dictionary yet during compilation).

**Standard Forth usage**:

```forth
: factorial  DUP 1 > IF DUP 1- RECURSE * THEN ;
```

**Relevance to forth-midi**: Medium. Recursive patterns are natural for musical structures:

```forth
: fractal-melody  DUP 0> IF
    DUP c4 + ,
    1- RECURSE
    DUP e4 + ,
  THEN DROP ;
```

However, deep recursion could cause issues with the current string-reparsing execution model.

**Recommendation**: Add with care. Useful for generative music. Implement with a recursion depth limit to prevent runaway execution.

---

### 10. String Handling (`S"`, `."`, `TYPE`)

**What it is**: String literals and output.

**Standard Forth usage**:

```forth
: greet  ." Hello, musician!" CR ;
S" filename.mid" SAVE-MIDI
```

**Current forth-midi status**: Has `\` comments and implicit string handling in `load` and `save` commands.

**Relevance**: Low-medium. Could improve user feedback and file operations, but not core to music generation.

**Recommendation**: Low priority. The current command-style interface (`load myfile.4th`) works adequately.

---

## Implementation Priority Matrix

| Feature | Value for Musicians | Implementation Effort | Priority |
|---------|--------------------|-----------------------|----------|
| `VARIABLE`/`CONSTANT` | High | Medium | **1** |
| `DO`/`LOOP` with `I` | High | Medium | **2** |
| `BEGIN`/`UNTIL` | Medium-High | Medium | **3** |
| `RECURSE` | Medium | Low | **4** |
| `'`/`EXECUTE` | Medium | Medium | 5 |
| Return stack | Low | High | Skip |
| Raw `@`/`!` | Low | Medium | Skip |
| `CREATE`/`DOES>` | Low | High | Skip |
| `IMMEDIATE` | Low | High | Skip |
| Memory model | Low | High | Skip |

---

## Philosophical Note

Forth's power comes from its ability to extend itself - to define new control structures, new data types, new syntax. This is valuable when building systems software or when the language must adapt to unforeseen requirements.

forth-midi has a narrower, well-defined purpose: expressive MIDI composition. The missing features fall into two categories:

1. **User-facing gaps**: Variables, loops, recursion. These limit what musicians can express. Add them.

2. **Meta-level machinery**: `CREATE`/`DOES>`, `IMMEDIATE`, memory model. These enable language extension but add cognitive overhead. Skip them and implement specific features directly in C when needed.

The goal is a language that feels Forth-like (stack-based, compositional, terse) while remaining approachable for musicians who aren't systems programmers.
