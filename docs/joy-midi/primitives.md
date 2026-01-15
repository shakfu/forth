# Joy-MIDI Primitives

This document lists all Joy primitives and their implementation status in joy-midi.

- [x] = Implemented
- [ ] = Not implemented

## Stack Operations

- [x] `dup`: Duplicate top of stack
- [x] `dup2`: Duplicate top two items
- [x] `dupd`: Duplicate second item
- [x] `pop`: Remove top of stack
- [x] `popd`: Remove second item
- [x] `swap`: Swap top two items
- [x] `swapd`: Swap second and third items
- [x] `rollup`: Roll up top three items (A B C -> C A B)
- [x] `rollupd`: Rollup under top
- [x] `rolldown`: Roll down top three items (A B C -> B C A)
- [x] `rolldownd`: Rolldown under top
- [x] `rotate`: Rotate top three items
- [x] `rotated`: Rotate under top
- [x] `over`: Copy second item to top
- [x] `pick`: Copy Nth item to top
- [x] `stack`: Push copy of stack as list
- [x] `unstack`: Replace stack with list contents
- [x] `conts`: Push continuation

## Arithmetic

- [x] `+`: Addition
- [x] `-`: Subtraction
- [x] `*`: Multiplication
- [x] `/`: Division
- [x] `rem`: Remainder/modulo
- [x] `div`: Integer division
- [x] `neg`: Negate
- [x] `abs`: Absolute value
- [x] `sign`: Sign (-1, 0, or 1)
- [x] `succ`: Successor (add 1)
- [x] `pred`: Predecessor (subtract 1)
- [x] `max`: Maximum of two values
- [x] `min`: Minimum of two values
- [x] `maxint`: Maximum integer value

## Math Functions

- [x] `sin`: Sine
- [x] `cos`: Cosine
- [x] `tan`: Tangent
- [x] `asin`: Arc sine
- [x] `acos`: Arc cosine
- [x] `atan`: Arc tangent
- [x] `atan2`: Arc tangent of y/x
- [x] `sinh`: Hyperbolic sine
- [x] `cosh`: Hyperbolic cosine
- [x] `tanh`: Hyperbolic tangent
- [x] `sqrt`: Square root
- [x] `exp`: Exponential (e^x)
- [x] `log`: Natural logarithm
- [x] `log10`: Base-10 logarithm
- [x] `pow`: Power (x^y)
- [x] `floor`: Floor
- [x] `ceil`: Ceiling
- [x] `trunc`: Truncate toward zero
- [x] `round`: Round to nearest integer
- [x] `frexp`: Extract mantissa and exponent
- [x] `ldexp`: Load exponent
- [x] `modf`: Split into integer and fraction

## Comparison

- [x] `=`: Equal
- [x] `!=`: Not equal
- [x] `<`: Less than
- [x] `>`: Greater than
- [x] `<=`: Less than or equal
- [x] `>=`: Greater than or equal
- [x] `compare`: Three-way comparison (-1, 0, 1)
- [x] `equal`: Deep equality test
- [x] `sametype`: Check if same type

## Logic

- [x] `and`: Logical AND
- [x] `or`: Logical OR
- [x] `xor`: Logical XOR
- [x] `not`: Logical NOT
- [x] `true`: Push true
- [x] `false`: Push false

## Type Predicates

- [x] `null`: Test if empty/zero/false
- [x] `small`: Test if small (0, 1, empty, or single element)
- [x] `integer`: Test if integer
- [x] `float`: Test if float
- [x] `char`: Test if character
- [x] `string`: Test if string
- [x] `list`: Test if list
- [x] `set`: Test if set
- [x] `logical`: Test if boolean
- [x] `file`: Test if file handle
- [x] `leaf`: Test if leaf (not a list)
- [x] `user`: Test if user-defined word
- [x] `typeof`: Get type code

## Type Conversion

- [x] `ord`: Character to integer
- [x] `chr`: Integer to character
- [x] `strtol`: String to integer
- [x] `strtod`: String to float
- [x] `format`: Format integer to string
- [x] `formatf`: Format float to string
- [x] `casting`: Type casting
- [x] `intern`: String to symbol
- [x] `name`: Symbol/word to string

## List/Aggregate Operations

- [x] `first`: First element
- [x] `rest`: All but first element
- [x] `cons`: Prepend element
- [x] `swons`: Swap and cons
- [x] `uncons`: Split into first and rest
- [x] `unswons`: Uncons and swap
- [x] `at`: Element at index
- [x] `of`: Index of element
- [x] `size`: Length/size
- [x] `concat`: Concatenate
- [x] `enconcat`: Enconcat (A B [C] -> [A C B])
- [x] `swoncat`: Swap and concat
- [x] `take`: Take first N elements
- [x] `drop`: Drop first N elements
- [x] `has`: Test membership
- [x] `in`: Test if element in aggregate

## Set Operations

- [x] `setsize`: Size of set

## Quotation/Combinator

- [x] `i`: Execute quotation
- [x] `x`: Execute quotation, keep quotation
- [x] `dip`: Execute under top
- [x] `nullary`: Execute, push one result
- [x] `unary`: Execute on top, push one result
- [x] `unary2`: Execute on top two
- [x] `unary3`: Execute on top three
- [x] `unary4`: Execute on top four
- [x] `binary`: Execute on top two, push one result
- [x] `ternary`: Execute on top three, push one result
- [x] `app1`: Apply to one argument
- [x] `app2`: Apply to two arguments
- [x] `app3`: Apply to three arguments
- [x] `app4`: Apply to four arguments
- [x] `app11`: Apply unary to each of two
- [x] `app12`: Apply binary, then unary
- [x] `construct`: Construct list from quotations
- [x] `cleave`: Apply multiple quotations to same value
- [x] `infra`: Execute on list as stack
- [x] `times`: Execute N times
- [x] `map`: Map quotation over list
- [x] `filter`: Filter list by predicate
- [x] `fold`: Fold/reduce list
- [x] `split`: Split list by predicate
- [x] `step`: Execute for each element
- [x] `some`: Test if any element satisfies predicate
- [x] `all`: Test if all elements satisfy predicate

## Conditionals

- [x] `branch`: If-then (no else)
- [x] `choice`: Ternary choice
- [x] `ifte`: If-then-else
- [x] `ifinteger`: Execute if integer
- [x] `iffloat`: Execute if float
- [x] `ifchar`: Execute if character
- [x] `ifstring`: Execute if string
- [x] `iflist`: Execute if list
- [x] `ifset`: Execute if set
- [x] `iflogical`: Execute if boolean
- [x] `iffile`: Execute if file
- [x] `cond`: Multi-way conditional
- [x] `opcase`: Case on operator
- [x] `case`: Case statement

## Recursion Combinators

- [x] `primrec`: Primitive recursion
- [x] `linrec`: Linear recursion
- [x] `binrec`: Binary recursion
- [x] `tailrec`: Tail recursion
- [x] `genrec`: General recursion
- [x] `condlinrec`: Conditional linear recursion
- [x] `condnestrec`: Conditional nested recursion
- [x] `treerec`: Tree recursion
- [x] `treegenrec`: General tree recursion
- [x] `treestep`: Step through tree
- [x] `while`: While loop

## I/O - Console

- [x] `.`: Print and pop
- [x] `put`: Print without newline
- [x] `putch`: Print character
- [x] `putchars`: Print string
- [x] `putln`: Print with newline
- [x] `newline`: Print newline
- [x] `get`: Read value from input
- [x] `stdin`: Standard input handle
- [x] `stdout`: Standard output handle
- [x] `stderr`: Standard error handle

## I/O - Files

- [x] `fopen`: Open file
- [x] `fclose`: Close file
- [x] `fread`: Read from file
- [x] `fwrite`: Write to file
- [x] `fgetch`: Read character from file
- [x] `fgets`: Read line from file
- [x] `fput`: Write value to file
- [x] `fputch`: Write character to file
- [x] `fputchars`: Write string to file
- [x] `fputstring`: Write string to file
- [x] `fflush`: Flush file buffer
- [x] `fseek`: Seek in file
- [x] `ftell`: Get file position
- [x] `feof`: Test end of file
- [x] `ferror`: Test file error
- [x] `fremove`: Remove file
- [x] `frename`: Rename file
- [ ] `filetime`: Get file modification time
- [ ] `finclude`: Include/execute file
- [ ] `include`: Include/execute Joy file (alias for finclude)

## Time

- [x] `time`: Current time (seconds since epoch)
- [x] `gmtime`: Convert to UTC time list
- [x] `localtime`: Convert to local time list
- [x] `mktime`: Convert time list to seconds
- [x] `strftime`: Format time as string
- [x] `clock`: CPU clock ticks

## System

- [x] `argc`: Argument count
- [x] `argv`: Argument vector
- [x] `getenv`: Get environment variable
- [x] `system`: Execute shell command
- [x] `abort`: Abort execution
- [x] `quit`: Exit interpreter

## Random Numbers

- [x] `rand`: Random integer
- [x] `srand`: Seed random generator

## Definitions

- [x] `body`: Get body of defined word
- [x] `assign`: Bind value to symbol
- [x] `unassign`: Remove symbol binding
- [x] `undefs`: List undefined symbols

## Interpreter Control

- [x] `id`: Identity (no-op)
- [x] `autoput`: Get autoput setting
- [x] `setautoput`: Set autoput mode
- [x] `echo`: Get echo setting
- [x] `setecho`: Set echo mode
- [x] `undeferror`: Get undeferror setting
- [x] `setundeferror`: Set undeferror mode
- [x] `gc`: Garbage collect
- [x] `__settracegc`: Set GC tracing
- [x] `manual`: Show manual
- [x] `help`: Show help
- [x] `helpdetail`: Show detailed help

## MIDI Primitives (joy-midi specific)

### Port Management

- [x] `midi-list`: List available MIDI output ports
- [x] `midi-virtual`: Create virtual MIDI output port
- [x] `midi-open`: Open MIDI port by index
- [x] `midi-close`: Close MIDI output port

### Note Playing

- [x] `midi-note`: Play note (pitch vel dur -> )
- [x] `midi-note-on`: Send note-on (pitch vel -> )
- [x] `midi-note-off`: Send note-off (pitch -> )
- [x] `midi-chord`: Play chord ([pitches] vel dur -> )
- [x] `play`: Play note using current settings (pitch -> )
- [x] `chord`: Play chord using current settings ([pitches] -> )

### Control

- [x] `midi-cc`: Send control change (cc val -> )
- [x] `midi-program`: Send program change (program -> )
- [x] `midi-panic`: All notes off
- [x] `midi-sleep`: Sleep for milliseconds (ms -> )

### Musical State

- [x] `tempo`: Set tempo in BPM (bpm -> )
- [x] `quant`: Set quantization 0-100 (q -> )
- [x] `vol`: Set volume 0-100 (vol -> )

### Music Theory

- [x] `pitch`: Parse pitch name to MIDI ("C4" -> 60)
- [x] `transpose`: Transpose pitch (pitch n -> pitch+n)
- [x] `major`: Build major triad (root -> [pitches])
- [x] `minor`: Build minor triad (root -> [pitches])
- [x] `dim`: Build diminished triad (root -> [pitches])
- [x] `aug`: Build augmented triad (root -> [pitches])
- [x] `dom7`: Build dominant 7th chord (root -> [pitches])
- [x] `maj7`: Build major 7th chord (root -> [pitches])
- [x] `min7`: Build minor 7th chord (root -> [pitches])

## Summary

**Standard Joy primitives: 221**
- Implemented: 218
- Not implemented: 3

**MIDI primitives: 26** (all implemented)

### Not Yet Implemented

- [ ] `filetime`: Get file modification time
- [ ] `finclude`: Include and execute Joy file
- [ ] `include`: Include and execute Joy file (alias)

### Notes

1. Most failing unit tests are due to:
   - Tests using `fopen` on files that don't exist in test environment
   - Tests expecting specific file system behavior
   - Platform-specific behavior differences

2. The `include` primitive (load Joy file) can be implemented using `finclude` once available.

3. Some primitives have aliases defined in `prelude.joy` for convenience.
