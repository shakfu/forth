/* mhs_midi_main.c - Entry point for mhs-midi REPL
 *
 * This is a simple entry point that delegates to the MicroHs main function.
 * The MIDI FFI is made available via xffi_table defined in midi_ffi_wrappers.c
 */

int mhs_main(int argc, char **argv);

int main(int argc, char **argv)
{
    return mhs_main(argc, argv);
}
