/* midi_ffi.c - C FFI implementation for MicroHs MIDI library */
#include "midi_ffi.h"
#include "music_theory.h"
#include "midi_file.h"
#include <libremidi/libremidi-c.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define usleep(us) Sleep((us) / 1000)

#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 0
#endif

static int clock_gettime(int clk_id, struct timespec *tp) {
    (void)clk_id;
    static LARGE_INTEGER freq = {0};
    LARGE_INTEGER counter;
    if (freq.QuadPart == 0) {
        QueryPerformanceFrequency(&freq);
    }
    QueryPerformanceCounter(&counter);
    tp->tv_sec = (long)(counter.QuadPart / freq.QuadPart);
    tp->tv_nsec = (long)((counter.QuadPart % freq.QuadPart) * 1000000000LL / freq.QuadPart);
    return 0;
}
#else
#include <unistd.h>
#endif

#define MAX_PORTS 64
#define MAX_PORT_NAME 256
#define MAX_CAPTURE_EVENTS 4096
#define TICKS_PER_QUARTER 480

/* Global state */
static libremidi_midi_observer_handle* g_observer = NULL;
static libremidi_midi_out_handle* g_midi_out = NULL;
static libremidi_midi_out_port* g_ports[MAX_PORTS];
static char g_port_names[MAX_PORTS][MAX_PORT_NAME];
static int g_port_count = 0;
static int g_initialized = 0;

/* MIDI capture state */
typedef struct {
    uint32_t time_ms;
    uint8_t type;      /* 0=note-on, 1=note-off, 2=cc */
    uint8_t channel;
    uint8_t data1;
    uint8_t data2;
} CapturedEvent;

static CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
static int capture_count = 0;
static int capture_active = 0;
static struct timespec capture_start_time;
static int capture_bpm = 120;

/* Get current time in ms since capture start */
static uint32_t capture_get_time_ms(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    uint64_t start_ms = (uint64_t)capture_start_time.tv_sec * 1000 +
                        (uint64_t)capture_start_time.tv_nsec / 1000000;
    uint64_t now_ms = (uint64_t)now.tv_sec * 1000 +
                      (uint64_t)now.tv_nsec / 1000000;
    return (uint32_t)(now_ms - start_ms);
}

/* Add event to capture buffer */
static void capture_add_event(int type, int channel, int data1, int data2) {
    if (!capture_active) return;
    if (capture_count >= MAX_CAPTURE_EVENTS) {
        printf("Capture buffer full!\n");
        capture_active = 0;
        return;
    }
    CapturedEvent* e = &capture_buffer[capture_count++];
    e->time_ms = capture_get_time_ms();
    e->type = type;
    e->channel = channel;
    e->data1 = data1;
    e->data2 = data2;
}

/* Callback for port enumeration */
static void port_callback(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (g_port_count >= MAX_PORTS) return;

    /* Clone the port */
    if (libremidi_midi_out_port_clone(port, &g_ports[g_port_count]) != 0) {
        return;
    }

    /* Get the port name */
    const char* name = NULL;
    size_t len = 0;
    if (libremidi_midi_out_port_name(port, &name, &len) == 0 && name) {
        size_t copy_len = len < MAX_PORT_NAME - 1 ? len : MAX_PORT_NAME - 1;
        memcpy(g_port_names[g_port_count], name, copy_len);
        g_port_names[g_port_count][copy_len] = '\0';
    } else {
        snprintf(g_port_names[g_port_count], MAX_PORT_NAME, "Port %d", g_port_count);
    }

    g_port_count++;
}

int midi_init(void) {
    if (g_initialized) return 0;

    /* Initialize observer configuration */
    libremidi_observer_configuration obs_conf;
    libremidi_midi_observer_configuration_init(&obs_conf);

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    /* Create observer */
    if (libremidi_midi_observer_new(&obs_conf, &api_conf, &g_observer) != 0) {
        return -1;
    }

    g_initialized = 1;
    return 0;
}

void midi_cleanup(void) {
    midi_close();

    /* Free ports */
    for (int i = 0; i < g_port_count; i++) {
        if (g_ports[i]) {
            libremidi_midi_out_port_free(g_ports[i]);
            g_ports[i] = NULL;
        }
    }
    g_port_count = 0;

    /* Free observer */
    if (g_observer) {
        libremidi_midi_observer_free(g_observer);
        g_observer = NULL;
    }

    g_initialized = 0;
}

int midi_list_ports(void) {
    if (!g_initialized) {
        if (midi_init() != 0) return 0;
    }

    /* Free any existing ports */
    for (int i = 0; i < g_port_count; i++) {
        if (g_ports[i]) {
            libremidi_midi_out_port_free(g_ports[i]);
            g_ports[i] = NULL;
        }
    }
    g_port_count = 0;

    /* Enumerate ports */
    libremidi_midi_observer_enumerate_output_ports(g_observer, NULL, port_callback);

    return g_port_count;
}

char* midi_port_name(int index) {
    if (index < 0 || index >= g_port_count) {
        return "";
    }
    return g_port_names[index];
}

int midi_open(int port_index) {
    if (!g_initialized) {
        if (midi_init() != 0) return -1;
    }

    if (port_index < 0 || port_index >= g_port_count) {
        return -1;
    }

    /* Close any existing connection */
    midi_close();

    /* Initialize MIDI configuration */
    libremidi_midi_configuration midi_conf;
    libremidi_midi_configuration_init(&midi_conf);
    midi_conf.version = MIDI1;
    midi_conf.out_port = g_ports[port_index];

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Create MIDI output */
    if (libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out) != 0) {
        g_midi_out = NULL;
        return -1;
    }

    return 0;
}

int midi_open_virtual(const char* name) {
    if (!g_initialized) {
        if (midi_init() != 0) return -1;
    }

    /* Close any existing connection */
    midi_close();

    /* Initialize MIDI configuration for virtual port */
    libremidi_midi_configuration midi_conf;
    libremidi_midi_configuration_init(&midi_conf);
    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = name ? name : "MhsMidi";

    /* Initialize API configuration */
    libremidi_api_configuration api_conf;
    libremidi_midi_api_configuration_init(&api_conf);
    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Create MIDI output */
    if (libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out) != 0) {
        g_midi_out = NULL;
        return -1;
    }

    return 0;
}

void midi_close(void) {
    if (g_midi_out) {
        midi_panic();
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
    }
}

int midi_is_open(void) {
    return g_midi_out != NULL;
}

int midi_send(uint8_t status, uint8_t data1, uint8_t data2) {
    if (!g_midi_out) return -1;

    unsigned char msg[3] = { status, data1 & 0x7F, data2 & 0x7F };
    return libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

int midi_note_on(int channel, int pitch, int velocity) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0x90 | ((channel - 1) & 0x0F);
    int result = midi_send(status, (uint8_t)pitch, (uint8_t)velocity);
    capture_add_event(0, channel - 1, pitch, velocity);
    return result;
}

int midi_note_off(int channel, int pitch) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0x80 | ((channel - 1) & 0x0F);
    int result = midi_send(status, (uint8_t)pitch, 0);
    capture_add_event(1, channel - 1, pitch, 0);
    return result;
}

int midi_cc(int channel, int controller, int value) {
    if (channel < 1 || channel > 16) return -1;
    uint8_t status = 0xB0 | ((channel - 1) & 0x0F);
    int result = midi_send(status, (uint8_t)controller, (uint8_t)value);
    capture_add_event(2, channel - 1, controller, value);
    return result;
}

int midi_program(int channel, int program) {
    if (!g_midi_out) return -1;
    if (channel < 1 || channel > 16) return -1;

    unsigned char msg[2];
    msg[0] = 0xC0 | ((channel - 1) & 0x0F);
    msg[1] = (uint8_t)(program & 0x7F);
    return libremidi_midi_out_send_message(g_midi_out, msg, 2);
}

int midi_pitch_bend(int channel, int value) {
    if (!g_midi_out) return -1;
    if (channel < 1 || channel > 16) return -1;

    /* Convert from -8192..8191 to 0..16383 */
    int bent = value + 8192;
    if (bent < 0) bent = 0;
    if (bent > 16383) bent = 16383;

    unsigned char msg[3];
    msg[0] = 0xE0 | ((channel - 1) & 0x0F);
    msg[1] = (uint8_t)(bent & 0x7F);        /* LSB */
    msg[2] = (uint8_t)((bent >> 7) & 0x7F); /* MSB */
    return libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

void midi_sleep(int ms) {
    if (ms > 0) {
        usleep((useconds_t)(ms * 1000));
    }
}

void midi_panic(void) {
    if (!g_midi_out) return;

    /* Send all notes off on all 16 channels */
    for (int ch = 1; ch <= 16; ch++) {
        midi_cc(ch, 123, 0);  /* All Notes Off */
    }
}

int midi_cents_to_bend(int cents) {
    return music_cents_to_bend(cents);
}

/* Random number generation */
static int g_random_initialized = 0;

void midi_seed_random(int seed) {
    srand((unsigned int)seed);
    g_random_initialized = 1;
}

int midi_random(void) {
    if (!g_random_initialized) {
        srand((unsigned int)time(NULL));
        g_random_initialized = 1;
    }
    return rand();
}

int midi_random_range(int min, int max) {
    if (min >= max) return min;
    int r = midi_random();
    return min + (r % (max - min + 1));
}

/* MIDI recording functions */
int midi_record_start(int bpm) {
    capture_bpm = bpm;
    if (capture_bpm < 1) capture_bpm = 1;
    if (capture_bpm > 300) capture_bpm = 300;
    capture_count = 0;
    capture_active = 1;
    clock_gettime(CLOCK_MONOTONIC, &capture_start_time);
    printf("MIDI recording started at %d BPM\n", capture_bpm);
    return 0;
}

int midi_record_stop(void) {
    if (capture_active) {
        capture_active = 0;
        printf("MIDI recording stopped. %d events recorded.\n", capture_count);
    } else {
        printf("Recording not active.\n");
    }
    return capture_count;
}

int midi_save_mid(const char* filename) {
    if (capture_count == 0) {
        printf("No events recorded.\n");
        return -1;
    }

    /* Create MIDI file writer */
    midi_file_writer* writer = NULL;
    if (midi_file_writer_new(&writer) != 0) {
        printf("Cannot create MIDI file writer\n");
        return -1;
    }

    /* Set PPQN and add a track */
    midi_file_writer_set_ppqn(writer, TICKS_PER_QUARTER);
    midi_file_writer_add_track(writer);

    /* Add tempo meta event */
    int us_per_beat = 60000000 / capture_bpm;
    midi_file_writer_tempo(writer, 0, 0, us_per_beat);

    /* Convert captured events to MIDI file events */
    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];
        int tick = (e->time_ms * capture_bpm * TICKS_PER_QUARTER) / 60000;

        if (e->type == 0) {
            /* Note-on */
            midi_file_writer_note_on(writer, tick, 0, e->channel + 1, e->data1, e->data2);
        } else if (e->type == 1) {
            /* Note-off */
            midi_file_writer_note_off(writer, tick, 0, e->channel + 1, e->data1, 0);
        } else if (e->type == 2) {
            /* Control change */
            midi_file_writer_cc(writer, tick, 0, e->channel + 1, e->data1, e->data2);
        }
    }

    /* Write to file */
    int result = midi_file_writer_save(writer, filename);
    midi_file_writer_free(writer);

    if (result != 0) {
        printf("Failed to write MIDI file: %s\n", filename);
        return -1;
    }

    printf("Saved %d events to %s\n", capture_count, filename);
    return 0;
}

int midi_record_count(void) {
    return capture_count;
}

int midi_record_active(void) {
    return capture_active;
}

/* Save recording as Haskell source file */
int midi_save_hs(const char* filename) {
    if (capture_count == 0) {
        printf("No events recorded.\n");
        return -1;
    }

    FILE* f = fopen(filename, "w");
    if (!f) {
        printf("Cannot open file: %s\n", filename);
        return -1;
    }

    fprintf(f, "-- MIDI recording - %d events at %d BPM\n", capture_count, capture_bpm);
    fprintf(f, "-- Replay with: :load %s\n\n", filename);
    fprintf(f, "import Midi\n");
    fprintf(f, "import Data.List (foldl')\n\n");
    fprintf(f, "events :: [(Int, Int, Int, Int, Int)]\n");
    fprintf(f, "events = [\n");

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];
        fprintf(f, "  (%u, %d, %d, %d, %d)%s\n",
                e->time_ms, e->type, e->channel + 1, e->data1, e->data2,
                i < capture_count - 1 ? "," : "");
    }

    fprintf(f, "  ]\n\n");
    fprintf(f, "playEvent :: (Int, Int, Int, Int, Int) -> IO ()\n");
    fprintf(f, "playEvent (_, 0, ch, pitch, vel) = do _ <- noteOn ch pitch vel; return ()\n");
    fprintf(f, "playEvent (_, 1, ch, pitch, _)   = do _ <- noteOff ch pitch; return ()\n");
    fprintf(f, "playEvent (_, 2, ch, ctrl, val)  = do _ <- cc ch ctrl val; return ()\n");
    fprintf(f, "playEvent _                       = return ()\n\n");
    fprintf(f, "-- Replay with original timing (uses delta delays between events)\n");
    fprintf(f, "replay :: IO ()\n");
    fprintf(f, "replay = do\n");
    fprintf(f, "  _ <- openVirtual \"MhsMidi\"\n");
    fprintf(f, "  go 0 events\n");
    fprintf(f, "  panic\n");
    fprintf(f, "  where\n");
    fprintf(f, "    go _ [] = return ()\n");
    fprintf(f, "    go lastTime ((t, ty, ch, d1, d2):rest) = do\n");
    fprintf(f, "      let delay = t - lastTime\n");
    fprintf(f, "      if delay > 0 then sleep delay else return ()\n");
    fprintf(f, "      playEvent (t, ty, ch, d1, d2)\n");
    fprintf(f, "      go t rest\n");

    fclose(f);
    printf("Saved %d events to %s\n", capture_count, filename);
    return 0;
}

/* Callback for reading MIDI file events */
static void read_mid_callback(void* ctx, const midi_file_event* event) {
    (void)ctx;

    /* Filter meta events for tempo display */
    if (event->type == 0x51) {
        printf("  tempo: %d us/beat (%d BPM)\n",
               event->data1, 60000000 / event->data1);
        return;
    }

    /* Only show channel events */
    if (event->type < 0x80 || event->type >= 0xF0) return;

    int status = event->type & 0xF0;
    const char* type_name = "unknown";
    switch (status) {
        case 0x80: type_name = "note-off"; break;
        case 0x90: type_name = (event->data2 > 0) ? "note-on" : "note-off"; break;
        case 0xA0: type_name = "aftertouch"; break;
        case 0xB0: type_name = "cc"; break;
        case 0xC0: type_name = "program"; break;
        case 0xD0: type_name = "ch-pressure"; break;
        case 0xE0: type_name = "pitch-bend"; break;
    }

    printf("  t=%5d ch=%2d %-10s %3d %3d\n",
           event->tick, event->channel + 1, type_name, event->data1, event->data2);
}

int midi_read_mid(const char* filename) {
    midi_file_reader* reader = NULL;
    if (midi_file_reader_new(&reader) != 0) {
        printf("Cannot create MIDI file reader\n");
        return -1;
    }

    int result = midi_file_reader_load(reader, filename);
    if (result == 0) {
        midi_file_reader_free(reader);
        printf("Cannot read MIDI file: %s\n", filename);
        return -1;
    }

    /* Display file info */
    int num_tracks = midi_file_reader_num_tracks(reader);
    float ppqn = midi_file_reader_ppqn(reader);
    float tempo = midi_file_reader_tempo(reader);
    float duration = midi_file_reader_duration(reader);
    int format = midi_file_reader_format(reader);

    printf("MIDI file: %s\n", filename);
    printf("  format: %d\n", format);
    printf("  tracks: %d\n", num_tracks);
    printf("  ppqn: %.0f\n", ppqn);
    printf("  tempo: %.0f us/beat (%.0f BPM)\n", tempo, 60000000.0 / tempo);
    printf("  duration: %.0f ms\n", duration);
    printf("Events:\n");

    /* Display events */
    midi_file_reader_for_each(reader, NULL, read_mid_callback);

    midi_file_reader_free(reader);
    return 0;
}
