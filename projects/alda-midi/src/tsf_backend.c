/**
 * @file tsf_backend.c
 * @brief Built-in synthesizer using TinySoundFont + miniaudio.
 */

#define TSF_IMPLEMENTATION
#include "tsf.h"

#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"

#include "alda/tsf_backend.h"
#include <stdio.h>
#include <string.h>
#include <pthread.h>

/* ============================================================================
 * Constants
 * ============================================================================ */

#define TSF_SAMPLE_RATE     44100
#define TSF_CHANNELS        2
#define TSF_PERIOD_FRAMES   512
#define TSF_MAX_VOICES      256

/* ============================================================================
 * Backend State (global singleton)
 * ============================================================================ */

typedef struct {
    tsf* synth;
    ma_device device;
    int device_initialized;
    int enabled;
    int initialized;
    pthread_mutex_t mutex;
} TsfBackend;

static TsfBackend g_tsf = {0};

/* ============================================================================
 * Audio Callback
 * ============================================================================ */

static void tsf_audio_callback(ma_device* device, void* output, const void* input, ma_uint32 frame_count) {
    (void)device;
    (void)input;

    float* out = (float*)output;

    pthread_mutex_lock(&g_tsf.mutex);
    if (g_tsf.synth && g_tsf.enabled) {
        tsf_render_float(g_tsf.synth, out, (int)frame_count, 0);
    } else {
        memset(out, 0, (size_t)frame_count * TSF_CHANNELS * sizeof(float));
    }
    pthread_mutex_unlock(&g_tsf.mutex);
}

/* ============================================================================
 * Initialization and Cleanup
 * ============================================================================ */

int alda_tsf_init(void) {
    if (g_tsf.initialized) {
        return 0;  /* Already initialized */
    }

    memset(&g_tsf, 0, sizeof(g_tsf));

    if (pthread_mutex_init(&g_tsf.mutex, NULL) != 0) {
        fprintf(stderr, "TSF: Failed to create mutex\n");
        return -1;
    }

    g_tsf.initialized = 1;
    return 0;
}

void alda_tsf_cleanup(void) {
    if (!g_tsf.initialized) {
        return;
    }

    /* Disable and stop audio */
    alda_tsf_disable();

    pthread_mutex_lock(&g_tsf.mutex);

    /* Close soundfont */
    if (g_tsf.synth) {
        tsf_close(g_tsf.synth);
        g_tsf.synth = NULL;
    }

    pthread_mutex_unlock(&g_tsf.mutex);

    pthread_mutex_destroy(&g_tsf.mutex);
    g_tsf.initialized = 0;
}

/* ============================================================================
 * Soundfont Management
 * ============================================================================ */

int alda_tsf_load_soundfont(const char* path) {
    if (!g_tsf.initialized) {
        fprintf(stderr, "TSF: Backend not initialized\n");
        return -1;
    }

    if (!path) {
        fprintf(stderr, "TSF: NULL path\n");
        return -1;
    }

    pthread_mutex_lock(&g_tsf.mutex);

    /* Close existing soundfont */
    if (g_tsf.synth) {
        tsf_close(g_tsf.synth);
        g_tsf.synth = NULL;
    }

    /* Load new soundfont */
    g_tsf.synth = tsf_load_filename(path);
    if (!g_tsf.synth) {
        pthread_mutex_unlock(&g_tsf.mutex);
        fprintf(stderr, "TSF: Failed to load soundfont: %s\n", path);
        return -1;
    }

    /* Configure output: 0.0 dB = full volume */
    tsf_set_output(g_tsf.synth, TSF_STEREO_INTERLEAVED, TSF_SAMPLE_RATE, 0.0f);
    tsf_set_max_voices(g_tsf.synth, TSF_MAX_VOICES);

    pthread_mutex_unlock(&g_tsf.mutex);

    return 0;
}

int alda_tsf_has_soundfont(void) {
    return g_tsf.initialized && g_tsf.synth != NULL;
}

int alda_tsf_get_preset_count(void) {
    if (!g_tsf.synth) {
        return 0;
    }
    return tsf_get_presetcount(g_tsf.synth);
}

const char* alda_tsf_get_preset_name(int index) {
    if (!g_tsf.synth) {
        return NULL;
    }
    if (index < 0 || index >= tsf_get_presetcount(g_tsf.synth)) {
        return NULL;
    }
    return tsf_get_presetname(g_tsf.synth, index);
}

/* ============================================================================
 * Enable/Disable
 * ============================================================================ */

int alda_tsf_enable(void) {
    if (!g_tsf.initialized) {
        fprintf(stderr, "TSF: Backend not initialized\n");
        return -1;
    }

    if (!g_tsf.synth) {
        fprintf(stderr, "TSF: No soundfont loaded\n");
        return -1;
    }

    if (g_tsf.enabled) {
        return 0;  /* Already enabled */
    }

    /* Initialize audio device if needed */
    if (!g_tsf.device_initialized) {
        ma_device_config config = ma_device_config_init(ma_device_type_playback);
        config.playback.format = ma_format_f32;
        config.playback.channels = TSF_CHANNELS;
        config.sampleRate = TSF_SAMPLE_RATE;
        config.dataCallback = tsf_audio_callback;
        config.pUserData = NULL;
        config.periodSizeInFrames = TSF_PERIOD_FRAMES;

        ma_result result = ma_device_init(NULL, &config, &g_tsf.device);
        if (result != MA_SUCCESS) {
            fprintf(stderr, "TSF: Failed to initialize audio device: %d\n", result);
            return -1;
        }

        g_tsf.device_initialized = 1;
    }

    /* Start audio playback */
    ma_result result = ma_device_start(&g_tsf.device);
    if (result != MA_SUCCESS) {
        fprintf(stderr, "TSF: Failed to start audio device: %d\n", result);
        return -1;
    }

    g_tsf.enabled = 1;
    return 0;
}

void alda_tsf_disable(void) {
    if (!g_tsf.initialized || !g_tsf.enabled) {
        return;
    }

    /* Stop all notes */
    alda_tsf_all_notes_off();

    /* Stop audio device */
    if (g_tsf.device_initialized) {
        ma_device_stop(&g_tsf.device);
    }

    g_tsf.enabled = 0;
}

int alda_tsf_is_enabled(void) {
    return g_tsf.initialized && g_tsf.enabled;
}

/* ============================================================================
 * MIDI Message Sending
 * ============================================================================ */

void alda_tsf_send_note_on(int channel, int pitch, int velocity) {
    if (!g_tsf.synth || !g_tsf.enabled) {
        return;
    }

    /* Convert velocity 0-127 to float 0.0-1.0 */
    float vel = (float)velocity / 127.0f;

    /* Channel is 1-16, TSF uses 0-15 */
    int ch = (channel - 1) & 0x0F;

    pthread_mutex_lock(&g_tsf.mutex);
    tsf_channel_note_on(g_tsf.synth, ch, pitch, vel);
    pthread_mutex_unlock(&g_tsf.mutex);
}

void alda_tsf_send_note_off(int channel, int pitch) {
    if (!g_tsf.synth || !g_tsf.enabled) {
        return;
    }

    /* Channel is 1-16, TSF uses 0-15 */
    int ch = (channel - 1) & 0x0F;

    pthread_mutex_lock(&g_tsf.mutex);
    tsf_channel_note_off(g_tsf.synth, ch, pitch);
    pthread_mutex_unlock(&g_tsf.mutex);
}

void alda_tsf_send_program(int channel, int program) {
    if (!g_tsf.synth || !g_tsf.enabled) {
        return;
    }

    /* Channel is 1-16, TSF uses 0-15 */
    int ch = (channel - 1) & 0x0F;

    /* Use bank 0 for GM instruments, drum channel 10 uses drum bank */
    int is_drum = (channel == 10);

    pthread_mutex_lock(&g_tsf.mutex);
    tsf_channel_set_presetnumber(g_tsf.synth, ch, program, is_drum);
    pthread_mutex_unlock(&g_tsf.mutex);
}

void alda_tsf_send_cc(int channel, int cc, int value) {
    if (!g_tsf.synth || !g_tsf.enabled) {
        return;
    }

    /* Channel is 1-16, TSF uses 0-15 */
    int ch = (channel - 1) & 0x0F;

    pthread_mutex_lock(&g_tsf.mutex);
    tsf_channel_midi_control(g_tsf.synth, ch, cc, value);
    pthread_mutex_unlock(&g_tsf.mutex);
}

void alda_tsf_all_notes_off(void) {
    if (!g_tsf.synth) {
        return;
    }

    pthread_mutex_lock(&g_tsf.mutex);
    tsf_note_off_all(g_tsf.synth);
    pthread_mutex_unlock(&g_tsf.mutex);
}
