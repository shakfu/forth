/*
 * lua_midi - Lua-based MIDI language using Lua 5.5
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

/* External functions from midi_module.c */
extern int luaopen_midi(lua_State *L);
extern void lua_midi_cleanup(void);

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] [file.lua]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -e EXPR    Execute Lua statement\n");
    fprintf(stderr, "  --version  Show version\n");
    fprintf(stderr, "  --help     Show this help\n");
    fprintf(stderr, "\nWithout arguments, starts an interactive REPL.\n");
}

static void print_error(lua_State *L) {
    const char *msg = lua_tostring(L, -1);
    if (msg) {
        fprintf(stderr, "Error: %s\n", msg);
    }
    lua_pop(L, 1);
}

static int run_string(lua_State *L, const char *code) {
    int status = luaL_loadstring(L, code);
    if (status == LUA_OK) {
        status = lua_pcall(L, 0, LUA_MULTRET, 0);
    }
    if (status != LUA_OK) {
        print_error(L);
        return 1;
    }
    /* Print results if any */
    int n = lua_gettop(L);
    if (n > 0) {
        luaL_checkstack(L, LUA_MINSTACK, "too many results to print");
        lua_getglobal(L, "print");
        lua_insert(L, 1);
        if (lua_pcall(L, n, 0, 0) != LUA_OK) {
            fprintf(stderr, "error calling 'print' (%s)\n", lua_tostring(L, -1));
        }
    }
    return 0;
}

static int run_file(lua_State *L, const char *filename) {
    int status = luaL_loadfile(L, filename);
    if (status == LUA_OK) {
        status = lua_pcall(L, 0, LUA_MULTRET, 0);
    }
    if (status != LUA_OK) {
        print_error(L);
        return 1;
    }
    return 0;
}

static void repl(lua_State *L) {
    printf("lua_midi - Lua MIDI language (Lua 5.5)\n");
    printf("Type help() for available functions, quit() or Ctrl-D to exit\n\n");

    while (1) {
#ifdef USE_READLINE
        char *line = readline("> ");
        if (!line) {
            printf("\n");
            break;  /* EOF or error */
        }

        /* Skip empty lines */
        if (line[0] == '\0') {
            free(line);
            continue;
        }

        /* Add to history */
        add_history(line);

        /* Check for quit */
        if (strcmp(line, "quit()") == 0 || strcmp(line, "exit()") == 0) {
            free(line);
            break;
        }

        /* Copy to buffer for processing */
        char buffer[4096];
        strncpy(buffer, line, sizeof(buffer) - 1);
        buffer[sizeof(buffer) - 1] = '\0';
        free(line);
#else
        char buffer[4096];
        printf("> ");
        fflush(stdout);

        if (!fgets(buffer, sizeof(buffer), stdin)) {
            printf("\n");
            break;  /* EOF or error */
        }

        /* Skip empty lines */
        size_t len = strlen(buffer);
        if (len == 0 || (len == 1 && buffer[0] == '\n')) {
            continue;
        }

        /* Remove trailing newline */
        if (buffer[len - 1] == '\n') {
            buffer[len - 1] = '\0';
        }

        /* Check for quit */
        if (strcmp(buffer, "quit()") == 0 || strcmp(buffer, "exit()") == 0) {
            break;
        }
#endif

        /* Try as expression first (prepend return) */
        char expr_buffer[4096 + 16];
        snprintf(expr_buffer, sizeof(expr_buffer), "return %s", buffer);

        int status = luaL_loadstring(L, expr_buffer);
        if (status != LUA_OK) {
            /* Failed as expression, try as statement */
            lua_pop(L, 1);  /* pop error message */
            status = luaL_loadstring(L, buffer);
        }

        if (status == LUA_OK) {
            status = lua_pcall(L, 0, LUA_MULTRET, 0);
        }

        if (status != LUA_OK) {
            print_error(L);
        } else {
            /* Print results if any */
            int n = lua_gettop(L);
            if (n > 0) {
                for (int i = 1; i <= n; i++) {
                    if (lua_isnil(L, i)) {
                        printf("nil");
                    } else if (lua_isboolean(L, i)) {
                        printf("%s", lua_toboolean(L, i) ? "true" : "false");
                    } else if (lua_isnumber(L, i)) {
                        if (lua_isinteger(L, i)) {
                            printf("%lld", (long long)lua_tointeger(L, i));
                        } else {
                            printf("%g", lua_tonumber(L, i));
                        }
                    } else if (lua_isstring(L, i)) {
                        printf("%s", lua_tostring(L, i));
                    } else if (lua_istable(L, i)) {
                        /* Print table contents */
                        printf("{");
                        int first = 1;
                        lua_pushnil(L);
                        while (lua_next(L, i) != 0) {
                            if (!first) printf(", ");
                            first = 0;
                            /* Key at -2, value at -1 */
                            if (lua_isinteger(L, -2)) {
                                printf("[%lld]=", (long long)lua_tointeger(L, -2));
                            } else if (lua_isstring(L, -2)) {
                                printf("%s=", lua_tostring(L, -2));
                            }
                            if (lua_isnumber(L, -1)) {
                                if (lua_isinteger(L, -1)) {
                                    printf("%lld", (long long)lua_tointeger(L, -1));
                                } else {
                                    printf("%g", lua_tonumber(L, -1));
                                }
                            } else if (lua_isstring(L, -1)) {
                                printf("\"%s\"", lua_tostring(L, -1));
                            } else {
                                printf("%s", luaL_typename(L, -1));
                            }
                            lua_pop(L, 1);  /* pop value, keep key */
                        }
                        printf("}");
                    } else {
                        /* For other types, use tostring */
                        lua_getglobal(L, "tostring");
                        lua_pushvalue(L, i);
                        if (lua_pcall(L, 1, 1, 0) == LUA_OK) {
                            printf("%s", lua_tostring(L, -1));
                            lua_pop(L, 1);
                        } else {
                            printf("%s: %p", luaL_typename(L, i), lua_topointer(L, i));
                            lua_pop(L, 1);
                        }
                    }
                    if (i < n) printf("\t");
                }
                printf("\n");
                lua_settop(L, 0);  /* clear stack */
            }
        }
    }
}

int main(int argc, char **argv) {
    lua_State *L;

    /* Initialize Lua */
    L = luaL_newstate();
    if (!L) {
        fprintf(stderr, "Failed to initialize Lua\n");
        return 1;
    }

    /* Open standard libraries */
    luaL_openlibs(L);

    /* Register MIDI module */
    luaL_requiref(L, "midi", luaopen_midi, 1);
    lua_pop(L, 1);  /* remove module from stack */

    /* Process arguments */
    if (argc >= 2) {
        for (int i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-e") == 0) {
                /* Execute statement */
                if (i + 1 >= argc) {
                    fprintf(stderr, "Error: -e requires an expression\n");
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
                if (run_string(L, argv[++i]) != 0) {
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
            } else if (strcmp(argv[i], "--version") == 0) {
                printf("lua_midi using %s\n", LUA_VERSION);
            } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                print_usage(argv[0]);
            } else {
                /* Load and run file */
                if (run_file(L, argv[i]) != 0) {
                    lua_midi_cleanup();
                    lua_close(L);
                    return 1;
                }
            }
        }
    } else {
        /* Interactive REPL */
        repl(L);
    }

    /* Cleanup */
    lua_midi_cleanup();
    lua_close(L);

    return 0;
}
