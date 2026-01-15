# Makefile frontend for CMake build

BUILD_DIR := build
CMAKE := cmake
CTEST := ctest
PRELUDE2C := ./scripts/prelude2c.py

# Prelude source files and their generated headers
PRELUDE_SCM := projects/s7-midi/prelude.scm
PRELUDE_LUA := projects/lua-midi/prelude.lua
PRELUDE_PY  := projects/pktpy-midi/prelude.py

HEADER_SCM := projects/s7-midi/scm_prelude.h
HEADER_LUA := projects/lua-midi/lua_prelude.h
HEADER_PY  := projects/pktpy-midi/py_prelude.h

PRELUDE_HEADERS := $(HEADER_SCM) $(HEADER_LUA) $(HEADER_PY)

.PHONY: all build configure clean test test-quick test-verbose \
		rebuild ctidy help reset preludes build-debug \
		alda-midi forth-midi joy-midi lua-midi pktpy-midi s7-midi \
		mhs-midi mhs-midi-all \
		mhs-midi-src mhs-midi-src-zstd \
		mhs-midi-pkg mhs-midi-pkg-zstd \
		test-joy test-joy-passing test-joy-failing test-joy-verbose 

all: build

# Generate prelude headers from source files
$(HEADER_SCM): $(PRELUDE_SCM) $(PRELUDE2C)
	@$(PRELUDE2C) $<

$(HEADER_LUA): $(PRELUDE_LUA) $(PRELUDE2C)
	@$(PRELUDE2C) $<

$(HEADER_PY): $(PRELUDE_PY) $(PRELUDE2C)
	@$(PRELUDE2C) $<

preludes: $(PRELUDE_HEADERS)
	@echo "Generated prelude headers"

# CMake handles MicroHs build automatically during configuration
configure: $(PRELUDE_HEADERS)
	@$(CMAKE) -B $(BUILD_DIR)

build: configure
	@$(CMAKE) --build $(BUILD_DIR) -j4

build-debug: $(PRELUDE_HEADERS)
	@$(CMAKE) -DENABLE_SANITIZERS=ON -DCMAKE_BUILD_TYPE=Debug -B $(BUILD_DIR)
	@$(CMAKE) --build $(BUILD_DIR)

# Individual language targets
alda-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target alda_midi

forth-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target forth_midi

joy-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target joy_midi

lua-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target lua_midi

pktpy-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target pktpy_midi

s7-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target s7_midi

# mhs-midi variants
mhs-midi: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi

mhs-midi-src: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-src

mhs-midi-src-zstd: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-src-zstd

mhs-midi-pkg: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-pkg

mhs-midi-pkg-zstd: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-pkg-zstd

mhs-midi-all: configure
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-all

clean:
	@rm -rf $(BUILD_DIR)

test: build
	@$(CTEST) --test-dir $(BUILD_DIR) --output-on-failure

test-quick: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L quick --output-on-failure

test-verbose: build
	@$(CTEST) --test-dir $(BUILD_DIR) -V

# Joy unit tests (from pyjoy-lang test suite)
test-joy: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L joy_unit --output-on-failure

test-joy-passing: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L joy_unit -j8 2>&1 | grep -E "Passed|passed"

test-joy-failing: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L joy_unit --rerun-failed --output-on-failure

test-joy-verbose: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L joy_unit -V

rebuild: reset build test

ctidy:
	python3 scripts/ctidy.py

reset:
	@rm -rf build
	@rm -rf thirdparty/MicroHs/bin
	@find . -type f -name ".mhscache" -delete

help:
	@echo "Targets:"
	@echo "  all              Build everything (default)"
	@echo "  build            Build all targets"
	@echo "  build-debug      Build with debug and sanitizer modes"
	@echo "  configure        Run CMake configuration"
	@echo "  preludes         Generate C headers from prelude source files"
	@echo "  clean            Remove build directory"
	@echo "  test             Run all tests"
	@echo "  test-quick       Run quick tests only"
	@echo "  test-verbose     Run tests with verbose output"
	@echo "  test-joy         Run Joy unit tests (pyjoy-lang suite)"
	@echo "  test-joy-passing Show only passing Joy tests"
	@echo "  test-joy-failing Re-run only failed Joy tests"
	@echo "  rebuild          Clean and build"
	@echo "  ctidy            Run clang-tidy"
	@echo "  reset            Reset project to initial state"
	@echo ""
	@echo "Language targets:"
	@echo "  alda-midi        Build Alda interpreter"
	@echo "  forth-midi       Build Forth interpreter"
	@echo "  joy-midi         Build Joy interpreter"
	@echo "  lua-midi         Build Lua interpreter"
	@echo "  pktpy-midi       Build PocketPy interpreter"
	@echo "  s7-midi          Build s7 Scheme interpreter"
	@echo ""
	@echo "mhs-midi variants:"
	@echo "  mhs-midi         Non-standalone (requires MHSDIR)"
	@echo "  mhs-midi-src     Source embedding (~3.3MB, ~20s cold start)"
	@echo "  mhs-midi-src-zstd  Compressed source (~1.3MB, smallest)"
	@echo "  mhs-midi-pkg     Package embedding (~4.8MB, ~1s cold start)"
	@echo "  mhs-midi-pkg-zstd  Compressed packages (~3MB, recommended)"
	@echo "  mhs-midi-all     Build all mhs-midi variants"
	@echo ""
