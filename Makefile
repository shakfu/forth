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

.PHONY: all build configure clean test test-quick test-verbose rebuild help \
		reset preludes build-debug build-mhs-standalone

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

build-mhs-standalone: $(PRELUDE_HEADERS)
	@$(CMAKE) -B $(BUILD_DIR) -DMHS_USE_PKG=ON -DMHS_USE_ZSTD=ON
	@$(CMAKE) --build $(BUILD_DIR) --target mhs-midi-standalone

clean:
	@rm -rf $(BUILD_DIR)

test: build
	@$(CTEST) --test-dir $(BUILD_DIR) --output-on-failure

test-quick: build
	@$(CTEST) --test-dir $(BUILD_DIR) -L quick --output-on-failure

test-verbose: build
	@$(CTEST) --test-dir $(BUILD_DIR) -V

rebuild: reset build test

reset:
	@rm -rf build
	@rm -rf thirdparty/MicroHs/bin
	@find . -type f -name ".mhscache" -delete

help:
	@echo "Targets:"
	@echo "  all          Build everything (default)"
	@echo "  build        Build all targets"
	@echo "  build-debug  Build all w/ debug and santizer modes"
	@echo "  configure    Run CMake configuration"
	@echo "  preludes     Generate C headers from prelude source files"
	@echo "  clean        Remove build directory"
	@echo "  test         Run all tests"
	@echo "  test-quick   Run quick tests only"
	@echo "  test-verbose Run tests with verbose output"
	@echo "  rebuild      Clean and build"
	@echo "  reset        Reset project to initial state"
	@echo ""
