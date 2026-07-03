# MAKEFILE FILE: GNU Make to build Seed7 support  -*- mode: makefile-gmake; -*-
#
# Purpose   : Run seed7-mode tests.
# Created   : Friday, July 11 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>

# ----------------------------------------------------------------------------
# Description
# -----------
#
# Controls byte and native compilation of all Emacs Lisp files used by the
# Seed7 support: seed7-mode.  The build only succeeds when all compilations
# succeed with no error and no warning.
#
# Used on GitHub CI build workflows to validate the code on multiple
# Emacs versions running on Linux, macOS and Windows.

# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - GNU Make, Emacs.

# ----------------------------------------------------------------------------
# Technical Details - Make syntax notes
# -------------------------------------
#
# Macros
# - all macro names are in uppercase
#   - Operator used:
#   -  =   Recursive assignment, re-evaluated on each use: if has other
#          expansions these are done on each expansion and may therefore
#          change if the content of a variable used in the expression changes.
#   -  :=  Single time assignment.  Fixed, never changes.
#   -  ?=  Set variable only if it does not already have a value.
#
# ----------------------------------------------------------------------------
# Portable makefile
.POSIX:

# -----------------------------------------------------------------------------
# Identify Program Names
# ----------------------
#
# - Emacs
# Allow overriding the Emacs binary at the Make command line
EMACS ?= emacs

# - Seed7 Compiler
# Allow overriding the Seed7 compiler binary at the Make command line.
S7C ?= s7c

# Note: the above macro allows the following use of make with
# other Emacs binaries:
#
#    make clean
#    make EMACS=emacs-26.1 pel test
#    make clean
#    make EMACS=emacs-24.3 pel test

# ----------------------------------------------------------------------------
# Detect OS platform
# ------------------
# On Windows $(OS) is always set to 'Windows_NT' (cmd.exe, PowerShell, Git Bash).
# On POSIX systems (Linux, macOS) $(OS) is unset or not 'Windows_NT'.

ifeq ($(OS),Windows_NT)
    ERT_TEST_CMD = powershell -NoProfile -ExecutionPolicy Bypass -File tools\ert-test.ps1
    ERT_TEST_DEP = tools/ert-test.ps1
else
    ERT_TEST_CMD = tools/ert-test
    ERT_TEST_DEP = tools/ert-test
endif

# ----------------------------------------------------------------------------
# Define Emacs capabilities - native compilation
# ----------------------------------------------

EMACS_NATIVE_COMP_AVAILABLE := $(shell $(EMACS) --batch --eval '(when \
                                                                  (and (fboundp (quote native-comp-available-p)) \
                                                                       (native-comp-available-p)) \
                                                                    (princ "yes"))')

# -----------------------------------------------------------------------------
# Identify Directories
# --------------------

# SRC_DIR   : where all .el file are stored
SRC_DIR := .

# DEST_ERT_TEST_DIR : the directory where ERT-based test source code files located.
DEST_ERT_TEST_DIR    := $(DEST_DIR)/tests/ert-tests

# -----------------------------------------------------------------------------
# Identify files
# --------------

# - Package file:
#
#   - Emacs Lisp package files
#     - source
EL_FILES := seed7-mode.el
#     - byte compiled
ELC_FILES := $(subst .el,.elc,$(EL_FILES))

# - Emacs ERT Test files:
#   - All test files are located inside the tests/ert-tests sub-directory
#     and have a name that matches: seed7-test-*.el.
#   - All ERT tests are performed by the tools/ert-test script.
#   - When a test passes, tools/ert-test creates a file that has the same name
#     as the file with the .test-passed suffix added to the file name.
#   - Those files are used as markers for make and prevent re-execution of
#     the tests that have already passed.

# ERT_TEST_EL_FILES := $(wildcard tests/ert-tests/seed7-test-*.el)
ERT_TEST_EL_FILES := tests/ert-tests/seed7-test-arrays-01.el \
                  tests/ert-tests/seed7-test-font-lock-01.el \
                  tests/ert-tests/seed7-test-font-lock-02.el \
                  tests/ert-tests/seed7-test-font-lock-02b.el \
                  tests/ert-tests/seed7-test-indent-01.el \
                  tests/ert-tests/seed7-test-indent-02.el \
                  tests/ert-tests/seed7-test-indent-03.el \
                  tests/ert-tests/seed7-test-mark-defun-01.el \
                  tests/ert-tests/seed7-test-nav-array-01.el \
                  tests/ert-tests/seed7-test-nav-final-pos-01.el \
                  tests/ert-tests/seed7-test-nav-nested-01.el \
                  tests/ert-tests/seed7-test-syntax-propertize-01.el

ALL_ERT_TEST_PASSED := $(ERT_TEST_EL_FILES:.el=.el.test-passed)

# ----------------------------------------------------------------------------
# Building Emacs Lisp Tools
# -------------------------
#
# The repository provide a set of tools to help development of the seed7-mode
# package.
#
# Two files live under tools/ and have their own dependency chain:
#
#   seed7-mode.elc
#       └── tools/seed7-mode-time.elc
#               └── tools/seed7-indent-bench.elc
#
# Both tools/ files need the root directory on the load-path (-L .) so they
# can (require 'seed7-mode), and the tools/ directory itself (-L tools) so
# they can (require 'seed7-mode-time).

TOOLS_EL_FILES  := tools/seed7-fopen-controlled.el \
                   tools/seed7-fopen-time.el \
                   tools/seed7-indent-bench.el \
                   tools/seed7-mode-time.el \
                   tools/sd7-indent-perf.el \
                   tools/sd7-nav-index.el

TOOLS_ELC_FILES := $(subst .el,.elc,$(TOOLS_EL_FILES))

# ----------------------------------------------------------------------------
# FIRST RULE - all: build all files; seed7-mode and all tools.
# ============================================================
# - On the GitHub CI/CD, Seed7 is not installed.
#   The GitHub CI/CD is identified by the presence of the GITHUB_WORKSPACE
#   environment variable is defined.

# CAUTION:  DO NOT write any other rule above this section!
#           The rule for 'all' must be the first in the file
#           so that 'make' is equivalent to 'make all'.
#
#           This means that all dependencies MUST be written AFTER this section!

.PHONY:	all

ifeq ($(GITHUB_WORKSPACE),)
# On user's systems
all: main tools tools/s7xref test
else
# On GitHub CI systems
all: main tools test
endif

# ----------------------------------------------------------------------------
# RULE - Default rule: compile seed7-mode
# ----------------------------------------
#
# byte compile and native compile if supported by Emacs.

.PHONY:	main
main: seed7-mode.elc

# ----------------------------------------------------------------------------
# RULE - tools: compile development tools
# ------------------------------------------------

.PHONY: tools
tools: $(TOOLS_ELC_FILES)


.PHONY:	s7tools
s7tools: tools/s7xref

# ----------------------------------------------------------------------------
# RULE - clean: remove all generated byte-code and native-code files.
# --------------------------------------------------------------------

.PHONY: clean
clean:  clean-test
	-rm -f $(ELC_FILES)
	-rm -f $(TOOLS_ELC_FILES)
	-rm -f tools/s7xref
	@# Remove native-compiled .eln files if any exist.
	@find . -name '*.eln' -delete 2>/dev/null || true

# -----------------------------------------------------------------------------
# RULE - help: Self-descriptive rule
# ----------------------------------

.PHONY: help
help:
	@printf "\nBuild seed7-mode.\n"
	@printf "Usage:\n"
	@printf " * make              - Same as 'make all'.\n"
	@printf " * make all          - Same as 'make main tools test'.\n"
	@printf " * make main         - Byte/native compile seed7-mode.el.\n"
	@printf " * make tools        - Byte/native compile tools programs.\n"
	@printf " * make test         - Compile and run ERT-based tests.\n"
	@printf " * make clean        - Remove all generated files, including test passed tags.\n"
	@printf " * make clean-test   - Remove test passed tags to force test next time.\n"
	@printf " * make check-seed7  - Check availabily of Seed7 compiler.\n"

# ----------------------------------------------------------------------------
# RULE - how to compile the Emacs-Lisp source code files - package files
# -----------------------------------------------------------------------
#
# Byte compile the file and native compile it if Emacs supports it.

# Single .el file byte-compile to .elc rule
.SUFFIXES: .el .elc

ifeq ($(EMACS_NATIVE_COMP_AVAILABLE), yes)
.el.elc:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-native-compile $<
else
.el.elc:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<
endif

# ----------------------------------------------------------------------------
# Tool Code dependencies
# ----------------------

# See TOOLS_EL_FILES

tools/seed7-fopen-controlled.elc: seed7-mode.elc tools/seed7-fopen-time.elc
tools/seed7-fopen-time.elc:       seed7-mode.elc
tools/seed7-indent-bench.elc:     seed7-mode.elc tools/seed7-fopen-time.elc
tools/seed7-mode-time.elc:        seed7-mode.elc
tools/sd7-nav-index.elc:          seed7-mode.elc
tools/sd7-indent-perf.elc:        seed7-mode.elc

# ----------------------------------------------------------------------------
# Test Code dependencies
# ----------------------

# See ERT_TEST_EL_FILES

tests/ert-tests/seed7-test-arrays-01.el.test-passed:             seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-arrays-01.elc
tests/ert-tests/seed7-test-font-lock-01.el.test-passed:          seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-font-lock-01.elc
tests/ert-tests/seed7-test-font-lock-02.el.test-passed:          seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-font-lock-02.elc
tests/ert-tests/seed7-test-font-lock-02b.el.test-passed:         seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-font-lock-02b.elc
tests/ert-tests/seed7-test-indent-01.el.test-passed:             seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-indent-01.elc
tests/ert-tests/seed7-test-indent-02.el.test-passed:             seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-indent-02.elc
tests/ert-tests/seed7-test-indent-03.el.test-passed:             seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-indent-03.elc
tests/ert-tests/seed7-test-mark-defun-01.el.test-passed:         seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-mark-defun-01.el
tests/ert-tests/seed7-test-nav-array-01.el.test-passed:          seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-nav-array-01.elc
tests/ert-tests/seed7-test-nav-final-pos-01.el.test-passed:      seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-nav-final-pos-01.elc
tests/ert-tests/seed7-test-nav-nested-01.el.test-passed:         seed7-mode.elc tests/ert-tests/pel-ert.elc tests/ert-tests/seed7-test-nav-nested-01.elc
tests/ert-tests/seed7-test-syntax-propertize-01.el.test-passed:  seed7-mode.elc tests/ert-tests/pel-ert.elc  tests/ert-tests/seed7-test-syntax-propertize-01.el
#tests/ert-tests/seed7-test-sets-01.el.test-passed:          seed7-mode.elc tests/ert-tests/seed7-test-sets-01.elc

# ----------------------------------------------------------------------------
# RULE - execute ERT tests
# ------------------------

#  Pattern Rule: How to create a .el.test-passed file from a .el file
tests/ert-tests/seed7-test-%.el.test-passed: tests/ert-tests/seed7-test-%.el $(ERT_TEST_DEP)
	$(ERT_TEST_CMD) $<

.PHONY:	test clean-test

test:	$(ALL_ERT_TEST_PASSED)

# The rm -f option prevents complaints from rm when the file is not present.
clean-test:
	-rm -f tests/ert-tests/*.test-passed

# ----------------------------------------------------------------------------
# RULE - check if Seed7 is installed by checking if s7c is accessible
# -------------------------------------------------------------------

.PHONY: check-seed7

ifeq ($(OS),Windows_NT)
check-seed7:
	@where $(S7C) >nul 2>nul || ( \
		echo Error: $(S7C) utility not found in PATH. && \
		echo The seed7-mode cross reference uses Seed7 && \
		echo - Please install Seed7 or update your PATH. && \
		echo - See https://seed7.net/ and https://github.com/ThomasMertes/seed7 && \
		exit 1 \
	)
	@echo $(S7C) is accessible.
else
check-seed7:
	@command -v $(S7C) >/dev/null 2>&1 || { \
		echo "Error: $(S7C) utility not found in PATH."; \
		echo "The seed7-mode cross reference uses Seed7."; \
		echo "  Please install Seed7 or update your PATH."; \
		echo "  See https://seed7.net/ and https://github.com/ThomasMertes/seed7"; \
		exit 1; \
	}
	@echo "$(S7C) is accessible."
endif

# ----------------------------------------------------------------------------
# RULE - How to compile Seed7 file
# --------------------------------

tools/s7xref: tools/s7xref.sd7
	$(S7C) $<

# ----------------------------------------------------------------------------
# Indentation benchmark targets
#
# Run the re-indentation timing benchmark on every .sd7 and .s7i file found
# inside SEED7_BENCH_DIR.
#
# SEED7_BENCH_DIR defaults to the standard seed7 library directory but can be
# overridden on the command line:
#
#   make bench                                  # use the default directory
#   make bench SEED7_BENCH_DIR=/path/to/files   # use a custom directory
#
# For each file the benchmark prints one timing-report line:
#
#   seed7-calc-indent: N calls | total=Xs | mean=Xms | min=Xms | max=Xms
#
# The file is re-saved after benchmarking; a diff of the file before and
# after the run is a good correctness check (content should be identical).
#
# Targets:
#   bench-check   verify SEED7_BENCH_DIR contains at least one matching file
#   bench         run the benchmark on all .sd7 and .s7i files

# Directory that contains the .sd7 / .s7i files to benchmark.
# Typical locations:
#   - /usr/local/lib/seed7      (system-wide installation)
#   - $(HOME)/seed7/lib         (per-user installation)


# Indentation benchmark targets
#
# Run the re-indentation timing benchmark on every .sd7 and .s7i file found
# inside SEED7_BENCH_DIR.
#
# SEED7_BENCH_DIR defaults to the standard seed7 library directory but can be
# overridden on the command line:
#
#   make bench                                  # use the default directory
#   make bench SEED7_BENCH_DIR=/path/to/files   # use a custom directory
#
# Targets:
#   bench-check   verify SEED7_BENCH_DIR contains at least one matching file
#   bench         run the benchmark on all .sd7 and .s7i files

# Directory that contains the .sd7 / .s7i files to benchmark.
SEED7_BENCH_DIR ?= /usr/local/lib/seed7

.PHONY: bench bench-check

bench-check:
	@files=$$(ls $(SEED7_BENCH_DIR)/*.sd7 $(SEED7_BENCH_DIR)/*.s7i 2>/dev/null); \
	 if [ -z "$$files" ]; then \
	     echo "ERROR: no .sd7 or .s7i files found in: $(SEED7_BENCH_DIR)"; \
	     echo "       Override with: make bench SEED7_BENCH_DIR=/your/path"; \
	     exit 1; \
	 fi

bench: bench-check
	$(EMACS) -Q --batch \
	    -L . \
	    -l seed7-mode.el \
	    -l tools/seed7-mode-time.el \
	    -l tools/seed7-indent-bench.el \
	    -- $(SEED7_BENCH_DIR)/*.sd7 $(SEED7_BENCH_DIR)/*.s7i

# ----------------------------------------------------------------------------
