# MAKEFILE FILE: Makefile
#
# Purpose   : Build and test seed7-mode.
# Created   : Friday, July 11 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>

# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Controls byte and native compilation of all Emacs Lisp files used by the
# Seed7 support.  The build only succeeds when all compilations succeed with
# no error and no warning.
#
# ERT test targets:
#   make test          - run all ERT test suites
#   make test-font-lock     - run font-lock tests only
#   make test-navigation    - run navigation tests only
#   make test-comment-style - run comment style tests only

# ----------------------------------------------------------------------------
# Dependencies
# ------------
#
# - GNU Make, Emacs.


# ----------------------------------------------------------------------------
# Code
# ----
#
#

# ----------------------------------------------------------------------------
# Portable makefile
.POSIX:

# -----------------------------------------------------------------------------
# allow overriding the Emacs binary at the command line
EMACS ?= emacs

# Note: the above macro allows the following use of make with
# other Emacs binaries:
#
#    make clean
#    make EMACS=emacs-26.1 pel test
#    make clean
#    make EMACS=emacs-24.3 pel test

# ----------------------------------------------------------------------------
# Define abilities of Emacs - native compilation.

EMACS_NATIVE_COMP_AVAILABLE := $(shell $(EMACS) --batch --eval '(when \
                                                                  (and (fboundp (quote native-comp-available-p)) \
                                                                       (native-comp-available-p)) \
                                                                    (princ "yes"))')

# -----------------------------------------------------------------------------
# Identify the files used in the package.

EL_FILES := seed7-mode.el

# ELC_FILES used for this project.
ELC_FILES := $(subst .el,.elc,$(EL_FILES))

# ERT test files under tests/erl-tests/ that use only standard ERT (no external deps).
# Note: tests/erl-tests/seed7-test-arrays-01.el requires the external 'pel-ert'
#       library (https://github.com/pierre-rouleau/pel) and is not run here.
ERT_TEST_FILES := tests/erl-tests/seed7-test-font-lock-01.el \
                  tests/erl-tests/seed7-test-navigation-01.el \
                  tests/erl-tests/seed7-test-comment-style-01.el

# ----------------------------------------------------------------------------
# RULES:  to byte-compile the Emacs-Lisp source code files

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
# Targets

all: $(ELC_FILES)

# Run all ERT test suites.
# The -L . flag ensures seed7-mode.el is found.
# The -L tests/erl-tests flag ensures the test files can require each other.
# ert-run-tests-batch-and-exit exits with a non-zero code when any test fails.
test: $(ELC_FILES)
	$(EMACS) -Q --batch \
	    -L . \
	    -L tests/erl-tests \
	    $(foreach f,$(ERT_TEST_FILES),-l $(f)) \
	    -f ert-run-tests-batch-and-exit

# Individual test targets for convenience.
test-font-lock: $(ELC_FILES)
	$(EMACS) -Q --batch \
	    -L . \
	    -l tests/erl-tests/seed7-test-font-lock-01.el \
	    -f ert-run-tests-batch-and-exit

test-navigation: $(ELC_FILES)
	$(EMACS) -Q --batch \
	    -L . \
	    -l tests/erl-tests/seed7-test-navigation-01.el \
	    -f ert-run-tests-batch-and-exit

test-comment-style: $(ELC_FILES)
	$(EMACS) -Q --batch \
	    -L . \
	    -l tests/erl-tests/seed7-test-comment-style-01.el \
	    -f ert-run-tests-batch-and-exit

# ----------------------------------------------------------------------------
