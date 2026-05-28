# MAKEFILE FILE: Makefile
#
# Purpose   : Run seed7-mode tests.
# Created   : Friday, July 11 2025.
# Author    : Pierre Rouleau <prouleau001@gmail.com>

# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Controls byte and native compilation of all Emacs Lisp files used by the
# Seed7 support.  The build only succeeds when all compilations succeed with
# no error and no warning.

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
ELC_FILES := $(subst .el,.elc,$(EL _FILES))

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

# ----------------------------------------------------------------------------
