;;; tools/run-sd7-benchmark.el --- GC-controlled Seed7 benchmark runner  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 12:06:45 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7-MODE package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2026  Pierre Rouleau
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Provides a GC-controlled Seed7 file-open benchmark that can be used:
;;
;;  1. Interactively, from inside an Emacs session:
;;       M-x sd7-bench-run
;;     Prompts for each argument and remembers the last-used values as
;;     defaults for the next invocation.
;;
;;  2. From the command line (batch mode):
;;       emacs -Q --batch --load tools/run-sd7-benchmark.el \
;;             -- SEED7_DIR X Y [ITERATIONS]
;;
;;  3. From a Makefile:
;;       $(EMACS) -Q --batch --load tools/run-sd7-benchmark.el \
;;                -- $(SEED7_DIR) $(X) $(Y) $(ITERATIONS)
;;
;; Arguments (interactive and CLI):
;;   SEED7_DIR  : path to the Seed7 source tree, must contain prg/ and lib/
;;   X          : major report number  (written as %02d, e.g. 3  → "03")
;;   Y          : minor report number  (e.g. 2)
;;   ITERATIONS : timed opens per file (default: 5)
;;
;; Output file (relative to the seed7-mode repository root):
;;   reports/file-open-benchmark-<XX>.<Y>.rst
;;
;; Batch-mode note:
;;   In --batch mode `sit-for 0' is a no-op, so this file overrides
;;   `sd7-controlled--open-file' to call `font-lock-ensure' instead.
;;   This forces synchronous font-lock in batch, giving timings equivalent
;;   to an interactive session.

;;; --------------------------------------------------------------------------
;;; Dependencies / load-path bootstrap
;;
;; This block must run both at load time and at byte-compile time so that
;; the required libraries are on load-path in all contexts:
;;   - `load-file-name'            set when loaded via `load'/`require'
;;   - `byte-compile-current-file' set by the byte compiler
;;   - `buffer-file-name'          fallback for an interactively visited buffer
;;
;; The outer `when this-file' guard makes the block a safe no-op if all
;; three are nil (defensive; should not happen in practice).

(eval-and-compile
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (let* ((tools-dir (file-name-directory (expand-file-name this-file)))
             (root-dir  (expand-file-name ".." tools-dir)))
        (dolist (dir (list root-dir tools-dir))
          (unless (member dir load-path)
            (push dir load-path)))))))

(require 'seed7-mode)
(require 'seed7-fopen-time)        ; provides `benchmark-sd7-files-in-dir'
(require 'seed7-fopen-controlled)  ; provides `generate-sd7-controlled-report'

;;; --------------------------------------------------------------------------
;;; Repository root (derived once from this file's location)

(defvar sd7-bench--repo-root
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (expand-file-name
       ".."
       (file-name-directory (expand-file-name this-file)))))
  "Absolute path to the seed7-mode repository root.
Derived from the location of this file at load or compile time.")

;;; --------------------------------------------------------------------------
;;; Remembered defaults (persist across invocations within an Emacs session)

(defvar sd7-bench--last-seed7-dir
  (expand-file-name "~/src/seed7")
  "Last Seed7 source directory used.  Offered as default in `sd7-bench-run'.")

(defvar sd7-bench--last-major 3
  "Last major report number used.  Offered as default in `sd7-bench-run'.")

(defvar sd7-bench--last-minor 0
  "Last minor report number used.  Offered as default in `sd7-bench-run'.")

(defvar sd7-bench--last-iterations 5
  "Last iteration count used.  Offered as default in `sd7-bench-run'.")

;;; --------------------------------------------------------------------------
;;; Help

(defun sd7-bench-help ()
  "Display usage information for the Seed7 GC-controlled benchmark runner."
  (interactive)
  (message "\
sd7-bench-run — GC-Controlled Seed7 File-Open Benchmark
========================================================

Interactive (inside Emacs):

  M-x sd7-bench-run
  Prompts for SEED7_DIR, X, Y, and ITERATIONS.
  Previous values are offered as defaults.

Command-line (batch):

  emacs -Q --batch --load tools/run-sd7-benchmark.el \\
        -- SEED7_DIR X Y [ITERATIONS]

  SEED7_DIR   root of the Seed7 source tree (must contain prg/ and lib/)
  X           major report number (zero-padded to 2 digits in filename)
  Y           minor report number
  ITERATIONS  timed opens per file (default: 5)

Makefile:

  $(EMACS) -Q --batch --load tools/run-sd7-benchmark.el \\
           -- $(SEED7_DIR) $(X) $(Y) $(ITERATIONS)

Output (relative to the seed7-mode repository root):

  reports/file-open-benchmark-<XX>.<Y>.rst"))

;;; --------------------------------------------------------------------------
;;; Batch-mode font-lock patch
;;
;; In --batch mode, `sit-for 0' is a no-op (no display event loop).
;; Override `sd7-controlled--open-file' so that font-lock is activated
;; synchronously via `font-lock-ensure', which forces a full-buffer
;; fontification pass and makes timings comparable to an interactive session.
;; The override is applied only when running non-interactively.

(defun sd7-bench--open-file-batch (file-name)
  "Batch-safe replacement for `sd7-controlled--open-file'.
Opens FILE-NAME, forces synchronous font-lock via `font-lock-ensure',
kills the buffer (unless it was already open), and returns the line count."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf          (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (unless font-lock-mode
        (font-lock-mode 1))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure))
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf
      (kill-buffer buf))
    line-count))

;;; --------------------------------------------------------------------------
;;; Internal helpers

(defun sd7-bench--validate-seed7-dir (seed7-dir)
  "Signal `user-error' if SEED7-DIR is not a valid Seed7 source tree."
  (unless (file-directory-p seed7-dir)
    (user-error "Not a directory: %s" seed7-dir))
  (dolist (sub '("prg" "lib"))
    (unless (file-directory-p (expand-file-name sub seed7-dir))
      (user-error "Expected sub-directory '%s/' not found under: %s"
                  sub seed7-dir))))

(defun sd7-bench--output-file (major minor)
  "Return the absolute path for the report file numbered MAJOR.MINOR.
Signals `user-error' if `sd7-bench--repo-root' is nil."
  (unless sd7-bench--repo-root
    (user-error
     "Cannot determine repository root (sd7-bench--repo-root is nil); \
load this file from a saved file path"))
  (expand-file-name
   (format "reports/file-open-benchmark-%02d.%d.rst" major minor)
   sd7-bench--repo-root))

;;; --------------------------------------------------------------------------
;;; Core runner (called by both the interactive command and the CLI entry point)

(defun sd7-bench-run-core (seed7-dir major minor iterations)
  "Run the GC-controlled Seed7 file-open benchmark and write the RST report.

SEED7-DIR   : path to the Seed7 source tree (must contain prg/ and lib/).
MAJOR       : major report number (integer; padded to %02d in filename).
MINOR       : minor report number (integer).
ITERATIONS  : number of timed opens per file (positive integer).

The report is written to:
  reports/file-open-benchmark-<MM>.<N>.rst
relative to the seed7-mode repository root.

Returns the absolute path of the written report file."
  ;; ── Validate arguments ────────────────────────────────────────────────────
  (setq seed7-dir (expand-file-name seed7-dir))
  (sd7-bench--validate-seed7-dir seed7-dir)
  (unless (integerp major)
    (user-error "MAJOR must be an integer, got: %S" major))
  (unless (integerp minor)
    (user-error "MINOR must be an integer, got: %S" minor))
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "ITERATIONS must be a positive integer, got: %S" iterations))
  ;; ── In batch mode, patch sd7-controlled--open-file ───────────────────────
  (when noninteractive
    (fset 'sd7-controlled--open-file #'sd7-bench--open-file-batch))
  ;; ── Build directory specs and output path ────────────────────────────────
  (let* ((dir-specs (list (list (expand-file-name "prg" seed7-dir) '("sd7"))
                          (list (expand-file-name "lib" seed7-dir) '("s7i"))))
         (out-file  (sd7-bench--output-file major minor)))
    (message "")
    (message "=== GC-Controlled Seed7 File-Open Benchmark ===")
    (message "seed7-mode : %s" seed7-mode-version-timestamp)
    (message "Seed7 dir  : %s" seed7-dir)
    (message "Iterations : %d per file" iterations)
    (message "Output     : %s" out-file)
    (message "")
    ;; ── Run the benchmark (results land in *sd7-controlled-benchmark*) ──────
    (generate-sd7-controlled-report dir-specs iterations)
    ;; ── Write the report buffer to the RST output file ───────────────────────
    (let ((buf (get-buffer "*sd7-controlled-benchmark*")))
      (unless buf
        (user-error
         "Buffer *sd7-controlled-benchmark* not found after benchmark run"))
      (with-current-buffer buf
        ;; Append the standard RST footer separator used in all reports
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert
         "\n.. ---------------------------------------------------------------------------\n")
        ;; Create the reports/ directory if needed
        (make-directory (file-name-directory out-file) :parents)
        ;; Write UTF-8, Unix line endings
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) out-file nil :silent))))
    (message "")
    (message "Report written to: %s" out-file)
    out-file))

;;; --------------------------------------------------------------------------
;;; Interactive command

;;;###autoload
(defun sd7-bench-run (seed7-dir major minor iterations)
  "Run the GC-controlled Seed7 file-open benchmark interactively.

Prompts for SEED7-DIR, MAJOR report number, MINOR report number, and
ITERATIONS.  Previous values are offered as defaults and remembered for
the next call within the same Emacs session.

The report is written to:
  reports/file-open-benchmark-<MM>.<N>.rst
relative to the seed7-mode repository root, and the buffer
*sd7-controlled-benchmark* is displayed."
  (interactive
   (list
    (read-directory-name
     "Seed7 source directory: "
     sd7-bench--last-seed7-dir   ; initial directory for the prompt
     nil                          ; default (nil → use DIR)
     nil)                         ; mustmatch: validate in core, not here
    (read-number "Major report number (X): " sd7-bench--last-major)
    (read-number "Minor report number (Y): " sd7-bench--last-minor)
    (read-number "Iterations per file: "     sd7-bench--last-iterations)))
  ;; ── Persist values for the next invocation ───────────────────────────────
  (setq sd7-bench--last-seed7-dir  (directory-file-name (expand-file-name seed7-dir))
        sd7-bench--last-major      major
        sd7-bench--last-minor      minor
        sd7-bench--last-iterations iterations)
  (sd7-bench-run-core seed7-dir major minor iterations))

;;; --------------------------------------------------------------------------
;;; CLI entry point

(defun sd7-bench--cli-help-and-exit (exit-code)
  "Print command-line usage to *Messages* and exit Emacs with EXIT-CODE."
  (message "\
Usage: emacs -Q --batch --load tools/run-sd7-benchmark.el \
-- SEED7_DIR X Y [ITERATIONS]

  SEED7_DIR   path to the Seed7 source tree (must contain prg/ and lib/)
  X           major report number  (e.g. 3)
  Y           minor report number  (e.g. 2)
  ITERATIONS  timed opens per file (default: 5)

Output: reports/file-open-benchmark-<XX>.<Y>.rst
        (relative to the seed7-mode repository root)

Example:
  emacs -Q --batch --load tools/run-sd7-benchmark.el -- ~/src/seed7 3 2 5")
  (kill-emacs exit-code))

(defun sd7-bench-main ()
  "Parse `command-line-args-left' and call `sd7-bench-run-core'.
This is the CLI / Makefile entry point.  Exits Emacs with 0 on success
or 1 on any error."
  (let ((args command-line-args-left))
    ;; Consume all remaining args so Emacs does not warn about unknown options.
    (setq command-line-args-left nil)
    (when (< (length args) 3)
      (sd7-bench--cli-help-and-exit 1))
    (let* ((seed7-dir  (nth 0 args))
           (major-str  (nth 1 args))
           (minor-str  (nth 2 args))
           (iters-str  (and (> (length args) 3) (nth 3 args)))
           (major      (string-to-number major-str))
           (minor      (string-to-number minor-str))
           (iterations (if iters-str (string-to-number iters-str) 5)))
      ;; Validate numeric arguments before calling the core runner.
      (when (and iters-str (<= iterations 0))
        (message "ERROR: ITERATIONS must be a positive integer, got: %s" iters-str)
        (kill-emacs 1))
      (condition-case err
          (progn
            (sd7-bench-run-core seed7-dir major minor iterations)
            (kill-emacs 0))
        (user-error
         (message "ERROR: %s" (cadr err))
         (kill-emacs 1))))))

;;; --------------------------------------------------------------------------
;;; CLI auto-dispatch
;;
;; `noninteractive' is t when Emacs is started with --batch (or --script).
;; In that mode no display event loop exists; the font-lock patch in
;; `sd7-bench-run-core' compensates for the missing `sit-for 0'.
;;
;; When this file is loaded from within an interactive Emacs session
;; (M-x load-file, require, or byte-compile-file) `noninteractive' is nil,
;; so `sd7-bench-main' is NOT called automatically; use M-x sd7-bench-run
;; instead.

(when noninteractive
  (sd7-bench-main))

;;; --------------------------------------------------------------------------
(provide 'run-sd7-benchmark)

;;; run-sd7-benchmark.el ends here
