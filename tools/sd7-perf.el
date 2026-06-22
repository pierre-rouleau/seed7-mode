;;; tools/sd7-perf.el --- Four-mode Seed7 mode performance benchmark  -*- lexical-binding: t; -*-

;; Created   : Sunday, June 22 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-22 11:01:37 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7 package.
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
;; Runs the GC-controlled Seed7 file-open benchmark in four measurement modes
;; and writes a single RST report with one table section per mode.
;;
;; Modes
;; -----
;;
;; Mode A — Major-mode activation only  (always available)
;;   `find-file-noselect' activates the major mode: syntax-table setup and
;;   font-lock keyword pattern compilation.  No window is created, so
;;   jit-lock never fires.  This is the direct measure of seed7-mode overhead.
;;
;; Mode B — Mode activation + initial visible jit-lock pass  (requires -nw frame)
;;   The buffer is displayed in a window, then `sit-for 0' lets jit-lock
;;   fontify the visible region (≈ terminal window height in -nw mode, typically
;;   24–50 lines).  Measures what a user pays on a plain `find-file'.
;;
;; Mode C — Mode activation + full-buffer fontification  (always available)
;;   `font-lock-ensure' forces the regexp engine over the ENTIRE buffer.
;;   This is a stress-test / worst-case measurement; useful for detecting
;;   catastrophic-backtracking regressions in font-lock patterns.
;;
;; Mode D — Mode activation + full incremental jit-lock  (requires -nw frame)
;;   The buffer is displayed and scrolled from top to bottom; `sit-for 0'
;;   after each screen lets jit-lock fontify incrementally.  Measures the
;;   cost a user pays when reading an entire file.
;;
;; Usage
;; -----
;;
;; 1. Interactive — preferred for all 4 modes (emacs -Q -nw):
;;
;;      M-x sd7-perf-run
;;
;;    Prompts for SEED7_DIR, REPORT_DIR, ID, and ITERATIONS.
;;    Previous values are offered as defaults.
;;
;; 2. Command-line — all 4 modes (requires -nw frame):
;;
;;      emacs -Q -nw --load tools/sd7-perf.el --eval "(sd7-perf-main)" \
;;            -- SEED7_DIR REPORT_DIR ID [ITERATIONS]
;;
;; 3. Command-line — batch mode (modes A and C only):
;;
;;      emacs -Q --batch --load tools/sd7-perf.el \
;;            -- SEED7_DIR REPORT_DIR ID [ITERATIONS]
;;
;; 4. Makefile:
;;
;;      # Full 4-mode run:
;;      $(EMACS) -Q -nw --load tools/sd7-perf.el --eval "(sd7-perf-main)" \
;;               -- $(SEED7_DIR) $(REPORT_DIR) $(ID) $(ITERATIONS)
;;
;;      # Batch (A + C only):
;;      $(EMACS) -Q --batch --load tools/sd7-perf.el \
;;               -- $(SEED7_DIR) $(REPORT_DIR) $(ID) $(ITERATIONS)
;;
;; Arguments:
;;   SEED7_DIR  : path to the Seed7 source tree (must contain prg/ and lib/)
;;   REPORT_DIR : directory where the report file is written (created if absent)
;;   ID         : free-form report identifier (e.g. "04.0", "xref-opt")
;;   ITERATIONS : timed opens per file (positive integer; default: 5)
;;
;; Output:
;;   REPORT_DIR/seed7mode-perf-ID.rst

;;; --------------------------------------------------------------------------
;;; Load-path bootstrap
;;
;; Runs at both load time and byte-compile time.
;; The `when this-file' guard is a safe no-op when all three sources are nil.

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
(require 'seed7-fopen-controlled)  ; `sd7-controlled--warmup',
                                   ; `sd7-controlled--timed-pass',
                                   ; `sd7-controlled--open-file'
(require 'cl-lib)                  ; `cl-caddr'


;;; --------------------------------------------------------------------------
;;; Repository root (derived once from this file's load-time location)

(defvar sd7-perf--repo-root
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
Derived from the location of this file at load or byte-compile time.")

;;; --------------------------------------------------------------------------
;;; Remembered defaults (persist across invocations within a session)

(defvar sd7-perf--last-seed7-dir  (expand-file-name "~/src/seed7")
  "Last Seed7 source directory used.  Offered as default in `sd7-perf-run'.")

(defvar sd7-perf--last-report-dir
  (when sd7-perf--repo-root
    (expand-file-name "reports" sd7-perf--repo-root))
  "Last report output directory.  Defaults to reports/ in the repo root.
Offered as default by `sd7-perf-run'.")

(defvar sd7-perf--last-id "01"
  "Last report ID used.  Offered as default in `sd7-perf-run'.")

(defvar sd7-perf--last-iterations 5
  "Last iteration count used.  Offered as default in `sd7-perf-run'.")

(defvar sd7-perf--last-modes "ABCD"
  "Last mode set used.  Offered as default in `sd7-perf-run'.
A string of mode letters (any subset of \"ABCD\") specifying which
measurement modes to execute.  Order is ignored; modes always run A→B→C→D.")

(defvar sd7-perf--mode-c-timeout nil
  "Per-file timeout (seconds) for Mode C `font-lock-ensure' calls.
When nil (the default), no timeout is applied and `font-lock-ensure' runs
to completion regardless of how long it takes.

Set to a positive float to cap the time spent fontifying any single file.
Files that exceed the limit are skipped (fontification is abandoned) and a
warning is printed to *Messages*; their timing entries still appear in the
report with the time measured up to the timeout.

Example — cap each file at 30 seconds:

  (setq sd7-perf--mode-c-timeout 30.0)

Typical use: set to 60.0 when running on a large Seed7 tree to prevent any
single catastrophically-backtracking file from stalling the benchmark for
tens of minutes.")

;;; --------------------------------------------------------------------------
;;; Help

(defun sd7-perf-help ()
  "Show usage information for `sd7-perf-run'."
  (interactive)
  (message "\
sd7-perf-run — Four-Mode Seed7 Mode Performance Benchmark
==========================================================

Modes: A (activation only)  B (+ first screen jit-lock)
       C (+ full font-lock-ensure)  D (+ full scroll jit-lock)
Modes B and D require an interactive (-nw) frame.
Mode C (font-lock-ensure) can be very slow on large files; use MODES
to skip it (e.g. \"ABD\") or set sd7-perf--mode-c-timeout for a per-file cap.

Interactive (emacs -Q -nw):

  M-x sd7-perf-run
  Prompts for SEED7_DIR, REPORT_DIR, ID, ITERATIONS, MODES.

Command-line — selected modes:

  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS [MODES]]

Command-line — batch (modes A + C only by default):

  emacs -Q --batch --load tools/sd7-perf.el \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS [MODES]]

MODES: string of mode letters, e.g. \"ABD\" to skip Mode C.
       Default: \"ABCD\" (-nw) or \"AC\" (--batch).

Per-file timeout for Mode C (Lisp, set before running):

  (setq sd7-perf--mode-c-timeout 30.0)

Output: REPORT_DIR/seed7mode-perf-ID.rst"))

;;; --------------------------------------------------------------------------
;;; Mode-specific open-file implementations

;; Mode A: use the existing sd7-controlled--open-file unchanged.
;; (no new function needed; sd7-controlled--open-file IS mode A)

(defun sd7-perf--open-file-c (file-name)
  "Mode C: open FILE-NAME and force full-buffer fontification.
Uses `font-lock-ensure' to fontify the entire buffer at once.
This is a stress-test / worst-case measurement.
Respects `sd7-perf--mode-c-timeout': if non-nil, `font-lock-ensure' is
wrapped in `with-timeout'; files that exceed the limit are skipped and
a warning is printed to *Messages*.
Returns the line count."
  (let* ((existing (get-file-buffer file-name))
         (buf      (or existing (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (unless font-lock-mode
        (font-lock-mode 1))
      (when (fboundp 'font-lock-ensure)
        (if (and sd7-perf--mode-c-timeout (> sd7-perf--mode-c-timeout 0))
            (when (with-timeout (sd7-perf--mode-c-timeout t)
                    (font-lock-ensure)
                    nil)
              ;; with-timeout returned t → fontification did not finish.
              (message "sd7-perf: Mode C: %s exceeded %.0fs timeout — fontification incomplete"
                       (file-name-nondirectory file-name)
                       sd7-perf--mode-c-timeout))
          (font-lock-ensure)))
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing
      (kill-buffer buf))
    line-count))

(defun sd7-perf--open-file-b (file-name)
  "Mode B: open FILE-NAME, display the selected window, trigger 1 jit-lock pass.
Fontifies only the initially visible region (≈ window height lines).
Returns the line count."
  (let* ((existing (get-file-buffer file-name))
         (buf      (or existing (find-file-noselect file-name)))
         line-count)
    ;; Display the buffer in the current window so jit-lock can see it.
    (with-selected-window (selected-window)
      (switch-to-buffer buf :norecord))
    (sit-for 0)   ; trigger jit-lock for the visible region
    (with-current-buffer buf
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing
      (kill-buffer buf))
    line-count))

(defun sd7-perf--open-file-d (file-name)
  "Mode D: open FILE-NAME, scroll top to bottom triggering jit-lock per screen.
Models the cost a user pays when reading the entire file.
Returns the line count."
  (let* ((existing (get-file-buffer file-name))
         (buf      (or existing (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (goto-char (point-min)))
    (with-selected-window (selected-window)
      (switch-to-buffer buf :norecord)
      (set-window-point (selected-window) (point-min))
      (set-window-start (selected-window) (point-min))
      (sit-for 0)   ; jit-lock: first screenful
      ;; Scroll one screenful at a time until the buffer end is visible.
      (while (< (window-end (selected-window) :update)
                (with-current-buffer buf (point-max)))
        (scroll-up)
        (sit-for 0)))   ; jit-lock: each newly visible region
    (with-current-buffer buf
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing
      (kill-buffer buf))
    line-count))

;;; --------------------------------------------------------------------------
;;; Internal helpers

;; File-name abbreviation

(defun sd7-perf--abbrev-file-name (file-name)
  "Strip the leading path prefix from FILE-NAME, keeping from the Seed7 dir.

Example:
  /home/user/src/seed7/prg/chkint.sd7  →  seed7/prg/chkint.sd7
  /opt/seed7/lib/string.s7i             →  seed7/lib/string.s7i

The transformation keeps the component just above the prg/ or lib/
sub-directory (i.e. the Seed7 installation directory name)."
  (replace-regexp-in-string
   "^.*/\\([^/]+/\\(?:prg\\|lib\\)/\\)" "\\1" file-name))

(defun sd7-perf--abbreviate-results (results)
  "Return new results list with file names abbreviated and a fresh max-len.
RESULTS is a list of (fname line-count mean-time) as returned by
`sd7-controlled--timed-pass'.
Returns a cons cell (abbrev-results . max-len)."
  (let* ((abbrev-results
          (mapcar (lambda (row)
                    (list (sd7-perf--abbrev-file-name (nth 0 row))
                          (nth 1 row)
                          (nth 2 row)))
                  results))
         (max-len
          (apply #'max
                 (cons (length "File Name") ; never shorter than the header
                       (mapcar (lambda (row) (length (nth 0 row)))
                               abbrev-results)))))
    (cons abbrev-results max-len)))

(defun sd7-perf--validate-seed7-dir (seed7-dir)
  "Signal `user-error' if SEED7-DIR is not a valid Seed7 source tree."
  (unless (file-directory-p seed7-dir)
    (user-error "Not a directory: %s" seed7-dir))
  (dolist (sub '("prg" "lib"))
    (unless (file-directory-p (expand-file-name sub seed7-dir))
      (user-error "Expected sub-directory '%s/' not found under: %s"
                  sub seed7-dir))))

;;

(defun sd7-perf--output-file (report-dir id)
  "Return the absolute path for the report with identifier ID in REPORT-DIR."
  (expand-file-name (format "seed7mode-perf-%s.rst" id) report-dir))

(defun sd7-perf--run-one-mode (open-fn directory-specs iterations mode-label
                               &optional warmup-runs)
  "Run one benchmark mode: warm-up with OPEN-FN, then GC-suppressed timed pass.

OPEN-FN        : function (file-name) → line-count, patched onto
                 `sd7-controlled--open-file' for both warm-up and timed pass.
DIRECTORY-SPECS: list of (DIR EXTENSIONS) as used by
                 `sd7-controlled--warmup'.
ITERATIONS     : timed opens per file.
MODE-LABEL     : string shown in progress messages (e.g. \"A\").
WARMUP-RUNS    : number of warm-up passes before the timed pass (default 2).
                 Pass 1 to reduce warm-up cost for expensive modes such as C
                 (font-lock patterns are already compiled after Mode A runs).
                 Pass 0 to skip warm-up entirely (not recommended).

Returns (results . max-fname-len) identical to `sd7-controlled--timed-pass'."
  (let ((n-warmup (or warmup-runs 2))
        (orig (symbol-function 'sd7-controlled--open-file)))
    (unwind-protect
        (progn
          (fset 'sd7-controlled--open-file open-fn)
          (message "")
          (dotimes (i n-warmup)
            (sd7-controlled--warmup directory-specs
                                    (when (zerop i)
                                      (format "Mode %s" mode-label))))
          (message "sd7-perf: Mode %s — timed pass (%d iterations)…"
                   mode-label iterations)
          (sd7-controlled--timed-pass directory-specs iterations))
      ;; Always restore the original open function.
      (fset 'sd7-controlled--open-file orig))))

;;; --------------------------------------------------------------------------
;;; RST report generation helpers

(defun sd7-perf--insert-section (buf mode-char title-short description
                                     results max-len)
  "Insert one benchmark section into BUF.

MODE-CHAR   : character such as ?A — used in section headings.
TITLE-SHORT : brief subtitle for the section heading.
DESCRIPTION : one-sentence explanation of what is measured.
RESULTS     : list of (abbrev-fname line-count mean-time) from
              `sd7-controlled--timed-pass'.
MAX-LEN     : length of the longest filename abbreviation."
  (let* ((count     (length results))
         (times     (mapcar #'cl-caddr results))
         (avg       (/ (apply #'+ times) (float count)))
         (mn        (apply #'min times))
         (mx        (apply #'max times))
         (title-bar (make-string max-len ?=))
         (spacing   (make-string (max 1 (- max-len (length "File Name"))) ?\s))
         (section-title (format "Mode %c — %s" mode-char title-short))
         (underline  (make-string (length section-title) ?-)))
    (with-current-buffer buf
      (insert "\n")
      (insert section-title "\n")
      (insert underline "\n\n")
      (insert description "\n\n")
      (insert "File Load Times — Mode " (char-to-string mode-char)
              " (mean, GC-free)\n")
      (insert (make-string (length "File Load Times — Mode X (mean, GC-free)") ?~)
              "\n\n")
      ;; Table header
      (insert (format "%s= ================= ================\n" title-bar))
      (insert (format "File Name%s  Mean Time (s)     Line count\n" spacing))
      (insert (format "%s= ================= ================\n" title-bar))
      ;; Table rows
      (dolist (row results)
        (insert (format (format "%%-%ds  %%-15.6f   %%5d\n" max-len)
                        (nth 0 row) (nth 2 row) (nth 1 row))))
      (insert (format "%s= ================= ================\n\n" title-bar))
      ;; Statistics
      (insert (format "Statistical Summary — Mode %c (GC-free, mean-of-N)\n"
                      mode-char))
      (insert (make-string (length "Statistical Summary — Mode c (GC-free, mean-of-N)") ?~) "\n\n")
      (insert "+-----------+-----------------+\n")
      (insert "| Metric    | Mean Time (s)   |\n")
      (insert "+===========+=================+\n")
      (insert (format "| Average   | %-15.6f |\n" avg))
      (insert "+-----------+-----------------+\n")
      (insert (format "| Minimum   | %-15.6f |\n" mn))
      (insert "+-----------+-----------------+\n")
      (insert (format "| Maximum   | %-15.6f |\n" mx))
      (insert "+-----------+-----------------+\n"))
    ;; Return the summary triple for the cross-mode table.
    (list mode-char avg mn mx)))

(defun sd7-perf--insert-cross-mode-summary (buf mode-summaries)
  "Insert a cross-mode comparison table into BUF.

MODE-SUMMARIES is a list of (mode-char avg min max) as returned by
`sd7-perf--insert-section'."
  (with-current-buffer buf
    (insert "\n")
    (insert "Cross-Mode Comparison\n")
    (insert "=====================\n\n")
    (insert "+------+-----------------+-----------------+-----------------+\n")
    (insert "| Mode | Average (s)     | Minimum (s)     | Maximum (s)     |\n")
    (insert "+======+=================+=================+=================+\n")
    (dolist (entry mode-summaries)
      (insert (format "| %c    | %-15.6f | %-15.6f | %-15.6f |\n"
                      (nth 0 entry) (nth 1 entry) (nth 2 entry) (nth 3 entry)))
      (insert "+------+-----------------+-----------------+-----------------+\n"))))

;;; --------------------------------------------------------------------------
;;; Core runner

(defun sd7-perf-run-core (seed7-dir report-dir id iterations &optional modes)
  "Run the Seed7 mode performance benchmark and write the RST report.

SEED7-DIR  : path to the Seed7 source tree (must contain prg/ and lib/).
REPORT-DIR : directory where the report is written (created if absent).
ID         : free-form report identifier string; used verbatim in the filename.
ITERATIONS : number of timed opens per file (positive integer).
MODES      : optional string of mode letters to execute (any subset of
             \"ABCD\").  Defaults to \"ABCD\" in interactive (-nw) sessions and
             to \"AC\" in batch (--batch) sessions.  Modes B and D are silently
             skipped in batch mode even if requested (they require a live frame).
             Example: pass \"ABD\" to skip the slow Mode C font-lock-ensure pass.

Per-file timeout for Mode C: set `sd7-perf--mode-c-timeout' (in seconds)
before calling this function.  Default nil = no timeout.

Returns the absolute path of the written report file."
  ;; -- Validate --------------------------------------------
  (setq seed7-dir  (expand-file-name seed7-dir)
        ;; Resolve report-dir against the repo root so that a relative path
        ;; like "reports" works regardless of which buffer's default-directory
        ;; is active when M-x sd7-perf-run is called.
        report-dir (if (file-name-absolute-p report-dir)
                       (expand-file-name report-dir)
                     (expand-file-name report-dir
                                       (or sd7-perf--repo-root
                                           default-directory))))
  (sd7-perf--validate-seed7-dir seed7-dir)
  (unless (and (stringp id) (not (string-empty-p id)))
    (user-error "ID must be a non-empty string, got: %S" id))
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "ITERATIONS must be a positive integer, got: %S" iterations))

  (let* ((have-frame  (not noninteractive))
         ;; Resolve active mode set.
         ;; 1. Use caller-supplied MODES if provided and non-empty.
         ;; 2. Otherwise fall back to the session default for the frame type.
         (active-modes (upcase
                        (or (and (stringp modes) (not (string-empty-p modes)) modes)
                            (if have-frame "ABCD" "AC"))))
         (run-a (string-match-p "A" active-modes))
         ;; B and D are always skipped in batch mode regardless of MODES.
         (run-b (and have-frame (string-match-p "B" active-modes)))
         (run-c (string-match-p "C" active-modes))
         (run-d (and have-frame (string-match-p "D" active-modes)))
         (modes-display
          (concat (if run-a "A" "")
                  (if run-b "B" "")
                  (if run-c "C" "")
                  (if run-d "D" "")
                  (if (not have-frame)
                      (let ((skipped (concat (and (string-match-p "B" active-modes) "B")
                                             (and (string-match-p "D" active-modes) "D"))))
                        (if (string-empty-p skipped) ""
                          (format "  (B/D skipped — batch mode)")))
                    "")))
         (dir-specs  (list (list (expand-file-name "prg" seed7-dir) '("sd7"))
                           (list (expand-file-name "lib" seed7-dir) '("s7i"))))
         (out-file   (sd7-perf--output-file report-dir id))
         ;; Collect (results . max-len) per mode.
         result-a result-b result-c result-d
         ;; Max filename length across all modes (for consistent column width).
         global-max-len)

    (message "")
    (message "=== Seed7 Mode Performance Benchmark — Four Modes ===")
    (message "seed7-mode : %s" seed7-mode-version-timestamp)
    (message "Seed7 dir  : %s" seed7-dir)
    (message "Report dir : %s" report-dir)
    (message "Report ID  : %s" id)
    (message "Iterations : %d per file" iterations)
    (message "Modes      : %s" modes-display)
    (when sd7-perf--mode-c-timeout
      (message "C timeout  : %.0fs per file" sd7-perf--mode-c-timeout))
    (message "Output     : %s" out-file)
    (message "")

    ;; -- Run Mode A -------------------------------------------------------
    (when run-a
      (setq result-a
            (sd7-perf--abbreviate-results
             (car
              (sd7-perf--run-one-mode
               (symbol-function 'sd7-controlled--open-file)
               dir-specs iterations "A")))))

    ;; -- Run Mode B (interactive only) ------------------------------------
    (when run-b
      (setq result-b
            (sd7-perf--abbreviate-results
             (car
              (sd7-perf--run-one-mode
               #'sd7-perf--open-file-b dir-specs iterations "B")))))

    ;; -- Run Mode C -------------------------------------------------------
    ;; Mode C uses only 1 warm-up pass (instead of the default 2) because
    ;; font-lock patterns are already compiled after Mode A.  This halves
    ;; the warm-up cost for large files where font-lock-ensure is expensive.
    (when run-c
      (setq result-c
            (sd7-perf--abbreviate-results
             (car
              (sd7-perf--run-one-mode
               #'sd7-perf--open-file-c dir-specs iterations "C"
               1)))))

    ;; -- Run Mode D (interactive only) ------------------------------------
    (when run-d
      (setq result-d
            (sd7-perf--abbreviate-results
             (car
              (sd7-perf--run-one-mode
               #'sd7-perf--open-file-d dir-specs iterations "D")))))

    ;; -- Determine the widest filename column across all active modes -----
    (let ((lens (delq nil (list (and result-a (cdr result-a))
                                (and result-b (cdr result-b))
                                (and result-c (cdr result-c))
                                (and result-d (cdr result-d))))))
      (unless lens
        (user-error "No modes produced results; check MODES argument: %s" active-modes))
      (setq global-max-len (apply #'max lens)))

    ;; -- Build combined RST report ----------------------------------------
    (let ((buf (get-buffer-create "*sd7-perf-report*"))
          mode-summaries)
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)

        ;; Document header
        (insert "=======================================================\n")
        (insert "GC-Controlled Benchmark Report: Seed7 Mode — Four Modes\n")
        (insert "=======================================================\n\n")
        (insert (format ":Running with: seed7-mode %s\n"
                        seed7-mode-version-timestamp))
        (insert (format ":Generated on: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                                            (current-time) t)))
        (insert (format ":N Iterations: %d  (mean of N timed opens per file)\n"
                        iterations))
        (insert ":GC @ testing: suppressed (gc-cons-threshold = most-positive-fixnum)\n")
        (insert (format ":Warm-up info: %s\n"
                        (if run-c
                            "A/B/D: 2 untimed passes; C: 1 untimed pass (patterns pre-compiled)"
                          "2 untimed passes per mode + garbage-collect before timing")))
        (insert (format ":Modes run  : %s\n"
                        (concat (if run-a "A " "")
                                (if run-b "B " "")
                                (if run-c "C " "")
                                (if run-d "D" ""))))
        (when sd7-perf--mode-c-timeout
          (insert (format ":C timeout  : %.0f s per file\n"
                          sd7-perf--mode-c-timeout)))
        (insert "\n")

        ;; Mode legend — only list modes that were actually run.
        (insert "Mode Descriptions\n")
        (insert "=================\n\n")
        (when run-a
          (insert "A:\n   Major-mode activation only.  No window → jit-lock never fires.\n")
          (insert "   Measures: syntax-table setup + font-lock keyword compilation.\n"))
        (when run-b
          (insert "B:\n   Mode activation + initial visible jit-lock pass.\n")
          (insert "   Measures: A + cost of rendering the first screenful (≈ window height).\n"))
        (when run-c
          (insert "C:\n   Mode activation + full-buffer fontification (font-lock-ensure).\n")
          (insert "   Stress test: worst-case / catastrophic-backtracking detector.\n"))
        (when run-d
          (insert "D:\n   Mode activation + full incremental jit-lock (scroll top→bottom).\n")
          (insert "   Measures: cost a user pays when reading an entire file.\n"))
        (insert "\n"))

      ;; -- Insert one section per mode ──────────────────────────────────────────
      (when result-a
        (push (sd7-perf--insert-section
               buf ?A "Major-Mode Activation Only"
               "No window created. Measures syntax-table setup and font-lock keyword \
compilation. jit-lock never fires. Direct measure of seed7-mode overhead."
               (car result-a) global-max-len)
              mode-summaries))

      (when result-b
        (push (sd7-perf--insert-section
               buf ?B "Mode Activation + Initial Visible jit-lock Pass"
               "Buffer displayed in a window. sit-for 0 triggers jit-lock on the \
visible region (≈ terminal window height). Models plain find-file user experience."
               (car result-b) global-max-len)
              mode-summaries))

      (when result-c
        (push (sd7-perf--insert-section
               buf ?C "Mode Activation + Full-Buffer Fontification (font-lock-ensure)"
               "font-lock-ensure forces the regexp engine over the entire buffer at once. \
Stress-test measurement; useful for detecting catastrophic-backtracking regressions."
               (car result-c) global-max-len)
              mode-summaries))

      (when result-d
        (push (sd7-perf--insert-section
               buf ?D "Mode Activation + Full Incremental jit-lock (Scroll Pass)"
               "Buffer scrolled from top to bottom; sit-for 0 after each screen lets \
jit-lock fontify incrementally. Models the cost of reading an entire file."
               (car result-d) global-max-len)
              mode-summaries))

      ;; -- Cross-mode summary ───────────────────────────────────────────────────
      (sd7-perf--insert-cross-mode-summary buf (nreverse mode-summaries))

      ;; -- RST document footer ──────────────────────────────────────────────────
      (with-current-buffer buf
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n.. ---------------------------------------------------------------------------\n"))

      ;; -- Write to file ────────────────────────────────────────────────────────
      (make-directory report-dir :parents)
      (with-current-buffer buf
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) out-file nil :silent)))
      (switch-to-buffer buf))

    (message "")
    (message "Done — report written to: %s" out-file)
    out-file))

;;; --------------------------------------------------------------------------
;;; Interactive command

;;;###autoload
(defun sd7-perf-run (seed7-dir report-dir id iterations modes)
  "Run the Seed7 mode performance benchmark interactively.

Prompts for SEED7-DIR, REPORT-DIR, ID, ITERATIONS, and MODES.
Previous values are offered as defaults and remembered within the session.
The report is written to: REPORT-DIR/seed7mode-perf-ID.rst

MODES is a string of mode letters such as \"ABCD\" or \"ABD\" (to skip Mode C).
Mode C (font-lock-ensure on every file) can take a very long time on large
Seed7 trees; use \"ABD\" to skip it, or set `sd7-perf--mode-c-timeout' for a
per-file time cap."
  (interactive
   (list
    (read-directory-name "Seed7 source directory: "
                         sd7-perf--last-seed7-dir nil nil)
    (read-directory-name "Report output directory: "
                         sd7-perf--last-report-dir nil nil)
    (read-string (format "Report ID (default %S): " sd7-perf--last-id)
                 nil nil sd7-perf--last-id)
    (read-number "Iterations per file: " sd7-perf--last-iterations)
    (read-string (format "Modes to run (default %S, any subset of \"ABCD\"): "
                         sd7-perf--last-modes)
                 nil nil sd7-perf--last-modes)))
  ;; Persist for the next invocation.
  (setq sd7-perf--last-seed7-dir  (directory-file-name (expand-file-name seed7-dir))
        sd7-perf--last-report-dir (directory-file-name (expand-file-name report-dir))
        sd7-perf--last-id         id
        sd7-perf--last-iterations iterations
        sd7-perf--last-modes      (upcase modes))
  (sd7-perf-run-core seed7-dir report-dir id iterations modes))

;;; --------------------------------------------------------------------------
;;; CLI entry point

(defun sd7-perf--cli-help-and-exit (exit-code)
  "Print usage to *Messages* and exit with EXIT-CODE."
  (message "\
Usage (selected modes — requires -nw frame):
  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS [MODES]]

Usage (batch — modes A and C by default):
  emacs -Q --batch --load tools/sd7-perf.el \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS [MODES]]

  SEED7_DIR   root of the Seed7 source tree (must contain prg/ and lib/)
  REPORT_DIR  directory where the RST report is written
  ID          report identifier string (e.g.  04.0  or  xref-opt)
  ITERATIONS  timed opens per file (default: 5)
  MODES       string of mode letters to run, e.g. ABD to skip Mode C
              (default: ABCD for -nw, AC for --batch)

Output: REPORT_DIR/seed7mode-perf-ID.rst

Mode C (font-lock-ensure) can be very slow on large Seed7 trees.
Use MODES=ABD to skip it.  To cap per-file time in Mode C, set the
Lisp variable sd7-perf--mode-c-timeout (seconds) via --eval before
calling sd7-perf-main.

Examples:
  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- ~/src/seed7 reports 04.0 5
  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- ~/src/seed7 reports 04.0 5 ABD
  emacs -Q -nw --load tools/sd7-perf.el \\
        --eval \"(setq sd7-perf--mode-c-timeout 30.0)\" \\
        --eval \"(sd7-perf-main)\" \\
        -- ~/src/seed7 reports 04.0 5
  emacs -Q --batch --load tools/sd7-perf.el \\
        -- ~/src/seed7 reports 04.0 5")
  (kill-emacs exit-code))

(defun sd7-perf-main ()
  "Parse `command-line-args-left' and call `sd7-perf-run-core'.
CLI / Makefile entry point.  Exits Emacs with 0 on success or 1 on error.

Command-line arguments (after --):
  SEED7_DIR REPORT_DIR ID [ITERATIONS [MODES]]

MODES is an optional string of mode letters (e.g. \"ABD\" to skip Mode C).
Default: \"ABCD\" in -nw sessions, \"AC\" in --batch sessions."
  (let ((args command-line-args-left))
    ;; Consume all args so Emacs doesn't warn about unknown options.
    (setq command-line-args-left nil)
    (when (equal (car args) "--")
      (setq args (cdr args)))
    (when (< (length args) 3)
      (sd7-perf--cli-help-and-exit 1))
    (let* ((seed7-dir  (nth 0 args))
           (report-dir (nth 1 args))
           (id         (nth 2 args))
           (iters-str  (and (> (length args) 3) (nth 3 args)))
           (iterations (if iters-str (string-to-number iters-str) 5))
           (modes      (and (> (length args) 4) (nth 4 args))))
      (when (and iters-str (<= iterations 0))
        (message "ERROR: ITERATIONS must be a positive integer, got: %s" iters-str)
        (kill-emacs 1))
      (when (string-empty-p id)
        (message "ERROR: ID must be a non-empty string")
        (kill-emacs 1))
      (condition-case err
          (progn
            (sd7-perf-run-core seed7-dir report-dir id iterations modes)
            (kill-emacs 0))
        (user-error
         (message "ERROR: %s" (cadr err))
         (kill-emacs 1))))))

;;; --------------------------------------------------------------------------
;;; Auto-dispatch
;;
;; In --batch mode (`noninteractive' = t): fires automatically.
;; In -nw interactive mode (`noninteractive' = nil): does NOT fire;
;;   use M-x sd7-perf-run  OR  add  --eval "(sd7-perf-main)"  to the
;;   command line to trigger the CLI path.

(when noninteractive
  (sd7-perf-main))

;;; --------------------------------------------------------------------------
(provide 'sd7-perf)

;;; sd7-perf.el ends here
