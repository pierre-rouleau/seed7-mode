;;; tools/sd7-perf.el --- Four-mode Seed7 mode performance benchmark  -*- lexical-binding: t; -*-

;; Created   : Sunday, June 22 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-22 15:23:58 EDT, updated by Pierre Rouleau>

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

;; ---------------------------------------------------------------------------
;;; Progress state helpers

(defvar sd7-perf--current-phase "not started"
  "Human-readable benchmark phase currently executing.")

(defvar sd7-perf--current-phase-start nil
  "Float timestamp when the current benchmark phase started.")

(defvar sd7-perf--current-file nil
  "File currently being processed, or nil.")

(defvar sd7-perf--current-iteration nil
  "Timed-pass iteration currently being processed, or nil.")

(defun sd7-perf--format-duration (seconds)
  "Format SECONDS as HH:MM:SS.mmm."
  (if (null seconds)
      "—"
    (let* ((whole (floor seconds))
           (h     (/ whole 3600))
           (m     (/ (% whole 3600) 60))
           (s     (% whole 60))
           (ms    (floor (* (- seconds whole) 1000))))
      (format "%02d:%02d:%02d.%03d" h m s ms))))

(defun sd7-perf--set-phase (phase &optional file iteration announce)
  "Record current PHASE, FILE, and ITERATION.

When ANNOUNCE is non-nil, emit a timestamp-prefixed progress message."
  (setq sd7-perf--current-phase phase
        sd7-perf--current-phase-start (float-time)
        sd7-perf--current-file file
        sd7-perf--current-iteration iteration)
  (when announce
    (sd7-controlled--message "%s%s%s"
                             phase
                             (if file
                                 (format " — %s" (abbreviate-file-name file))
                               "")
                             (if iteration
                                 (format " — iteration %d" iteration)
                               ""))))

(defun sd7-perf--progress-callback (kind label file-name iteration)
  "Update `sd7-perf' progress state from controlled benchmark callbacks."
  (let* ((label-text (or label "unknown mode"))
         (kind-text  (pcase kind
                       (:warm-up "warm-up")
                       (:timed-pass "timed pass")
                       (_ (format "%s" kind))))
         (phase      (format "%s %s" label-text kind-text)))
    (setq sd7-perf--current-phase phase
          sd7-perf--current-file file-name
          sd7-perf--current-iteration iteration)
    ;; Emit one message per file, but not for every timed iteration.
    ;; This gives useful diagnostics without overwhelming `*Messages*' or
    ;; adding excessive measurement overhead.
    (when (null iteration)
      (sd7-controlled--message "%s — %s"
                               phase
                               (abbreviate-file-name file-name)))))

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

Interactive (emacs -Q -nw):

  M-x sd7-perf-run
  Prompts for SEED7_DIR, REPORT_DIR, ID, ITERATIONS.

Command-line — all 4 modes:

  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS]

Command-line — batch (modes A + C only):

  emacs -Q --batch --load tools/sd7-perf.el \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS]

Output: REPORT_DIR/seed7mode-perf-ID.rst"))

;;; --------------------------------------------------------------------------
;;; Mode-specific open-file implementations

;; Mode A: use the existing sd7-controlled--open-file unchanged.
;; (no new function needed; sd7-controlled--open-file IS mode A)

(defun sd7-perf--open-file-c (file-name)
  "Mode C: open FILE-NAME and force full-buffer fontification.
Uses `font-lock-ensure' to fontify the entire buffer at once.
This is a stress-test / worst-case measurement.
Returns the line count."
  (let* ((existing (get-file-buffer file-name))
         (buf      (or existing (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (unless font-lock-mode
        (font-lock-mode 1))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure))
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing
      (kill-buffer buf))
    line-count))

(defun sd7-perf--open-file-b (file-name)
  "Mode B: open FILE-NAME at top, display it, trigger 1 jit-lock pass.

Fontifies only the initially visible region at the beginning of the file
\(≈ window height lines).  This intentionally resets point/window-start to
`point-min' so an existing buffer's old point does not make Mode B measure an
arbitrary later region, such as an unterminated block comment near EOF.

Returns the line count."
  (let* ((existing (get-file-buffer file-name))
         (buf      (or existing (find-file-noselect file-name)))
         line-count)
    ;; Mode B is intended to measure the initial screenful when opening a file.
    ;; If BUF already existed, its point/window state may be near EOF or inside
    ;; pathological invalid syntax.  Reset explicitly before displaying.
    (with-current-buffer buf
      (goto-char (point-min)))
    (with-selected-window (selected-window)
      (switch-to-buffer buf :norecord)
      (set-window-point (selected-window) (point-min))
      (set-window-start (selected-window) (point-min) :noforce))
    (sit-for 0)   ; trigger jit-lock for the first visible region
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


(defun sd7-perf--run-one-mode (open-fn directory-specs iterations mode-label)
  "Run one benchmark mode.

Returns (RESULT WARMUP-TIME TIMED-PASS-TIME), where RESULT is the
`sd7-controlled--timed-pass' return value."
  (let ((orig  (symbol-function 'sd7-controlled--open-file))
        (label (format "Mode %s" mode-label)))
    (unwind-protect
        (progn
          (fset 'sd7-controlled--open-file open-fn)

          ;; Warm-up.
          (sd7-perf--set-phase (format "%s warm-up" label) nil nil t)
          (let* ((warm-start (float-time))
                 (_          (sd7-controlled--warmup
                              directory-specs label
                              #'sd7-perf--progress-callback))
                 (warm-time  (- (float-time) warm-start)))

            ;; Timed pass.
            (sd7-perf--set-phase (format "%s timed pass" label) nil nil t)
            (let* ((test-start (float-time))
                   (result     (sd7-controlled--timed-pass
                                directory-specs iterations label
                                #'sd7-perf--progress-callback))
                   (test-time  (- (float-time) test-start)))
              (sd7-controlled--message
               "%s complete — warm-up %s, timed pass %s, total %s"
               label
               (sd7-perf--format-duration warm-time)
               (sd7-perf--format-duration test-time)
               (sd7-perf--format-duration (+ warm-time test-time)))
              (list result warm-time test-time))))
      ;; Always restore original open function, including after C-g.
      (fset 'sd7-controlled--open-file orig))))

;; ---------------------------------------------------------------------------
;; Timing summary helper

(defun sd7-perf--insert-timing-summary (buf mode-times total-elapsed
                                            interrupted-p interrupted-during)
  "Insert warm-up/timed-pass timing summary into BUF."
  (with-current-buffer buf
    (let* ((fmt #'sd7-perf--format-duration)
           (mode-total (lambda (wu tp)
                         (and wu tp (+ wu tp))))
           (rows `(("Mode A" ,(plist-get mode-times :a-wu)
                    ,(plist-get mode-times :a-tp))
                   ("Mode B" ,(plist-get mode-times :b-wu)
                    ,(plist-get mode-times :b-tp))
                   ("Mode C" ,(plist-get mode-times :c-wu)
                    ,(plist-get mode-times :c-tp))
                   ("Mode D" ,(plist-get mode-times :d-wu)
                    ,(plist-get mode-times :d-tp)))))
      (goto-char (point-max))
      (insert "\nPhase Timing Summary")
      (insert "\n====================\n\n")
      (when interrupted-p
        (insert (format "**Partial report: interrupted during %s.**\n\n"
                        interrupted-during)))
      (insert "Wall-clock times include warm-up work and GC performed by the warm-up helper.\n\n")
      (insert "+----------+--------------+--------------+--------------+\n")
      (insert "| Phase    | Warm-up      | Timed pass   | Mode total   |\n")
      (insert "+==========+==============+==============+==============+\n")
      (dolist (row rows)
        (let* ((label (nth 0 row))
               (wu    (nth 1 row))
               (tp    (nth 2 row))
               (tot   (funcall mode-total wu tp)))
          (when (or wu tp)
            (insert (format "| %-8s | %12s | %12s | %12s |\n"
                            label
                            (funcall fmt wu)
                            (funcall fmt tp)
                            (funcall fmt tot)))
            (insert "+----------+--------------+--------------+--------------+\n"))))
      (insert (format "| %-8s | %12s | %12s | %12s |\n"
                      "Total" "" "" (funcall fmt total-elapsed)))
      (insert "+----------+--------------+--------------+--------------+\n\n"))))


;; ---------------------------------------------------------------------------
;; Report finalization helper

(defun sd7-perf--finish-report (seed7-dir report-dir id iterations have-frame
                                         result-a result-b result-c result-d
                                         mode-times total-elapsed
                                         interrupted-p interrupted-during)
  "Build and write full or partial sd7-perf report."
  (let* ((out-file (sd7-perf--output-file report-dir id))
         (buf      (get-buffer-create "*sd7-perf-report*"))
         (available-max-lens
          (delq nil (list (and result-a (cdr result-a))
                          (and result-b (cdr result-b))
                          (and result-c (cdr result-c))
                          (and result-d (cdr result-d))
                          20)))
         (global-max-len (apply #'max available-max-lens))
         mode-summaries)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)

      (when interrupted-p
        (insert ".. ===========================================================\n")
        (insert ".. PARTIAL REPORT — BENCHMARK WAS INTERRUPTED BY THE USER\n")
        (insert (format ".. Interrupted during: %s\n" interrupted-during))
        (when sd7-perf--current-file
          (insert (format ".. Current file: %s\n"
                          (abbreviate-file-name sd7-perf--current-file))))
        (when sd7-perf--current-iteration
          (insert (format ".. Current iteration: %d\n"
                          sd7-perf--current-iteration)))
        (when sd7-perf--current-phase-start
          (insert (format ".. Interrupted phase elapsed so far: %s\n"
                          (sd7-perf--format-duration
                           (- (float-time) sd7-perf--current-phase-start)))))
        (insert ".. Results below include only completed phases.\n")
        (insert ".. ===========================================================\n\n"))

      (insert "=======================================================\n")
      (insert (if interrupted-p
                  "PARTIAL GC-Controlled Benchmark Report: Seed7 Mode — Four Modes\n"
                "GC-Controlled Benchmark Report: Seed7 Mode — Four Modes\n"))
      (insert "=======================================================\n\n")
      (insert (format ":Running with: seed7-mode %s\n"
                      seed7-mode-version-timestamp))
      (insert (format ":Seed7 dir   : %s\n"
                      (file-name-as-directory (expand-file-name seed7-dir))))

      (insert (format ":Started at: %s local time\n"
                      sd7-controlled--started-at))

      (insert (format ":Generated on: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                                          (current-time) t)))
      (insert (format ":N Iterations: %d  (mean of N timed opens per file)\n"
                      iterations))
      (insert ":GC @ testing: suppressed (gc-cons-threshold = most-positive-fixnum)\n")
      (insert ":Warm-up info: yes (1 untimed pass per mode + garbage-collect before timing)\n")
      (insert (format ":Modes planned: %s\n"
                      (if have-frame "A B C D"
                        "A C  (B and D require an interactive -nw frame)")))
      (when interrupted-p
        (insert (format ":INTERRUPTED: yes — during %s\n" interrupted-during)))
      (insert "\n")

      (insert "Mode Descriptions\n")
      (insert "=================\n\n")
      (insert "A:\n   Major-mode activation only. No window → jit-lock never fires.\n")
      (insert "B:\n   Mode activation + initial visible jit-lock pass.\n")
      (insert "C:\n   Mode activation + full-buffer fontification (font-lock-ensure).\n")
      (insert "D:\n   Mode activation + full incremental jit-lock (scroll top→bottom).\n\n"))

    (when result-a
      (push (sd7-perf--insert-section
             buf ?A "Major-Mode Activation Only"
             "No window created. Measures syntax-table setup and font-lock keyword compilation."
             (car result-a) global-max-len)
            mode-summaries))
    (when result-b
      (push (sd7-perf--insert-section
             buf ?B "Mode Activation + Initial Visible jit-lock Pass"
             "Buffer displayed in a window; sit-for 0 triggers visible-region jit-lock."
             (car result-b) global-max-len)
            mode-summaries))
    (when result-c
      (push (sd7-perf--insert-section
             buf ?C "Mode Activation + Full-Buffer Fontification (font-lock-ensure)"
             "Full-buffer fontification stress test."
             (car result-c) global-max-len)
            mode-summaries))
    (when result-d
      (push (sd7-perf--insert-section
             buf ?D "Mode Activation + Full Incremental jit-lock (Scroll Pass)"
             "Scrolls through the full buffer and lets jit-lock work incrementally."
             (car result-d) global-max-len)
            mode-summaries))

    (when mode-summaries
      (sd7-perf--insert-cross-mode-summary buf (nreverse mode-summaries)))

    (sd7-perf--insert-timing-summary
     buf mode-times total-elapsed interrupted-p interrupted-during)

    (with-current-buffer buf
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n.. ---------------------------------------------------------------------------\n"))

    (make-directory report-dir :parents)
    (with-current-buffer buf
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) out-file nil :silent)))
    (switch-to-buffer buf)
    out-file))

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

(defun sd7-perf-run-core (seed7-dir report-dir id iterations)
  "Run the four-mode Seed7 performance benchmark and write the RST report.

SEED7-DIR  : path to the Seed7 source tree (must contain prg/ and lib/).
REPORT-DIR : directory where the report is written (created if absent).
ID         : free-form report identifier string; used verbatim in the filename.
ITERATIONS : number of timed opens per file (positive integer).

In interactive (-nw) mode: runs modes A, B, C, D.
In batch (--batch) mode  : runs modes A and C only (B and D require a frame).

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

  (let* ((dir-specs (list (list (expand-file-name "prg" seed7-dir) '("sd7"))
                          (list (expand-file-name "lib" seed7-dir) '("s7i"))))
         (out-file (sd7-perf--output-file report-dir id))
         (have-frame (not noninteractive))
         (total-start nil)
         result-a result-b result-c result-d
         time-a-wu time-a-tp
         time-b-wu time-b-tp
         time-c-wu time-c-tp
         time-d-wu time-d-tp
         interrupted-p
         interrupted-during)

    (sd7-controlled--reset-start-time)
    (setq total-start (float-time))

    (sd7-controlled--message "Seed7 Mode Performance Benchmark — Four Modes")
    (sd7-controlled--message "seed7-mode: %s" seed7-mode-version-timestamp)
    (sd7-controlled--message "Seed7 dir: %s" seed7-dir)
    (sd7-controlled--message "Report dir: %s" report-dir)
    (sd7-controlled--message "Report ID: %s" id)
    (sd7-controlled--message "Iterations: %d per file" iterations)
    (sd7-controlled--message "Modes: %s"
                             (if have-frame "A B C D"
                               "A C (B and D skipped — batch mode)"))
    (sd7-controlled--message "Output: %s" out-file)

    (condition-case _quit
        (progn
          ;; Mode A
          (let* ((ret (sd7-perf--run-one-mode
                       (symbol-function 'sd7-controlled--open-file)
                       dir-specs iterations "A"))
                 (raw-result (car (nth 0 ret))))
            (setq result-a (sd7-perf--abbreviate-results raw-result)
                  time-a-wu (nth 1 ret)
                  time-a-tp (nth 2 ret)))

          ;; Mode B
          (when have-frame
            (let* ((ret (sd7-perf--run-one-mode
                         #'sd7-perf--open-file-b dir-specs iterations "B"))
                   (raw-result (car (nth 0 ret))))
              (setq result-b (sd7-perf--abbreviate-results raw-result)
                    time-b-wu (nth 1 ret)
                    time-b-tp (nth 2 ret))))

          ;; Mode C
          (let* ((ret (sd7-perf--run-one-mode
                       #'sd7-perf--open-file-c dir-specs iterations "C"))
                 (raw-result (car (nth 0 ret))))
            (setq result-c (sd7-perf--abbreviate-results raw-result)
                  time-c-wu (nth 1 ret)
                  time-c-tp (nth 2 ret)))

          ;; Mode D
          (when have-frame
            (let* ((ret (sd7-perf--run-one-mode
                         #'sd7-perf--open-file-d dir-specs iterations "D"))
                   (raw-result (car (nth 0 ret))))
              (setq result-d (sd7-perf--abbreviate-results raw-result)
                    time-d-wu (nth 1 ret)
                    time-d-tp (nth 2 ret)))))
      (quit
       (setq interrupted-p t
             interrupted-during sd7-perf--current-phase)
       (sd7-controlled--message
        "interrupted during %s%s%s — writing partial report"
        sd7-perf--current-phase
        (if sd7-perf--current-file
            (format " — %s" (abbreviate-file-name sd7-perf--current-file))
          "")
        (if sd7-perf--current-iteration
            (format " — iteration %d" sd7-perf--current-iteration)
          ""))))

    (let* ((total-elapsed (- (float-time) total-start))
           (mode-times (list :a-wu time-a-wu :a-tp time-a-tp
                             :b-wu time-b-wu :b-tp time-b-tp
                             :c-wu time-c-wu :c-tp time-c-tp
                             :d-wu time-d-wu :d-tp time-d-tp))
           (written (sd7-perf--finish-report
                     seed7-dir report-dir id iterations have-frame
                     result-a result-b result-c result-d
                     mode-times total-elapsed
                     interrupted-p interrupted-during)))
      (sd7-controlled--message "%s report written to: %s"
                               (if interrupted-p "partial" "complete")
                               written)
      written)))

;;; --------------------------------------------------------------------------
;;; Interactive command

;;;###autoload
(defun sd7-perf-run (seed7-dir report-dir id iterations)
  "Run the four-mode Seed7 performance benchmark interactively.

Prompts for SEED7-DIR, REPORT-DIR, ID, and ITERATIONS.
Previous values are offered as defaults and remembered within the session.
The report is written to: REPORT-DIR/seed7mode-perf-ID.rst"
  (interactive
   (list
    (read-directory-name "Seed7 source directory: "
                         sd7-perf--last-seed7-dir nil nil)
    (read-directory-name "Report output directory: "
                         sd7-perf--last-report-dir nil nil)
    (read-string (format "Report ID (default %S): " sd7-perf--last-id)
                 nil nil sd7-perf--last-id)
    (read-number "Iterations per file: " sd7-perf--last-iterations)))
  ;; Persist for the next invocation.
  (setq sd7-perf--last-seed7-dir  (directory-file-name (expand-file-name seed7-dir))
        sd7-perf--last-report-dir (directory-file-name (expand-file-name report-dir))
        sd7-perf--last-id         id
        sd7-perf--last-iterations iterations)
  (sd7-perf-run-core seed7-dir report-dir id iterations))

;;; --------------------------------------------------------------------------
;;; CLI entry point

(defun sd7-perf--cli-help-and-exit (exit-code)
  "Print usage to *Messages* and exit with EXIT-CODE."
  (message "\
Usage (all 4 modes — requires -nw frame):
  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS]

Usage (batch — modes A and C only):
  emacs -Q --batch --load tools/sd7-perf.el \\
        -- SEED7_DIR REPORT_DIR ID [ITERATIONS]

  SEED7_DIR   root of the Seed7 source tree (must contain prg/ and lib/)
  REPORT_DIR  directory where the RST report is written
  ID          report identifier string (e.g.  04.0  or  xref-opt)
  ITERATIONS  timed opens per file (default: 5)

Output: REPORT_DIR/seed7mode-perf-ID.rst

Examples:
  emacs -Q -nw --load tools/sd7-perf.el --eval \"(sd7-perf-main)\" \\
        -- ~/src/seed7 ~/reports 04.0 5
  emacs -Q --batch --load tools/sd7-perf.el \\
        -- ~/src/seed7 ~/reports 04.0 5")
  (kill-emacs exit-code))

(defun sd7-perf-main ()
  "Parse `command-line-args-left' and call `sd7-perf-run-core'.
CLI / Makefile entry point.  Exits Emacs with 0 on success or 1 on error."
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
           (iterations (if iters-str (string-to-number iters-str) 5)))
      (when (and iters-str (<= iterations 0))
        (message "ERROR: ITERATIONS must be a positive integer, got: %s" iters-str)
        (kill-emacs 1))
      (when (string-empty-p id)
        (message "ERROR: ID must be a non-empty string")
        (kill-emacs 1))
      (condition-case err
          (progn
            (sd7-perf-run-core seed7-dir report-dir id iterations)
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
