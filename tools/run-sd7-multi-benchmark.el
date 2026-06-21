;;; tools/run-sd7-multi-benchmark.el --- Four-scenario Seed7 file-open benchmark  -*- lexical-binding: t; -*-

;; Created   : Sunday, June 21 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-21 00:00:00 EDT, updated by Pierre Rouleau>

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
;; Runs four file-open benchmark scenarios in a single pass and writes one
;; RST report containing four sets of per-file timing tables plus a combined
;; statistical summary.
;;
;; The four scenarios differ on two orthogonal axes:
;;
;;   Axis 1 – GC control
;;     • Plain (no GC suppression, no explicit warm-up)
;;     • Controlled (warm-up pass + GC suppressed during timed pass)
;;
;;   Axis 2 – Window scroll
;;     • No scroll  (open file, sit-for 0, measure; font-lock covers only
;;                   the initially visible region)
;;     • With scroll (open file, then scroll the window through the whole
;;                    buffer in page-sized steps, triggering font-lock on
;;                    every visible region before measuring)
;;
;;   ┌──────────────┬───────────────────┬──────────────────────────────┐
;;   │ Scenario     │ GC control        │ Window scroll                │
;;   ├──────────────┼───────────────────┼──────────────────────────────┤
;;   │ A            │ plain             │ no                           │
;;   │ B            │ plain             │ yes                          │
;;   │ C            │ controlled        │ no                           │
;;   │ D            │ controlled        │ yes                          │
;;   └──────────────┴───────────────────┴──────────────────────────────┘
;;
;; Scrolling simulation
;; --------------------
;; "Scrolling the window" means advancing through the buffer in
;; window-height-sized steps (or a fixed 40-line step in batch mode where
;; no live window exists), calling `sit-for 0' at each position to let
;; font-lock fontify the newly-revealed region.  This models the real-world
;; cost of a user paging through an entire file.
;;
;; Usage
;; -----
;;
;;  1. Interactive (inside an Emacs session):
;;       M-x sd7-multi-bench-run
;;     Prompts for SEED7_DIR, major (X), minor (Y), and ITERATIONS.
;;     Previous values are offered as defaults.
;;
;;  2. Command-line / Makefile (batch mode):
;;       emacs -Q --batch --load tools/run-sd7-multi-benchmark.el \
;;             -- SEED7_DIR X Y [ITERATIONS]
;;
;; Arguments
;; ---------
;;   SEED7_DIR  : path to the Seed7 source tree (must contain prg/ and lib/)
;;   X          : major report number  (written as %02d, e.g. 4  → "04")
;;   Y          : minor report number  (e.g. 0)
;;   ITERATIONS : timed opens per file per scenario (default: 5)
;;
;; Output file (relative to the seed7-mode repository root):
;;   reports/file-open-benchmark-multi-<XX>.<Y>.rst
;;
;; Batch-mode note
;; ---------------
;; In --batch mode `sit-for 0' is a no-op.  The internal open-file
;; primitives therefore switch to `font-lock-ensure' for fontification
;; (same strategy as `run-sd7-benchmark.el').  Scrolling in batch mode
;; uses `goto-char' to advance through the buffer in fixed steps so that
;; `font-lock-ensure' is called once per chunk, mimicking the interactive
;; window-scroll cost.

;;; --------------------------------------------------------------------------
;;; Dependencies / load-path bootstrap

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
(require 'seed7-fopen-controlled)  ; provides `sd7-controlled--warmup'
(require 'cl-lib)                  ; cl-mapcan, cl-caddr

;;; --------------------------------------------------------------------------
;;; Repository root

(defvar sd7-multi-bench--repo-root
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (expand-file-name
       ".."
       (file-name-directory (expand-file-name this-file)))))
  "Absolute path to the seed7-mode repository root.")

;;; --------------------------------------------------------------------------
;;; Remembered defaults

(defvar sd7-multi-bench--last-seed7-dir
  (expand-file-name "~/src/seed7")
  "Last Seed7 source directory used by `sd7-multi-bench-run'.")

(defvar sd7-multi-bench--last-major 4
  "Last major report number used by `sd7-multi-bench-run'.")

(defvar sd7-multi-bench--last-minor 0
  "Last minor report number used by `sd7-multi-bench-run'.")

(defvar sd7-multi-bench--last-iterations 5
  "Last iteration count used by `sd7-multi-bench-run'.")

;;; --------------------------------------------------------------------------
;;; Low-level file-open primitives (one per scenario axis combination)

(defconst sd7-multi-bench--scroll-step 40
  "Lines per scroll step used in batch mode when no live window is available.")

(defun sd7-multi-bench--scroll-buffer (buf)
  "Scroll BUF from top to bottom in page-sized steps, calling `sit-for 0' each time.
In batch mode, uses fixed `sd7-multi-bench--scroll-step'-line chunks and
calls `font-lock-ensure' per chunk instead of `sit-for 0'."
  (with-current-buffer buf
    (goto-char (point-min))
    (if noninteractive
        ;; Batch mode: advance in fixed steps, force font-lock on each chunk.
        (let ((step-chars (* sd7-multi-bench--scroll-step
                             (1+ (/ (point-max) (max 1 (line-number-at-pos (point-max))))))))
          (while (< (point) (point-max))
            (when (fboundp 'font-lock-ensure)
              (font-lock-ensure (point)
                                (min (point-max) (+ (point) step-chars))))
            (forward-char (min step-chars (- (point-max) (point))))))
      ;; Interactive mode: scroll by window-height lines at a time.
      (let ((step (max 1 (window-height))))
        (while (< (point) (point-max))
          (sit-for 0)
          (forward-line step))
        (sit-for 0)))))

(defun sd7-multi-bench--open-plain (file-name)
  "Open FILE-NAME, sit-for 0 once (no scroll), kill buffer.  Returns line count."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (if noninteractive
          (progn
            (unless font-lock-mode (font-lock-mode 1))
            (when (fboundp 'font-lock-ensure) (font-lock-ensure)))
        (sit-for 0))
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf (kill-buffer buf))
    line-count))

(defun sd7-multi-bench--open-plain-scroll (file-name)
  "Open FILE-NAME, scroll window through buffer, kill buffer.  Returns line count."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (unless font-lock-mode (font-lock-mode 1))
      (when (and noninteractive (fboundp 'font-lock-ensure))
        (font-lock-ensure))
      (sd7-multi-bench--scroll-buffer buf)
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf (kill-buffer buf))
    line-count))

(defun sd7-multi-bench--open-controlled (file-name)
  "Open FILE-NAME with GC-suppressed font-lock (no scroll).  Returns line count.
This is the same implementation as `sd7-controlled--open-file' (or its
batch override in `run-sd7-benchmark.el')."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (if noninteractive
          (progn
            (unless font-lock-mode (font-lock-mode 1))
            (when (fboundp 'font-lock-ensure) (font-lock-ensure)))
        (sit-for 0))
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf (kill-buffer buf))
    line-count))

(defun sd7-multi-bench--open-controlled-scroll (file-name)
  "Open FILE-NAME with GC-suppressed font-lock + window scroll.  Returns line count."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (unless font-lock-mode (font-lock-mode 1))
      (when (and noninteractive (fboundp 'font-lock-ensure))
        (font-lock-ensure))
      (sd7-multi-bench--scroll-buffer buf)
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf (kill-buffer buf))
    line-count))

;;; --------------------------------------------------------------------------
;;; Timed-pass engine
;;
;; Runs N iterations of OPEN-FN on every file in DIRECTORY-SPECS,
;; optionally suppressing GC.  Returns (report . max-fname-len).

(defun sd7-multi-bench--timed-pass (directory-specs iterations open-fn suppress-gc)
  "Benchmark OPEN-FN on all files in DIRECTORY-SPECS, ITERATIONS times each.
When SUPPRESS-GC is non-nil, set `gc-cons-threshold' to
`most-positive-fixnum' for the entire timed pass.
Returns (report . max-fname-len) where REPORT is a list of
\(abbrev-name line-count mean-time)."
  (let ((old-threshold  gc-cons-threshold)
        (old-percentage gc-cons-percentage)
        report
        (max-fname-len 0))
    (when suppress-gc
      (setq gc-cons-threshold  most-positive-fixnum
            gc-cons-percentage 1.0))
    (unwind-protect
        (dolist (dir-spec directory-specs)
          (let* ((directory  (car dir-spec))
                 (file-names (cl-mapcan (lambda (ext)
                                          (benchmark-sd7-files-in-dir directory ext))
                                        (cadr dir-spec))))
            (dolist (file-name file-names)
              (let* ((abbrev  (abbreviate-file-name file-name))
                     (len     (length abbrev))
                     (times   nil)
                     line-count)
                (when (> len max-fname-len)
                  (setq max-fname-len len))
                (dotimes (_ iterations)
                  (let ((info (benchmark-run 1
                                (setq line-count
                                      (funcall open-fn file-name)))))
                    (push (car info) times)))
                (let ((mean (/ (apply #'+ times) (float iterations))))
                  (push (list abbrev line-count mean) report))))))
      (when suppress-gc
        (setq gc-cons-threshold  old-threshold
              gc-cons-percentage old-percentage)))
    (cons (nreverse report) max-fname-len)))

;;; --------------------------------------------------------------------------
;;; RST report helpers

(defun sd7-multi-bench--insert-scenario-table (buf results max-fname-len
                                                   scenario-letter title
                                                   gc-label scroll-label)
  "Insert one scenario table into BUF.
RESULTS is a list of (abbrev line-count mean-time).
SCENARIO-LETTER, TITLE, GC-LABEL, SCROLL-LABEL describe the scenario."
  (let* ((count     (length results))
         (times     (mapcar #'cl-caddr results))
         (sum       (apply #'+ times))
         (avg       (/ sum count))
         (mn        (apply #'min times))
         (mx        (apply #'max times))
         (title-bar (make-string max-fname-len ?=))
         (spacing   (make-string (max 1 (- max-fname-len (length "File Name"))) ?\s)))
    (with-current-buffer buf
      ;; Section heading
      (let ((heading (format "Scenario %s – %s" scenario-letter title)))
        (insert heading "\n")
        (insert (make-string (length heading) ?-) "\n\n"))
      (insert (format ":GC control  : %s\n" gc-label))
      (insert (format ":Window scroll: %s\n\n" scroll-label))

      ;; Per-file table
      (insert "File Load Times (mean, GC-free)\n")
      (insert "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
      (insert (format "%s= ================= ================\n" title-bar))
      (insert (format "File Name%s  Mean Time (s)     Line count\n" spacing))
      (insert (format "%s= ================= ================\n" title-bar))
      (dolist (row results)
        (insert (format (format "%%-%ds  %%-15.6f   %%5d\n" max-fname-len)
                        (nth 0 row) (nth 2 row) (nth 1 row))))
      (insert (format "%s= ================= ================\n\n" title-bar))

      ;; Statistical summary
      (insert "Statistical Summary\n")
      (insert "~~~~~~~~~~~~~~~~~~~\n\n")
      (insert "+-----------+-----------------+\n")
      (insert "| Metric    | Mean Time (s)   |\n")
      (insert "+===========+=================+\n")
      (insert (format "| Average   | %-15.6f |\n" avg))
      (insert "+-----------+-----------------+\n")
      (insert (format "| Minimum   | %-15.6f |\n" mn))
      (insert "+-----------+-----------------+\n")
      (insert (format "| Maximum   | %-15.6f |\n" mx))
      (insert "+-----------+-----------------+\n\n"))))

;;; --------------------------------------------------------------------------
;;; Core runner

(defun sd7-multi-bench--validate-seed7-dir (seed7-dir)
  "Signal `user-error' if SEED7-DIR is not a valid Seed7 source tree."
  (unless (file-directory-p seed7-dir)
    (user-error "Not a directory: %s" seed7-dir))
  (dolist (sub '("prg" "lib"))
    (unless (file-directory-p (expand-file-name sub seed7-dir))
      (user-error "Expected sub-directory '%s/' not found under: %s" sub seed7-dir))))

(defun sd7-multi-bench--output-file (major minor)
  "Return the absolute report path for MAJOR.MINOR."
  (unless sd7-multi-bench--repo-root
    (user-error
     "Cannot determine repository root (sd7-multi-bench--repo-root is nil)"))
  (expand-file-name
   (format "reports/file-open-benchmark-multi-%02d.%d.rst" major minor)
   sd7-multi-bench--repo-root))

(defun sd7-multi-bench-run-core (seed7-dir major minor iterations)
  "Run the four-scenario benchmark and write the RST report.

SEED7-DIR   : path to the Seed7 source tree (must contain prg/ and lib/).
MAJOR       : major report number (integer; padded to %02d in filename).
MINOR       : minor report number (integer).
ITERATIONS  : timed opens per file per scenario (positive integer).

The report is written to:
  reports/file-open-benchmark-multi-<MM>.<N>.rst
relative to the seed7-mode repository root.

Returns the absolute path of the written report file."
  ;; Validate
  (setq seed7-dir (expand-file-name seed7-dir))
  (sd7-multi-bench--validate-seed7-dir seed7-dir)
  (unless (and (integerp major) (integerp minor))
    (user-error "MAJOR and MINOR must be integers"))
  (unless (and (integerp iterations) (> iterations 0))
    (user-error "ITERATIONS must be a positive integer, got: %S" iterations))

  (let* ((dir-specs (list (list (expand-file-name "prg" seed7-dir) '("sd7"))
                          (list (expand-file-name "lib" seed7-dir) '("s7i"))))
         (out-file  (sd7-multi-bench--output-file major minor))
         (buf-name  "*sd7-multi-benchmark*"))

    (message "")
    (message "=== Four-Scenario Seed7 File-Open Benchmark ===")
    (message "seed7-mode : %s" seed7-mode-version-timestamp)
    (message "Seed7 dir  : %s" seed7-dir)
    (message "Iterations : %d per file per scenario" iterations)
    (message "Output     : %s" out-file)
    (message "")

    ;; ── Scenario A: plain, no scroll ──────────────────────────────────────
    (message "Running Scenario A: plain open, no scroll …")
    (let* ((r.len-a (sd7-multi-bench--timed-pass
                     dir-specs iterations
                     #'sd7-multi-bench--open-plain nil))
           (results-a (car r.len-a))
           (maxlen-a  (cdr r.len-a)))
      (when (zerop (length results-a))
        (user-error "No matching files found"))

      ;; ── Scenario B: plain, with scroll ──────────────────────────────────
      (message "Running Scenario B: plain open, with scroll …")
      (let* ((r.len-b (sd7-multi-bench--timed-pass
                       dir-specs iterations
                       #'sd7-multi-bench--open-plain-scroll nil))
             (results-b (car r.len-b))
             (maxlen-b  (cdr r.len-b)))

        ;; ── Warm-up before controlled scenarios ─────────────────────────
        (message "Warm-up pass for scenarios C and D …")
        (sd7-controlled--warmup dir-specs)

        ;; ── Scenario C: controlled (GC suppressed), no scroll ───────────
        (message "Running Scenario C: controlled, no scroll …")
        (let* ((r.len-c (sd7-multi-bench--timed-pass
                         dir-specs iterations
                         #'sd7-multi-bench--open-controlled t))
               (results-c (car r.len-c))
               (maxlen-c  (cdr r.len-c)))

          ;; ── Scenario D: controlled (GC suppressed), with scroll ─────
          (message "Running Scenario D: controlled, with scroll …")
          (let* ((r.len-d (sd7-multi-bench--timed-pass
                           dir-specs iterations
                           #'sd7-multi-bench--open-controlled-scroll t))
                 (results-d (car r.len-d))
                 (maxlen-d  (cdr r.len-d))
                 (max-fname-len (max maxlen-a maxlen-b maxlen-c maxlen-d)))

            ;; ── Compose the RST report ─────────────────────────────────
            (let ((buf (get-buffer-create buf-name)))
              (with-current-buffer buf
                (setq buffer-read-only nil)
                (erase-buffer)

                ;; Document title
                (insert "===========================================================\n")
                (insert "Four-Scenario Benchmark Report: Seed7 File Open\n")
                (insert "===========================================================\n\n")
                (insert (format ":Running with: seed7-mode %s\n"
                                seed7-mode-version-timestamp))
                (insert (format ":Generated on: %s\n"
                                (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                                                    (current-time) t)))
                (insert (format ":N Iterations: %d  (mean of N timed opens per file per scenario)\n"
                                iterations))
                (insert ":Scenarios   : A (plain/no-scroll), B (plain/scroll),\n")
                (insert ":             : C (GC-controlled/no-scroll), D (GC-controlled/scroll)\n\n")

                ;; Scenario descriptions
                (insert "Scenario Definitions\n")
                (insert "====================\n\n")
                (insert ".. list-table::\n")
                (insert "   :header-rows: 1\n\n")
                (insert "   * - Scenario\n")
                (insert "     - GC control\n")
                (insert "     - Window scroll\n")
                (insert "   * - A\n")
                (insert "     - plain (no suppression, no warm-up)\n")
                (insert "     - no\n")
                (insert "   * - B\n")
                (insert "     - plain (no suppression, no warm-up)\n")
                (insert "     - yes (page through entire buffer)\n")
                (insert "   * - C\n")
                (insert "     - controlled (warm-up + GC suppressed)\n")
                (insert "     - no\n")
                (insert "   * - D\n")
                (insert "     - controlled (warm-up + GC suppressed)\n")
                (insert "     - yes (page through entire buffer)\n\n"))

              ;; Insert each scenario's table
              (sd7-multi-bench--insert-scenario-table
               buf results-a max-fname-len
               "A" "Plain open, no scroll"
               "plain (no suppression, no warm-up)" "no")

              (sd7-multi-bench--insert-scenario-table
               buf results-b max-fname-len
               "B" "Plain open, with scroll"
               "plain (no suppression, no warm-up)" "yes (page through entire buffer)")

              (sd7-multi-bench--insert-scenario-table
               buf results-c max-fname-len
               "C" "GC-controlled open, no scroll"
               "controlled (warm-up + GC suppressed)" "no")

              (sd7-multi-bench--insert-scenario-table
               buf results-d max-fname-len
               "D" "GC-controlled open, with scroll"
               "controlled (warm-up + GC suppressed)" "yes (page through entire buffer)")

              (with-current-buffer buf
                ;; Append RST footer separator
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n.. ---------------------------------------------------------------------------\n")

                ;; Write report to file
                (make-directory (file-name-directory out-file) :parents)
                (let ((coding-system-for-write 'utf-8-unix))
                  (write-region (point-min) (point-max) out-file nil :silent)))

              (switch-to-buffer buf)
              (message "")
              (message "Report written to: %s" out-file)
              out-file)))))))

;;; --------------------------------------------------------------------------
;;; Interactive command

;;;###autoload
(defun sd7-multi-bench-run (seed7-dir major minor iterations)
  "Run the four-scenario Seed7 file-open benchmark interactively.

Prompts for SEED7-DIR, MAJOR report number, MINOR report number, and
ITERATIONS.  Previous values are offered as defaults.

The report is written to:
  reports/file-open-benchmark-multi-<MM>.<N>.rst
relative to the seed7-mode repository root."
  (interactive
   (list
    (read-directory-name "Seed7 source directory: "
                         sd7-multi-bench--last-seed7-dir nil nil)
    (read-number "Major report number (X): " sd7-multi-bench--last-major)
    (read-number "Minor report number (Y): " sd7-multi-bench--last-minor)
    (read-number "Iterations per file per scenario: "
                 sd7-multi-bench--last-iterations)))
  (setq sd7-multi-bench--last-seed7-dir  (directory-file-name
                                           (expand-file-name seed7-dir))
        sd7-multi-bench--last-major      major
        sd7-multi-bench--last-minor      minor
        sd7-multi-bench--last-iterations iterations)
  (sd7-multi-bench-run-core seed7-dir major minor iterations))

;;; --------------------------------------------------------------------------
;;; CLI entry point

(defun sd7-multi-bench--cli-help-and-exit (exit-code)
  "Print usage to *Messages* and exit Emacs with EXIT-CODE."
  (message "\
Usage: emacs -Q --batch --load tools/run-sd7-multi-benchmark.el \
-- SEED7_DIR X Y [ITERATIONS]

  SEED7_DIR   path to the Seed7 source tree (must contain prg/ and lib/)
  X           major report number  (e.g. 4)
  Y           minor report number  (e.g. 0)
  ITERATIONS  timed opens per file per scenario (default: 5)

Output: reports/file-open-benchmark-multi-<XX>.<Y>.rst
        (relative to the seed7-mode repository root)

Example:
  emacs -Q --batch --load tools/run-sd7-multi-benchmark.el -- ~/src/seed7 4 0 5")
  (kill-emacs exit-code))

(defun sd7-multi-bench-main ()
  "Parse `command-line-args-left' and call `sd7-multi-bench-run-core'.
CLI / Makefile entry point.  Exits Emacs with 0 on success or 1 on error."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (when (< (length args) 3)
      (sd7-multi-bench--cli-help-and-exit 1))
    (let* ((seed7-dir  (nth 0 args))
           (major-str  (nth 1 args))
           (minor-str  (nth 2 args))
           (iters-str  (and (> (length args) 3) (nth 3 args)))
           (major      (string-to-number major-str))
           (minor      (string-to-number minor-str))
           (iterations (if iters-str (string-to-number iters-str) 5)))
      (when (and iters-str (<= iterations 0))
        (message "ERROR: ITERATIONS must be a positive integer, got: %s" iters-str)
        (kill-emacs 1))
      (condition-case err
          (progn
            (sd7-multi-bench-run-core seed7-dir major minor iterations)
            (kill-emacs 0))
        (user-error
         (message "ERROR: %s" (cadr err))
         (kill-emacs 1))))))

;;; --------------------------------------------------------------------------
;;; CLI auto-dispatch

(when noninteractive
  (sd7-multi-bench-main))

;;; --------------------------------------------------------------------------
(provide 'run-sd7-multi-benchmark)

;;; run-sd7-multi-benchmark.el ends here
