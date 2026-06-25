;;; tools/sd7-indent-perf.el --- Seed7 indentation performance & correctness benchmark  -*- lexical-binding: t; -*-

;; Created   : Tuesday, June 24 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 12:06:59 EDT, updated by Pierre Rouleau>

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
;; Measures the time taken by `indent-region' to re-indent every Seed7 file
;; found (recursively) under INPUT-DIR, writes the re-indented content to a
;; mirrored OUTPUT-DIR tree (leaving the originals untouched), and produces an
;; RST benchmark + correctness report.
;;
;; The two-tree design is the key feature:
;;
;;   INPUT-DIR/prg/chkint.sd7   (never modified)
;;   OUTPUT-DIR/prg/chkint.sd7  (content after re-indentation by seed7-mode)
;;
;; Comparing the two trees with:
;;
;;   diff -r INPUT-DIR OUTPUT-DIR
;;
;; shows every line that seed7-mode would change when indenting a file.
;; Running the tool before and after a change to seed7-mode.el and then
;; diffing the two OUTPUT-DIRs reveals any behavioural regression introduced
;; by the change.
;;
;; Indentation exercises `seed7-calc-indent' on every line.  That function
;; calls (among others) `seed7-line-inside-array-definition-block' which
;; uses `seed7--array-definition-start-regexp' — the regexp simplified in
;; this PR.  This benchmark is therefore the correct tool for measuring the
;; impact of changes to indentation-path regexps.
;;
;; Usage
;; -----
;;
;; 1. Interactive (preferred):
;;
;;      M-: (setq debug-on-quit t)
;;      M-x sd7-indent-perf-run
;;
;;    Prompts for INPUT-DIR, OUTPUT-DIR, REPORT-DIR, and ID.
;;
;; 2. Batch:
;;
;;      emacs -Q --batch --load tools/sd7-indent-perf.el \
;;            -- INPUT_DIR OUTPUT_DIR REPORT_DIR ID
;;
;; 3. Makefile:
;;
;;      $(EMACS) -Q --batch --load tools/sd7-indent-perf.el \
;;               -- $(INPUT_DIR) $(OUTPUT_DIR) $(REPORT_DIR) $(ID)
;;
;; Arguments:
;;   INPUT_DIR  : root of the input directory tree (read-only)
;;   OUTPUT_DIR : root of the output directory tree (created/overwritten)
;;   REPORT_DIR : directory where the RST report is written (created if absent)
;;   ID         : free-form report identifier (e.g. "01", "before-simplify")
;;
;; Output:
;;   OUTPUT_DIR/  — mirrored tree with re-indented .sd7 / .s7i files
;;   REPORT_DIR/seed7mode-indent-perf-ID.rst

;;; --------------------------------------------------------------------------
;;; Load-path bootstrap

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
(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; Repository root

(defvar sd7-indent-perf--repo-root
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

(defvar sd7-indent-perf--last-input-dir
  (expand-file-name "~/src/seed7")
  "Last INPUT-DIR used.  Offered as default in `sd7-indent-perf-run'.")

(defvar sd7-indent-perf--last-output-dir
  (expand-file-name "~/tmp/sd7-indent-out")
  "Last OUTPUT-DIR used.  Offered as default in `sd7-indent-perf-run'.")

(defvar sd7-indent-perf--last-report-dir
  (when sd7-indent-perf--repo-root
    (expand-file-name "reports" sd7-indent-perf--repo-root))
  "Last REPORT-DIR used.  Defaults to reports/ in the repo root.")

(defvar sd7-indent-perf--last-id "01"
  "Last report ID used.  Offered as default in `sd7-indent-perf-run'.")

;;; --------------------------------------------------------------------------
;;; Warning capture

(defvar sd7-indent-perf--current-warnings nil
  "Warnings collected from seed7-mode during the current file's `indent-region'.
Each element is a string of the form produced by seed7-calc-indent when it
encounters a construct it cannot indent, e.g.:
  \"At line 183: string line syntax not yet supported!
    Recurse count=0 Please report.\"
Reset to nil at the start of each file; populated by
`sd7-indent-perf--capture-advice'.")

(defun sd7-indent-perf--capture-advice (orig-fn fmt &rest args)
  "Advice around `message' to capture seed7-mode indentation warnings.
Strings matching \"not yet supported\" or \"timed out\" are appended to
`sd7-indent-perf--current-warnings' in addition to being displayed normally.
Strings matching \"Indenting region\" or \"delay-mode-hooks\" are suppressed
so they do not overwrite the tool's own progress messages in the echo area."
  (let ((txt (condition-case nil
                 (apply #'format fmt args)
               (error (format "%s" fmt)))))
    (when (string-match-p
           (rx (or "not yet supported"  ; in string handling in `seed7-calc-indent'
                   "timed out"))        ; timed out in `seed7--to-top' or
                                        ; `seed7-re-search-backward'
           txt)
      (push txt sd7-indent-perf--current-warnings))
    (unless (string-match-p
             (rx (or "Indenting region"
                     "delay-mode-hooks"))   ; ← suppress Emacs internal noise
             txt)
      (apply orig-fn fmt args))))

;;; --------------------------------------------------------------------------
;;; Helpers

(defconst sd7-indent-perf--progress-buffer "*sd7-indent-perf*"
  "Buffer name for live progress output during an `sd7-indent-perf-run' session.")

(defun sd7-indent-perf--message (fmt &rest args)
  "Emit a timestamped progress message to `*Messages*' and `*sd7-indent-perf*'."
  (let ((text (format "[%s] %s"
                      (format-time-string "%H:%M:%S")
                      (apply #'format fmt args))))
    ;; Write to *Messages* / echo area.
    (message "%s" text)
    ;; Also append to the dedicated progress buffer (creates it if absent).
    (with-current-buffer (get-buffer-create sd7-indent-perf--progress-buffer)
      (goto-char (point-max))
      (insert text "\n")
      ;; Scroll any window showing this buffer to the bottom so the user
      ;; can see the latest message without manually scrolling.
      (let ((win (get-buffer-window sd7-indent-perf--progress-buffer t)))
        (when win
          (set-window-point win (point-max)))))
    ;; Force Emacs to repaint the screen so the message appears immediately
    ;; even while a long computation (e.g. indent-region) is running.
    (redisplay)))

(defun sd7-indent-perf--format-duration (seconds)
  "Format SECONDS as MM:SS.mmm."
  (if (null seconds)
      "—"
    (let* ((whole (floor seconds))
           (m     (/ whole 60))
           (s     (% whole 60))
           (ms    (floor (* (- seconds whole) 1000))))
      (format "%02d:%02d.%03d" m s ms))))

(defun sd7-indent-perf--seed7-files (input-dir)
  "Return a sorted list of all .sd7 and .s7i files under INPUT-DIR.
Files are returned in spec-list order: prg/*.sd7 alphabetically first,
then lib/*.s7i alphabetically — matching the ordering used by sd7-perf.el
and sd7-nav-index.el."
  (let ((prg-files (directory-files-recursively
                    (expand-file-name "prg" input-dir)
                    "\\.sd7\\'"))
        (lib-files (directory-files-recursively
                    (expand-file-name "lib" input-dir)
                    "\\.s7i\\'")))
    (append (sort prg-files #'string<)
            (sort lib-files #'string<))))

(defun sd7-indent-perf--output-path (input-file input-dir output-dir)
  "Return the output file path for INPUT-FILE mirroring INPUT-DIR under OUTPUT-DIR."
  (let* ((rel (file-relative-name input-file input-dir)))
    (expand-file-name rel output-dir)))

;;; --------------------------------------------------------------------------
;;; Per-file processing

(defun sd7-indent-perf--process-file (input-file input-dir output-dir)
  "Re-indent INPUT-FILE with seed7-mode and write result to the output tree.

The original INPUT-FILE is never modified.  The re-indented content is
written to OUTPUT-DIR/<relative-path-from-INPUT-DIR> **immediately** after
indentation completes, before any subsequent file is processed.

Returns a plist:
  :file        abbreviated file name (relative to INPUT-DIR)
  :lines       line count of the file
  :file-start  wall-clock time (float) when indentation started
  :indent-time elapsed time in seconds for `indent-region'
  :warnings    list of captured indentation-warning strings (may be nil)
  :error       error message string if processing failed, or nil"
  (let* ((rel        (file-relative-name input-file input-dir))
         (out-file   (sd7-indent-perf--output-path input-file input-dir output-dir))
         (out-parent (file-name-directory out-file))
         line-count
         indent-time
         file-start
         warnings
         err-msg)
    ;; Announce the start of this file with a wall-clock timestamp.
    (sd7-indent-perf--message "  indenting %s …" rel)
    (condition-case err
        (progn
          ;; Install warning capture BEFORE with-temp-buffer so that
          ;; messages emitted during seed7-mode activation (e.g.
          ;; "Making delay-mode-hooks buffer-local while locally let-bound!")
          ;; are also filtered by `sd7-indent-perf--capture-advice'.
          (setq sd7-indent-perf--current-warnings nil)
          (advice-add 'message :around #'sd7-indent-perf--capture-advice)
          (unwind-protect
              (with-temp-buffer
                (insert-file-contents input-file)
                (delay-mode-hooks
                  (seed7-mode))
                (setq line-count (line-number-at-pos (point-max)))
                (let ((t0 (current-time)))
                  (setq file-start (float-time t0))
                  (indent-region (point-min) (point-max))
                  (setq indent-time
                        (float-time (time-subtract (current-time) t0))))
                (make-directory out-parent t)
                (let ((coding-system-for-write 'undecided))
                  (write-region (point-min) (point-max) out-file nil :silent)))
            ;; Always remove the advice.
            (advice-remove 'message #'sd7-indent-perf--capture-advice)
            (setq warnings (nreverse sd7-indent-perf--current-warnings))))
      (error
       (setq err-msg (format "%s" (cadr err)))))

    ;; Progress message: elapsed time + warning count + written path.
    (if err-msg
        (sd7-indent-perf--message "  ERROR %s: %s" rel err-msg)
      (sd7-indent-perf--message
       "  done  %s  %.3fs  %d line(s)%s  → %s"
       rel
       (or indent-time 0.0)
       (or line-count 0)
       (if warnings (format "  [%d warning(s)]" (length warnings)) "")
       out-file))
    (list :file        rel
          :lines       (or line-count 0)
          :file-start  (or file-start 0.0)
          :indent-time (or indent-time 0.0)
          :warnings    warnings
          :error       err-msg)))

;;; --------------------------------------------------------------------------
;;; RST report

(defconst sd7-indent-perf--col-file  50 "Width of the File column.")
(defconst sd7-indent-perf--col-lines  7 "Width of the Lines column.")
(defconst sd7-indent-perf--col-time  16 "Width of the Time column.")

(defun sd7-indent-perf--rst-rule ()
  "Return one RST simple-table rule line."
  (format "%s %s %s"
          (make-string sd7-indent-perf--col-file  ?=)
          (make-string sd7-indent-perf--col-lines ?=)
          (make-string sd7-indent-perf--col-time  ?=)))

(defun sd7-indent-perf--write-report (results input-dir output-dir
                                               report-dir id
                                               total-elapsed)
  "Write the RST indentation benchmark report.

RESULTS is a list of plists as returned by `sd7-indent-perf--process-file'.
Returns the path of the written report file."
  (let* ((report-file (expand-file-name
                       (format "seed7mode-indent-perf-%s.rst" id)
                       report-dir))
         (timestamp   (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (errors      (cl-remove-if-not (lambda (r) (plist-get r :error)) results))
         (ok-results  (cl-remove-if (lambda (r) (plist-get r :error)) results))
         (times       (mapcar (lambda (r) (plist-get r :indent-time)) ok-results))
         (n           (length results))
         (n-ok        (length ok-results))
         (avg         (if (> n-ok 0) (/ (apply #'+ times) (float n-ok)) 0.0))
         (mn          (if (> n-ok 0) (apply #'min times) 0.0))
         (mx          (if (> n-ok 0) (apply #'max times) 0.0))
         (total-time  (apply #'+ times))
         (fw          sd7-indent-perf--col-file)
         (lw          sd7-indent-perf--col-lines)
         (tw          sd7-indent-perf--col-time)
         (rule        (sd7-indent-perf--rst-rule)))
    (make-directory report-dir t)
    (with-temp-file report-file
      ;; Title
      (let* ((title "Seed7 Mode — Indentation Performance & Correctness Benchmark")
             (bar   (make-string (length title) ?=)))
        (insert bar "\n" title "\n" bar "\n\n"))
      ;; Metadata
      (insert (format ":Running with    : seed7-mode %s\n"
                      seed7-mode-version-timestamp))
      (insert (format ":Input dir       : %s\n"
                      (file-name-as-directory (expand-file-name input-dir))))
      (insert (format ":Output dir      : %s\n"
                      (file-name-as-directory (expand-file-name output-dir))))
      (insert (format ":Generated on    : %s\n" timestamp))
      (insert (format ":Files processed : %d\n" n))
      (insert (format ":Files succeeded : %d\n" n-ok))
      (when errors
        (insert (format ":Files with errors: %d\n" (length errors))))
      (insert (format ":Total indent time: %.3f s\n" total-time))
      (insert (format ":Wall-clock time   : %s\n\n"
                      (sd7-indent-perf--format-duration total-elapsed)))
      ;; Correctness note
      (insert "Correctness Check\n")
      (insert "=================\n\n")
      (insert "To check whether re-indentation changed any file, run::\n\n")
      (insert (format "  diff -r %s \\\n          %s\n\n"
                      (file-name-as-directory (expand-file-name input-dir))
                      (file-name-as-directory (expand-file-name output-dir))))
      (insert "No output from ``diff -r`` means seed7-mode indentation is idempotent\n")
      (insert "for all files in the input tree.\n\n")
      (insert "To compare two OUTPUT-DIRs produced by different versions of seed7-mode::\n\n")
      (insert "  diff -r OUTPUT_DIR_BEFORE OUTPUT_DIR_AFTER\n\n")
      (insert "Any line in the diff output indicates a change in indentation decisions.\n\n")
      ;; File table
      (insert "File Indentation Times\n")
      (insert "======================\n\n")
      (insert rule "\n")
      (insert (format (format "%%-%ds %%%dd %%%ds\n" fw lw tw)
                      "File Name" (length "Lines") "Indent Time (s)"))
      (insert rule "\n")
      (dolist (r results)
        (let ((file  (plist-get r :file))
              (lines (plist-get r :lines))
              (time  (plist-get r :indent-time))
              (err   (plist-get r :error)))
          (if err
              (insert (format (format "%%-%ds %%%ds %%s\n" fw lw)
                              file "—" (format "ERROR: %s" err)))
            (insert (format (format "%%-%ds %%%dd %%.6f\n" fw lw)
                            file lines time)))))
      (insert rule "\n\n")
      ;; Statistics
      (insert "Statistical Summary\n")
      (insert "===================\n\n")
      (insert "Per-file `indent-region' times (seconds).\n\n")
      (insert "+-----------+------------------+\n")
      (insert "| Metric    | Time (s)         |\n")
      (insert "+===========+==================+\n")
      (insert (format "| Files     | %-16d |\n" n))
      (insert "+-----------+------------------+\n")
      (insert (format "| Average   | %-16.6f |\n" avg))
      (insert "+-----------+------------------+\n")
      (insert (format "| Minimum   | %-16.6f |\n" mn))
      (insert "+-----------+------------------+\n")
      (insert (format "| Maximum   | %-16.6f |\n" mx))
      (insert "+-----------+------------------+\n")
      (insert (format "| Total     | %-16.6f |\n" total-time))
      (insert "+-----------+------------------+\n\n")
      ;; Error table (only if there were errors)
      (when errors
        (insert "Files with Errors\n")
        (insert "=================\n\n")
        (dolist (r errors)
          (insert (format "- ``%s``: %s\n"
                          (plist-get r :file)
                          (plist-get r :error))))
        (insert "\n"))

      ;; ---- Indentation warnings (files with "not yet supported" messages) ----
      (let ((warn-results (cl-remove-if-not
                           (lambda (r) (plist-get r :warnings))
                           results)))
        (when warn-results
          (insert "Indentation Warnings\n")
          (insert "====================\n\n")
          (insert "Lines where seed7-mode reported \"not yet supported\" or \"timed out\"\n")
          (insert "during indentation.  \"not yet supported\" indicates a Seed7 syntax\n")
          (insert "construct not yet handled by the indentation engine.  \"timed out\"\n")
          (insert "indicates a catastrophic search in `seed7-re-search-backward' or\n")
          (insert "`seed7--to-top' — see the fix proposals in the PR comments.\n\n")
          (dolist (r warn-results)
            (insert (format "``%s``\n" (plist-get r :file)))
            (dolist (w (plist-get r :warnings))
              (insert (format "  - %s\n" w)))
            (insert "\n"))))

      ;; Terminator
      (insert "\n.. ---------------------------------------------------------------------------\n"))
    report-file))

;;; --------------------------------------------------------------------------
;;; Core runner

(defun sd7-indent-perf-run-core (input-dir output-dir report-dir id)
  "Re-indent all Seed7 files under INPUT-DIR and write a benchmark report.

INPUT-DIR  : root of the directory tree to scan (never modified).
OUTPUT-DIR : root of the output tree; re-indented files are written here
             with the same relative paths as in INPUT-DIR.
REPORT-DIR : directory where the RST report is written (created if absent).
ID         : free-form report identifier used verbatim in the filename.

Returns the absolute path of the written report file."
  (setq input-dir  (expand-file-name input-dir)
        output-dir (expand-file-name output-dir)
        report-dir (if (file-name-absolute-p report-dir)
                       (expand-file-name report-dir)
                     (expand-file-name report-dir
                                       (or sd7-indent-perf--repo-root
                                           default-directory))))
  (unless (file-directory-p input-dir)
    (user-error "Not a directory: %s" input-dir))
  (when (file-equal-p input-dir output-dir)
    (user-error "OUTPUT-DIR must be different from INPUT-DIR"))
  (when (file-in-directory-p output-dir input-dir)
    (user-error "OUTPUT-DIR must not be inside INPUT-DIR: %s" output-dir))
  (unless (and (stringp id) (not (string-empty-p id)))
    (user-error "ID must be a non-empty string, got: %S" id))

  (sd7-indent-perf--message
   "Seed7 Mode — Indentation Performance & Correctness Benchmark")
  (sd7-indent-perf--message "seed7-mode: %s" seed7-mode-version-timestamp)
  (sd7-indent-perf--message "Input dir : %s" input-dir)
  (sd7-indent-perf--message "Output dir: %s" output-dir)
  (sd7-indent-perf--message "Report dir: %s" report-dir)
  (sd7-indent-perf--message "Report ID : %s" id)

  (let* ((files       (sd7-indent-perf--seed7-files input-dir))
         (total-files (length files))
         (_ (sd7-indent-perf--message "Files found: %d" total-files))
         (total-start (float-time))
         (results     '())
         (n           0))
    (dolist (f files)
      (cl-incf n)
      ;; Announce file number + wall-clock start time before entering the
      ;; (potentially long) per-file indentation.
      (sd7-indent-perf--message
       "[%d/%d]  started %s"
       n total-files
       (format-time-string "%H:%M:%S"))
      (push (sd7-indent-perf--process-file f input-dir output-dir)
            results))

    (let* ((total-elapsed (- (float-time) total-start))
           (results-fwd   (nreverse results))
           (report        (sd7-indent-perf--write-report
                           results-fwd input-dir output-dir
                           report-dir id total-elapsed)))
      (sd7-indent-perf--message
       "Done.  Wall clock: %s"
       (sd7-indent-perf--format-duration total-elapsed))
      (sd7-indent-perf--message "Report: %s" report)
      report)))

;;; --------------------------------------------------------------------------
;;; Interactive entry point

;;;###autoload
(defun sd7-indent-perf-run (input-dir output-dir report-dir id)
  "Interactively run the Seed7 indentation benchmark.

Prompts for INPUT-DIR, OUTPUT-DIR, REPORT-DIR, and ID.
Previous values are offered as defaults and remembered within the session.

OUTPUT-DIR will mirror INPUT-DIR with re-indented file content; it is
created if absent.  The original files in INPUT-DIR are never modified.

The RST report is written to REPORT-DIR/seed7mode-indent-perf-ID.rst."
  (interactive
   (list
    (read-directory-name "Input directory (Seed7 source tree): "
                         sd7-indent-perf--last-input-dir)
    (read-directory-name "Output directory (re-indented tree): "
                         sd7-indent-perf--last-output-dir)
    (read-directory-name "Report directory: "
                         sd7-indent-perf--last-report-dir)
    (read-string (format "Report ID (default %S): "
                         sd7-indent-perf--last-id)
                 nil nil sd7-indent-perf--last-id)))
  (setq sd7-indent-perf--last-input-dir  (directory-file-name
                                          (expand-file-name input-dir))
        sd7-indent-perf--last-output-dir (directory-file-name
                                          (expand-file-name output-dir))
        sd7-indent-perf--last-report-dir (directory-file-name
                                          (expand-file-name report-dir))
        sd7-indent-perf--last-id         id)
  ;; Show the live progress buffer in another window before starting.
  (with-current-buffer (get-buffer-create sd7-indent-perf--progress-buffer)
    (erase-buffer))
  (display-buffer sd7-indent-perf--progress-buffer
                  '((display-buffer-pop-up-window) (inhibit-same-window . t)))
  (sd7-indent-perf-run-core input-dir output-dir report-dir id))

;;; --------------------------------------------------------------------------
;;; Batch / CLI entry point

(defun sd7-indent-perf-main ()
  "Parse `command-line-args-left' and call `sd7-indent-perf-run-core'.

Expected positional arguments (after --)::

  INPUT_DIR OUTPUT_DIR REPORT_DIR ID

Exits Emacs with 0 on success or 1 on error."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (when (equal (car args) "--")
      (setq args (cdr args)))
    (when (< (length args) 4)
      (message "Usage: -- INPUT_DIR OUTPUT_DIR REPORT_DIR ID")
      (kill-emacs 1))
    (condition-case err
        (progn
          (sd7-indent-perf-run-core
           (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args))
          (kill-emacs 0))
      (error
       (message "ERROR: %s" (error-message-string err))
       (kill-emacs 1)))))

;;; --------------------------------------------------------------------------
;;; Auto-dispatch in batch mode

(when noninteractive
  (sd7-indent-perf-main))

;;; --------------------------------------------------------------------------
(provide 'sd7-indent-perf)

;;; sd7-indent-perf.el ends here
