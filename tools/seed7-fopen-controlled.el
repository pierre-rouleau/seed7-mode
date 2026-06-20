;;; seed7-fopen-controlled.el --- GC-controlled file-open benchmark  -*- lexical-binding: t; -*-

;; Created   : Friday, June 19 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-20 12:21:09 EDT, updated by Pierre Rouleau>

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
;; Commentary:
;;
;; This file is a tool used to measure the performance of the seed7-mode
;; file rendering.  It is a stricter harness than `seed7-fopen-time' that
;; isolates CPU costs by:
;;
;;  1. Warm-up pass  : every file opened once (untimed) so font-lock
;;                     patterns, regexp JIT, and mode hooks are hot.
;;  2. GC suppression: `gc-cons-threshold' is set to `most-positive-fixnum'
;;                     for the timed pass so no GC pause contaminates results.
;;  3. N iterations  : each file is opened N times; the mean is reported.
;;
;;
;; How to run it:
;;
;; Copy the following in *scratch*, after loading seed7-mode:
;;
;;   (add-to-list 'load-path "/path/to/seed7-mode/tools")
;;   (require 'seed7-fopen-time)
;;   (require 'seed7-fopen-controlled)
;;
;;   (generate-sd7-controlled-report
;;    '(("PATH_TO/seed7/prg" ("sd7"))   ; change PATH_TO
;;      ("PATH_TO/seed7/lib" ("s7i")))  ; change PATH_TO
;;    5)    ; ← 5 iterations per file

;; The report lands in *sd7-controlled-benchmark*.

;;; --------------------------------------------------------------------------
;;; Dependencies:

;; Note: this file is *not* located in Emacs load-path on purpose: it's
;;       a tool meant only for special tests.  Therefore, to allow
;;       compilation interactively the following special code is required
;;       to explicitly make it accessible.
;;
;; Allow byte-compiling this file from an interactive Emacs session
;; (e.g. M-x byte-compile-file) without needing the directories to be
;; pre-configured in `load-path'.
;;
;; Variable availability by context:
;;   `load-file-name'           – set when a file is loaded via `load'/`require'
;;   `byte-compile-current-file'– set by the byte compiler during batch/interactive
;;                                byte compilation (the missing case in the previous fix)
;;   `buffer-file-name'         – fallback for an interactively visited buffer
;;
;; The `when this-file' guard makes the block a safe no-op if none of the
;; three variables is bound (should not happen in practice, but defensive).
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
(require 'seed7-fopen-time)   ; use: `benchmark-sd7-files-in-dir'

;; `mapcan' and `caddr' were introduced in Emacs 26,
;; but the `cl-mapcan' and `cl-caddr' were available then
;; and were provided by cl-lib.
(require 'cl-lib)                       ; use `cl-mapcan', `cl-caddr'


;;; --------------------------------------------------------------------------
;;; Code:

(defun sd7-controlled--open-file (file-name)
  "Open FILE-NAME in a buffer, activate the mode, then kill the buffer.
Returns the number of lines in the file."
  (let* ((existing-buf (get-file-buffer file-name))
         (buf (or existing-buf (find-file-noselect file-name)))
         line-count)
    (with-current-buffer buf
      (sit-for 0)
      (setq line-count (line-number-at-pos (point-max))))
    (unless existing-buf
      (kill-buffer buf))
    line-count))


(defun sd7-controlled--warmup (directory-specs)
  "Open every file in DIRECTORY-SPECS once (untimed), then GC."
  (message "sd7-controlled: warm-up pass …")
  (dolist (dir-spec directory-specs)
    (let* ((directory  (car dir-spec))
           (file-names (cl-mapcan (lambda (ext)
                                    (benchmark-sd7-files-in-dir directory ext))
                                  (cadr dir-spec))))
      (dolist (file-name file-names)
        (sd7-controlled--open-file file-name))))
  (garbage-collect)
  (message "sd7-controlled: warm-up done, heap GC'd to baseline."))


(defun sd7-controlled--timed-pass (directory-specs iterations)
  "Open every file ITERATIONS times with GC suppressed.
Returns (report . max-fname-len)."
  (let ((old-threshold  gc-cons-threshold)
        (old-percentage gc-cons-percentage)
        report
        (max-fname-len 0))
    ;; ── Suppress GC for the entire timed pass ──────────────────────────────
    (setq gc-cons-threshold  most-positive-fixnum
          gc-cons-percentage 1.0)
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
                ;; ── N timed repetitions ─────────────────────────────────────
                (dotimes (_ iterations)
                  (let ((info (benchmark-run 1
                                (setq line-count
                                      (sd7-controlled--open-file file-name)))))
                    ;; info = (elapsed-time gc-runs gc-time)
                    (push (car info) times)))
                (let ((mean (/ (apply #'+ times) (float iterations))))
                  (push (list abbrev line-count mean) report))))))
      ;; ── Always restore GC settings ─────────────────────────────────────────
      (setq gc-cons-threshold  old-threshold
            gc-cons-percentage old-percentage))
    (cons (nreverse report) max-fname-len)))


(defun generate-sd7-controlled-report (directory-specs &optional iterations)
  "Run a GC-controlled benchmark and display the report.

DIRECTORY-SPECS: same format as `generate-sd7-benchmark-report'.
ITERATIONS: how many timed opens per file (default 3).

Sequence:
  1. Warm-up pass  (all files opened once, then GC).
  2. Timed pass    (GC suppressed, ITERATIONS opens per file, mean taken).
  3. Report displayed in *sd7-controlled-benchmark*."
  (interactive
   (list (read--expression "Directory specs: ")
         (read-number "Iterations per file: " 3)))
  (let* ((iters (or iterations 3)))
    ;; 1. Warm up
    (sd7-controlled--warmup directory-specs)
    ;; 2. Timed pass
    (message "sd7-controlled: timed pass (%d iterations per file) …" iters)
    (let* ((result.len (sd7-controlled--timed-pass directory-specs iters))
           (results    (car result.len))
           (max-len    (cdr result.len))
           (count      (length results)))
      (when (zerop count)
        (user-error "No matching files found in directory-specs argument"))
      (let* ((times      (mapcar #'cl-caddr results))
             (sum        (apply #'+ times))
             (avg        (/ sum count))
             (mn         (apply #'min times))
             (mx         (apply #'max times))
             (title-bar  (make-string max-len ?=))
             (spacing    (make-string (max 1 (- max-len (length "File Name"))) ?\s))
             (buf        (get-buffer-create "*sd7-controlled-benchmark*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "================================================\n")
          (insert "GC-Controlled Benchmark Report: Seed7 File Open\n")
          (insert "================================================\n\n")
          (insert (format ":Running with: seed7-mode %s\n"
                          seed7-mode-version-timestamp))
          (insert (format ":Generated on: %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S UTC"
                                              (current-time) t)))
          (insert (format ":Iterations  : %d  (mean of N timed opens per file)\n"
                          iters))
          (insert ":GC during measure: suppressed (gc-cons-threshold = most-positive-fixnum)\n")
          (insert ":Warm-up: yes (1 untimed pass + garbage-collect before timing)\n\n")

          ;; Table
          (insert "File Load Times (mean, GC-free)\n")
          (insert "================================\n\n")
          (insert (format "%s= ================= ================\n" title-bar))
          (insert (format "File Name%s  Mean Time (s)     Line count\n" spacing))
          (insert (format "%s= ================= ================\n" title-bar))
          (dolist (row results)
            (insert (format (format "%%-%ds  %%-15.6f   %%5d\n" max-len)
                            (nth 0 row) (nth 2 row) (nth 1 row))))
          (insert (format "%s= ================= ================\n\n" title-bar))

          ;; Summary
          (insert "Statistical Summary (GC-free, mean-of-N)\n")
          (insert "=========================================\n\n")
          (insert "+-----------+-----------------+\n")
          (insert "| Metric    | Mean Time (s)   |\n")
          (insert "+===========+=================+\n")
          (insert (format "| Average   | %-15.6f |\n" avg))
          (insert "+-----------+-----------------+\n")
          (insert (format "| Minimum   | %-15.6f |\n" mn))
          (insert "+-----------+-----------------+\n")
          (insert (format "| Maximum   | %-15.6f |\n" mx))
          (insert "+-----------+-----------------+\n"))
        (switch-to-buffer buf)
        (message "sd7-controlled: report ready (%d files, %d iters each)."
                 count iters)))))

;; ---------------------------------------------------------------------------
(provide 'seed7-fopen-controlled)

;;; seed7-fopen-controlled.el ends here
