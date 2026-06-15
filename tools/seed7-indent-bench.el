;;; seed7-indent-bench.el --- Benchmark seed7-mode indentation on Seed7 files  -*- lexical-binding: t; -*-

;; Created   : ...
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-15 12:21:31 EDT, updated by Pierre Rouleau>

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
;; Developer tool to measure the time taken to re-indent every line of one or
;; more Seed7 source files using `seed7-complete-statement-or-indent'.
;;
;; For each file the tool:
;;
;;  1. Opens the file in a buffer with `seed7-mode' active.
;;  2. Resets the timing counters (`seed7-timing-reset').
;;  3. Starts the timing harness (`seed7-timing-start').
;;  4. Walks every line from top to bottom:
;;       a. Inserts a leading space  (forces a dirty indentation state).
;;       b. Deletes all leading whitespace (sets the line to column 0).
;;       c. Calls `seed7-complete-statement-or-indent' to re-indent.
;;  5. Prints the timing report (`seed7-timing-report'):
;;       calls | total time | mean ms | min ms | max ms
;;  6. Stops the timing harness (`seed7-timing-stop').
;;  7. Saves the buffer back to the file.
;;
;; The content of each file should be unchanged after the run (indentation
;; differences would indicate a bug in the indentation engine).
;;
;; Batch usage
;; ===========
;;
;;   emacs --batch \
;;     -l /path/to/seed7-mode.el \
;;     -l /path/to/tools/seed7-mode-time.el \
;;     -l /path/to/tools/seed7-indent-bench.el \
;;     -- file1.s7d file2.s7i ...
;;
;; Interactive usage
;; =================
;;
;;   M-x seed7-indent-bench-file   (prompts for a single file)

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
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
(require 'seed7-mode-time)

;;; --------------------------------------------------------------------------
;;; Code:


(defun seed7-indent-bench-buffer ()
  "Benchmark re-indentation of every line in the current buffer.

Resets and starts the timing harness, then for each line:
  - inserts a leading space to force a dirty indentation state,
  - removes all leading whitespace so the line starts at column 0,
  - calls `seed7-complete-statement-or-indent' to re-indent the line.
Prints a timing report at the end and stops the harness."
  (seed7-mode)
  (seed7-timing-start)
  (message "===============================")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; (a) Force a non-zero indentation state.
      (beginning-of-line)
      (insert " ")
      ;; (b) Remove all leading whitespace — line is now at column 0.
      (beginning-of-line)
      (delete-region (point)
                     (progn (skip-chars-forward " \t") (point)))
      ;; (c) Re-indent using the standard seed7 command.
      (seed7-complete-statement-or-indent)
      (forward-line 1)))
  (seed7-timing-report)
  (message "===============================")
  (seed7-timing-stop)
  (seed7-timing-reset))


(defun seed7-indent-bench-file (filename)
  "Benchmark re-indentation of every line in FILENAME.

Opens FILENAME (which must be a Seed7 source file), runs
`seed7-indent-bench-buffer', saves the buffer, and then kills it.
Use this interactively or call it from `seed7-indent-bench-files'."
  (interactive "fSeed7 file to benchmark: ")
  (message "")
  (message "=== seed7-indent-bench: %s ===" (abbreviate-file-name filename))
  (let* ((existing-buf (get-file-buffer filename))
         (buf (or existing-buf (find-file-noselect filename))))
    (unwind-protect
        (with-current-buffer buf
          (seed7-indent-bench-buffer)
          (save-buffer))
      (unless existing-buf
        (kill-buffer buf)))))

(defun seed7-indent-bench-files (files)
  "Benchmark re-indentation for each file path in the list FILES.

Skips files that are not readable and prints a warning for each."
  (if (null files)
      (message "seed7-indent-bench: no files specified.")
    (dolist (f files)
      (if (file-readable-p f)
          (seed7-indent-bench-file f)
        (message "WARNING: seed7-indent-bench: cannot read \"%s\", skipping." f)))))


;;; --------------------------------------------------------------------------
;;; Batch-mode entry point
;;
;; When this file is loaded with `--batch', the Seed7 file paths are taken
;; from the remaining command-line arguments (those after `--').

(when noninteractive
  (seed7-indent-bench-files command-line-args-left)
  ;; Consume the arguments so Emacs does not try to visit them again.
  (setq command-line-args-left nil))

;;; --------------------------------------------------------------------------
(provide 'seed7-indent-bench)

;;; seed7-indent-bench.el ends here
