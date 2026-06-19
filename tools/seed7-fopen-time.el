;;; seed7-fopen-time.el --- Measure stats of time to open Seed7 files  -*- lexical-binding: t; -*-

;; Created   : Friday, June 19 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-19 15:13:49 EDT, updated by Pierre Rouleau>

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
;;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'seed7-mode)
;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun benchmark-sd7-files-in-dir (directory extension)
  "Return a sorted list of files with EXTENSION inside the DIRECTORY."
  (directory-files directory
                   t
                   (format "\\.%s$" extension)))

(defun benchmark-sd7-files-in-specs (directory-specs)
  "Benchmark open/render time of all file-names identified in DIRECTORY-SPECS.

The DIRECTORY-SPECS is a list of (DIRECTORY EXTENSIONS) elements, where:
- DIRECTORY is a string; the name of the directory
- EXTENSIONS is a list of strings, each being a file-name extension with no
  leading period like \"sd7\" or \"s7i\".

Returns a (REPORT. MAX-FILENAME-LEN) where:
- REPORT is a list (FILE-NAME FILE-LINE-COUNT TOTAL-TIME NUM-GC GC-TIME),
  where the last 3 elements are returned by `benchmark-run`,
- MAX-FILENAME-LEN is the length of largest file name found.
"
  (let ((report nil)
        (max-filename-len 0)
        (abbrev-fname nil)
        (len nil)
        (file-line-count))
    (dolist (dir-spec directory-specs)
      (let* ((directory (car dir-spec))
             (file-names
              (mapcan (lambda (extension)
                        (benchmark-sd7-files-in-dir directory extension))
                      (cadr dir-spec))))
        (dolist (file-name file-names)
          (let ((bench-info
                 (benchmark-run 1
                   (let* ((existing-buf (get-file-buffer file-name))
                          (buf (or existing-buf (find-file-noselect file-name))))
                     (with-current-buffer buf
                       (sit-for 0)
                       (setq file-line-count (line-number-at-pos (point-max))))
                     ;; Clean up buffer to prevent memory issues, but only if
                     ;; it was not previously opened.
                     (unless existing-buf
                       (kill-buffer buf))))))
            (setq abbrev-fname (abbreviate-file-name file-name))
            (setq len (length abbrev-fname))
            (when (> len  max-filename-len)
              (setq max-filename-len len))
            ;; Add pair to report: (filename . (total-time num-gc gc-time))
            (push (cons abbrev-fname
                        (cons file-line-count bench-info))
                  report)))))
    ;; Return the result reversed so it matches directory order
    (cons (nreverse report) max-filename-len)))


(defun generate-sd7-benchmark-report (directory-specs)
  "Run time benchmark on all files listed in DIRECTORY-SPECS and produce a report.

The DIRECTORY-SPECS is a list of (DIRECTORY EXTENSIONS) elements, where:
- DIRECTORY is a string; the name of the directory
- EXTENSIONS is a list of strings, each being a file extension with no
  leading period like \"sd7\" or \"s7i\"."
  (let* ((results.max-fname-len (benchmark-sd7-files-in-specs directory-specs))
         (results       (car results.max-fname-len))
         (max-fname-len (cdr results.max-fname-len))
         (count         (length results))
         (title-bar     (make-string max-fname-len ?=))
         (spacing       (make-string (max 1 (- max-fname-len (length "File Name"))) ?\s)))
    (if (null results)
        (message "No .sd7 files found in that directory.")
      (let* ((times (mapcar #'caddr results))
             (gc-counts (mapcar #'cadddr results))
             (gc-times  (mapcar (apply-partially #'nth 4) results))
             ;; Statistical calculations
             (sum-time (apply #'+ times))
             (avg-time (/ sum-time count))
             (min-time (apply #'min times))
             (max-time (apply #'max times))

             (sum-gc-time (apply #'+ gc-times))
             (avg-gc-time (/ sum-gc-time count))
             (min-gc-time (apply #'min gc-times))
             (max-gc-time (apply #'max gc-times))

             (sum-gc-cnt (apply #'+ gc-counts))
             (avg-gc-cnt (/ (float sum-gc-cnt) count))
             (min-gc-cnt (apply #'min gc-counts))
             (max-gc-cnt (apply #'max gc-counts))

             ;; Setup for distribution histogram (4 bins)
             (range (- max-time min-time))
             (bin-width (if (zerop range) 0.001 (/ range 4.0)))
             (bin1 0) (bin2 0) (bin3 0) (bin4 0)

             ;; Output Buffer
             (report-buf (get-buffer-create "*sd7-benchmark-report*")))

        ;; Distribute times into bins natively using setq
        (dolist (t-val times)
          (cond
           ((= t-val max-time) (setq bin4 (1+ bin4))) ;; Edge case for absolute max
           ((< t-val (+ min-time bin-width)) (setq bin1 (1+ bin1)))
           ((< t-val (+ min-time (* 2 bin-width))) (setq bin2 (1+ bin2)))
           ((< t-val (+ min-time (* 3 bin-width))) (setq bin3 (1+ bin3)))
           (t (setq bin4 (1+ bin4)))))

        (with-current-buffer report-buf
          (setq buffer-read-only nil)
          (erase-buffer)

          ;; Document Title
          (insert "========================================\n")
          (insert "Benchmark Report for opening Seed7 Files\n")
          (insert "========================================\n\n")
          (insert ":Generated by: tools/seed7-fopen-time\n")
          (insert (format ":Running with: seed7-mode %s\n"
                          seed7-mode-version-timestamp))
          (insert (format ":Generated on: %s\n\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S UTC" (current-time) t)))

          ;; 1. File and Time Specification Table
          (insert "File Load Times\n")
          (insert "===============\n\n")
          (insert (format  "%s= ================= =============== ================= ================\n" title-bar))
          (insert (format  "File Name%s  Total Time (s)    GC Count        GC Time (s)       Line count\n" spacing))
          (insert (format  "%s= ================= =============== ================= ================\n" title-bar))
          (dolist (row results)
            (insert (format (format "%%-%ds  %%-15.6f   %%-13d   %%-15.6f   %%5d\n" max-fname-len)
                            (nth 0 row)
                            (nth 2 row)
                            (nth 3 row)
                            (nth 4 row)
                            (nth 1 row))))
          (insert (format  "%s= ================= =============== ================= ================\n" title-bar))

          (insert "\n\n")

          ;; 2. Statistical Analysis Component
          (insert "Statistical Summary\n")
          (insert "===================\n\n")
          (insert "+-----------+-----------------+---------------+-----------------+\n")
          (insert "| Metric    | Total Time (s)  | GC Count      | GC Time (s)     |\n")
          (insert "+===========+=================+===============+=================+\n")
          (insert (format "| Average   | %-15.6f | %-13.2f | %-15.6f |\n" avg-time avg-gc-cnt avg-gc-time))
          (insert "+-----------+-----------------+---------------+-----------------+\n")
          (insert (format "| Minimum   | %-15.6f | %-13d | %-15.6f |\n" min-time min-gc-cnt min-gc-time))
          (insert "+-----------+-----------------+---------------+-----------------+\n")
          (insert (format "| Maximum   | %-15.6f | %-13d | %-15.6f |\n" max-time max-gc-cnt max-gc-time))
          (insert "+-----------+-----------------+---------------+-----------------+\n\n\n")

          ;; 3. Distribution Visualization
          (insert "Time Distribution (Histogram)\n")
          (insert "=============================\n\n")
          (insert "::\n\n") ;; Literal block indicator for rendering text histograms cleanly
          (if (zerop range)
              (insert (format "  %.4fs - %.4fs :%d\n"
                              min-time max-time  count))
            (insert (format "  %.4fs - %.4fs : %d\n" min-time (+ min-time bin-width) bin1))
            (insert (format "  %.4fs - %.4fs : %d\n" (+ min-time bin-width) (+ min-time (* 2 bin-width)) bin2))
            (insert (format "  %.4fs - %.4fs : %d\n" (+ min-time (* 2 bin-width)) (+ min-time (* 3 bin-width)) bin3))
            (insert (format "  %.4fs - %.4fs : %d\n" (+ min-time (* 3 bin-width)) max-time bin4)))
          (rst-mode)
          (goto-char (point-min)))
        (switch-to-buffer report-buf)))))


;;; --------------------------------------------------------------------------
(provide 'seed7-fopen-time)

;;; seed7-fopen-time.el ends here
