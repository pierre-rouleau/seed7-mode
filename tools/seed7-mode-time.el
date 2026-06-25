;;; seed7-mode-time.el --- Measure execution time of seed7-mode  -*- lexical-binding: t; -*-

;; Created   : Wednesday, June  3 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 12:08:16 EDT, updated by Pierre Rouleau>

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
;;  This file is a developer tool used to measure execution time of the
;;  seed7-mode code.  It is not meant to be used normally as it injects code
;;  in the system.
;;
;; Timing harness for seed7-calc-indent:
;;
;;   - Activate:   M-x seed7-timing-start
;;   - Deactivate: M-x seed7-timing-stop
;;   - Report:     M-x seed7-timing-report
;;   - Reset:      M-x seed7-timing-reset
;;
;; Simple Workflow
;; ===============
;;
;; - Open a Seed7 file
;; - Open this file and byte compile it
;; - Change to the buffer containing the Seed7  file.
;;
;; - M-x seed7-timing-start
;; - edit a representative file for a while (press RET, TAB, etc.),
;; - M-x seed7-timing-report.
;;
;;
;; Micro Benchmark
;; ===============
;;
;; - Visit a Seed7 file buffer, position to the location to indent.
;; - M-x seed7-benchmark-calc-indent

;; Using The Emacs Profiler
;; ========================
;;
;; - Start profiling:
;;
;;   - execute: M-:  (profiler-start 'cpu)
;;   - or:      M-x profile-start cpu RET
;;
;;
;; - ... edit the .s7d or .s7i file for 30-60 seconds, press RET many times ...
;;
;; - Stop and display:
;;
;;    - M-x profiler-stop
;;    - M-x profiler-report

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'seed7-mode)
(require 'cl-lib)   ; use: `cl-incf'

;;; --------------------------------------------------------------------------
;;; Code:
;;


(defvar seed7--timing-calls 0 "Number of calls timed.")
(defvar seed7--timing-total 0.0 "Total elapsed time in seconds.")
(defvar seed7--timing-min most-positive-fixnum "Minimum call time.")
(defvar seed7--timing-max 0.0 "Maximum call time.")

(defun seed7--timing-advice (orig-fn &rest args)
  "Time one call to `seed7-calc-indent'."
  (let* ((t0 (float-time))
         (result (apply orig-fn args))
         (elapsed (- (float-time) t0)))
    (cl-incf seed7--timing-calls)
    (cl-incf seed7--timing-total elapsed)
    (when (< elapsed seed7--timing-min)
      (setq seed7--timing-min elapsed))
    (when (> elapsed seed7--timing-max)
      (setq seed7--timing-max elapsed))
    result))

(defun seed7-timing-start ()
  "Start timing seed7-calc-indent calls."
  (interactive)
  (advice-add 'seed7-calc-indent :around #'seed7--timing-advice)
  (seed7-timing-reset)
  (message "seed7-calc-indent timing started."))

(defun seed7-timing-stop ()
  "Stop timing seed7-calc-indent calls."
  (interactive)
  (advice-remove 'seed7-calc-indent #'seed7--timing-advice)
  (message "seed7-calc-indent timing stopped."))

(defun seed7-timing-reset ()
  "Reset timing counters."
  (interactive)
  (setq seed7--timing-calls 0
        seed7--timing-total 0.0
        seed7--timing-min most-positive-fixnum
        seed7--timing-max 0.0)
  (message "seed7-calc-indent timing counters reset."))

(defun seed7-timing-report ()
  "Display timing report for seed7-calc-indent."
  (interactive)
  (if (zerop seed7--timing-calls)
      (message "No calls recorded yet.")
    (message
     "seed7-calc-indent: %d calls | total=%.4fs | mean=%.4fms | min=%.4fms | max=%.4fms"
     seed7--timing-calls
     seed7--timing-total
     (* 1000.0 (/ seed7--timing-total seed7--timing-calls))
     (* 1000.0 seed7--timing-min)
     (* 1000.0 seed7--timing-max))))


;; ---------------------------------------------------------------------------
;;; Adjust N to taste (1000 is usually enough for stable numbers).

(defun seed7-benchmark-calc-indent (n)
  "Benchmark N calls to `seed7-calc-indent' at point."
  (interactive "nNumber of iterations: ")
  (require 'benchmark)
  (let* ((result (benchmark-run n (seed7-calc-indent)))
         (total  (car result))
         (gc     (nth 1 result))
         (gc-t   (nth 2 result)))
    (message "seed7-calc-indent x%d: %.4fs total (%.4fms/call) | GC: %d runs, %.4fs"
             n total (* 1000.0 (/ total n)) gc gc-t)))


;;; --------------------------------------------------------------------------
(provide 'seed7-mode-time)

;;; seed7-mode-time.el ends here
