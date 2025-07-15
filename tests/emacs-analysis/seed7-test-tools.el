;;; seed7-test-tools.el --- Test tool to test Emacs navigation code.  -*- lexical-binding: t; -*-

;; Created   : Monday, July 14 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-07-14 23:10:57 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7 package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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

(require 'python)                       ; use `python-nav-forward-sexp',
                                        ; `python-nav-end-of-defun',
                                        ; `python-nav-beginning-of-defun'


(require 'seed7-mode)                   ; use `seed7-current-line-number'
;; lisp: use `forward-sexp', `end-of-defun'

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defun test--forward-sexp (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (forward-sexp 1)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test--end-of-defun (&optional stay-on-line)
  "Test end-of-defun "
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (end-of-defun)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test--beginning-of-defun (&optional stay-on-line)
  "Test beginning-of-defun"
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (beginning-of-defun)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
     (forward-line 1)))

(defun test-all-elisp ()
  "Test all functions over all lines of current buffer."
  (interactive)

  (message "Testing (forward-sexp 1)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--forward-sexp))

  (message "\nTesting (end-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--end-of-defun))

  (message "\nTesting (beginning-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--beginning-of-defun)))


;; ---------------------------------------------------------------------------

(defun test--python-nav-forward-sexp (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (python-nav-forward-sexp 1)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test--python-nav-end-of-defun (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (python-nav-end-of-defun)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test--python-nav-beginning-of-defun (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (python-nav-beginning-of-defun 1)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))


(defun test-all-python ()
  "Test all functions over all lines of current buffer."
  (interactive)

  (message "Testing (python-nav-forward-sexp 1)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--python-nav-forward-sexp))

  (message "\nTesting (python-nav-end-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--python-nav-end-of-defun))

  (message "\nTesting (end-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--end-of-defun))

  (message "\nTesting (python-nav-beginning-of-defun 1)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--python-nav-beginning-of-defun))

  (message "\nTesting (beginning-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--beginning-of-defun)))

;; ---------------------------------------------------------------------------

(defun test--seed7-nav-end-of-defun (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (seed7-nav-end-of-defun)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test--seed7-nav-beginning-of-defun (&optional stay-on-line)
  "Test forward-sexp 1"
  (interactive)
  (save-excursion
    (message "   %3d, %2d, %2d" (point) (seed7-current-line-number) (current-column))
    (seed7-nav-beginning-of-defun 1)
    (message "-> %3d, %2d, %2d" (point) (seed7-current-line-number)
             (current-column)))
  (unless stay-on-line
   (forward-line 1)))

(defun test-all-seed7 ()
  "Test all functions over all lines of current buffer."
  (interactive)

  (message "\nTesting (seed7-nav-end-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--seed7-nav-end-of-defun))

  (message "\nTesting (end-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--end-of-defun))

  (message "\nTesting (seed7-nav-beginning-of-defun 1)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--seed7-nav-beginning-of-defun))

  (message "\nTesting (beginning-of-defun)")
  (goto-char (point-min))
  (while (not (eobp))
    (test--beginning-of-defun)))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-tools)

;;; seed7-test-tools.el ends here
