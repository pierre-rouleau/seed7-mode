;;; seed7-test-sets-01.el --- ERT-based test of Seed7 set handling.  -*- lexical-binding: t; -*-

;; Created   : Friday, July 11 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-17 10:49:52 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7 package.
;; This file is not part of GNU Emacs.

;; Copyright (C) 2025, 2026  Pierre Rouleau
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
;; This uses PEL ERT support.
;;  See: https://github.com/pierre-rouleau/pel/blob/master/pel-ert.el
(require 'pel-ert)
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst seed7-test-seed7-code-dirname (expand-file-name
                                         "seed7-code"
                                         (file-name-directory
                                          (directory-file-name
                                           (file-name-directory
                                            (directory-file-name
                                             (buffer-file-name))))))
  ;; That directory is the peer of the directory holding this emacs lisp file.
  ;; The current file is in          : seed7-mode/tests/ert-tests
  ;; The Seed7 test code files are in: seed7-mode/tests/seed7-code
  "Absolute path of the directory that holds the Seed7 test code files.")


;;; --------------------------------------------------------------------------
(provide 'seed7-test-sets-01)

;;; seed7-test-sets-01.el ends here
