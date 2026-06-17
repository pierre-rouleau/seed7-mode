;;; seed7-test-mark-defun-01.el --- ERT-based test of mark-defun in Seed7 buffer.  -*- lexical-binding: t; -*-

;; Created   : Sunday, July 20 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-17 11:09:00 EDT, updated by Pierre Rouleau>

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

;;; ----------------------------------------------------------------------------
;;; Commentary:
;;
;;

;;; ----------------------------------------------------------------------------
;;; Dependencies:
;;
;; Allow byte-compiling and loading this file from an interactive Emacs session
;; or in batch mode without needing `tests/ert-tests/' to be pre-configured in
;; `load-path'.
;;
;; Variable availability by context:
;;   `load-file-name'            – set when a file is loaded via `load'/`require'
;;                                 (i.e. via the -l flag in batch mode)
;;   `byte-compile-current-file' – set by the byte compiler during batch/interactive
;;                                 byte compilation
;;   `buffer-file-name'          – fallback for an interactively visited buffer
(eval-and-compile
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (let* ((ert-tests-dir (file-name-directory (expand-file-name this-file))))
        (unless (member ert-tests-dir load-path)
          (push ert-tests-dir load-path))))))

(require 'ert)
(require 'seed7-mode)
(require 'pel-ert)

;;; ----------------------------------------------------------------------------
;;; Code:
;;

;; Identify directory of test
(defconst seed7-test-seed7-code-dirname (expand-file-name
                                         "seed7-code"
                                         (file-name-directory
                                          (directory-file-name
                                           (file-name-directory
                                            (directory-file-name
                                             (or load-file-name buffer-file-name))))))
  ;; That directory is the peer of the directory holding this emacs lisp file.
  ;; The current file is in          : seed7-mode/tests/ert-tests
  ;; The Seed7 test code files are in: seed7-mode/tests/seed7-code
  "Absolute path of the directory that holds the Seed7 test code files.")


(defconst seed7-test-seed-nav-end-of-defun-test-spec
  '(

    (1 ,   1, 0, t,   150,  7, 27 )
    (10,   2, 0, t,   150,  7, 27 )
    (38,   3, 0, t,   150,  7, 27 )
    (64,   4, 0, t,   150,  7, 27 )
    (87,   5, 0, t,   150,  7, 27 )
    (88,   6, 0, t,   150,  7, 27 )
    (123,  7, 0, t,   150,  7, 27 )
    (151,  8, 0, t,   295, 15, 11 )
    (152,  9, 0, t,   295, 15, 11 )
    (153, 10, 0, t,   295, 15, 11 )
    (194, 11, 0, t,   295, 15, 11 )
    (203, 12, 0, t,   295, 15, 11 )
    (240, 13, 0, t,   295, 15, 11 )
    (248, 14, 0, t,   295, 15, 11 )
    (284, 15, 0, t,   295, 15, 11 )
    (296, 16, 0, t,   393, 19, 23 )
    (297, 17, 0, t,   393, 19, 23 )
    (319, 18, 0, t,   393, 19, 23 )
    (370, 19, 0, t,   393, 19, 23 )
    (394, 20, 0, t,   508, 24, 24 )
    (395, 21, 0, t,   508, 24, 24 )
    (416, 22, 0, t,   508, 24, 24 )
    (417, 23, 0, t,   508, 24, 24 )
    (484, 24, 0, t,   508, 24, 24 )
    (509, 25, 0, t,   689, 33, 31 )
    (510, 26, 0, t,   689, 33, 31 )
    (511, 27, 0, t,   689, 33, 31 )
    (542, 28, 0, t,   689, 33, 31 )
    (566, 29, 0, t,   689, 33, 31 )
    (580, 30, 0, t,   689, 33, 31 )
    (581, 32, 0, t,   689, 33, 31 )
    (582, 33, 0, t,   689, 33, 31 )
    (658, 34, 0, t,   689, 33, 31 )
    (690, 35, 0, nil, 690  34,  0 ))
  "Test specs data for testing (seed7-nav-end-of-defun).
Each element consist of a list holding the following elements:
- 0: starting point
- 1: starting line
- 2: starting column
- 3: Value returned by `seed7-nav-end-of-defun'
- 4: landing point
- 5: landing line
- 6: landing column.")

(defconst seed7-test-seed-nav-beginning-of-defun-test-spec-01
  '(
    (1 ,   1, 0, t, 88, 6,   0 )
    (10,   2, 0, t, 88, 6,   0 )
    (38,   3, 0, t, 88, 6,   0 )
    (64,   4, 0, t, 88, 6,   0 )
    (87,   5, 0, t, 88, 6,   0 )
    (88,   6, 0, t, 153, 10,   0 )
    (123,  7, 0, t, 153, 10,   0 )
    (151,  8, 0, t, 153, 10,   0 )
    (152,  9, 0, t, 153, 10,   0 )
    (153, 10, 0, t, 319, 18,   0 )
    (194, 11, 0, t, 319, 18,   0 )
    (203, 12, 0, t, 319, 18,   0 )
    (240, 13, 0, t, 319, 18,   0 )
    (248, 14, 0, t, 319, 18,   0 )
    (284, 15, 0, t, 319, 18,   0 )
    (296, 16, 0, t, 319, 18,   0 )
    (297, 17, 0, t, 319, 18,   0 )
    (319, 18, 0, t, 417, 23,   0 )
    (370, 19, 0, t, 417, 23,   0 )
    (394, 20, 0, t, 417, 23,   0 )
    (395, 21, 0, t, 417, 23,   0 )
    (416, 22, 0, t, 417, 23,   0 )
    (417, 23, 0, t, 582, 32,   0 )
    (484, 24, 0, t, 582, 32,   0 )
    (509, 25, 0, t, 582, 32,   0 )
    (510, 26, 0, t, 582, 32,   0 )
    (511, 27, 0, t, 582, 32,   0 )
    (542, 28, 0, t, 582, 32,   0 )
    (566, 29, 0, t, 582, 32,   0 )
    (580, 30, 0, t, 582, 32,   0 )
    (581, 32, 0, t, 582, 32,   0 )
    (582, 33, 0, nil, 582, 32,   0 )
    (658, 34, 0, t, 319, 18,   0 )
    (690, 35, 0, t, 319, 18,   0 ))
  "Test specs data for testing (seed7-nav-beginning-of-defun -1).
Each element consist of a list holding the following elements:
- 0: starting point
- 1: starting line
- 2: starting column
- 3: Value returned by `seed7-nav-beginning-of-defun'
- 4: landing point
- 5: landing line
- 6: landing column.")
;;; ----------------------------------------------------------------------------
(provide 'seed7-test-mark-defun-01)

;;; seed7-test-mark-defun-01.el ends here
