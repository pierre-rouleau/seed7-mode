;;; seed7-test-arrays-01.el --- ERT-based test of Seed7 array handling.  -*- lexical-binding: t; -*-

;; Created   : Friday, July 11 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-07-11 16:01:08 EDT, updated by Pierre Rouleau>

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
  ;; That directory is the peer of the directory holing this emacs lisp file.
  ;; The current file is in          : seed7-mode/test/erl-tests
  ;; The Seed7 test code files are in: seed7-mode/test/seed7-code
  "Absolute path of the directory that holds the Seed7 test code files.")

;; line, column, expected result of (seed7-line-at-endof-array-definition-block 0)
(defconst seed7--endof-array-definition-block-tests
  '((22 0 nil)
    (23 0 nil)
    (28 0 nil)
    (31 0 nil)
    (32 0 nil)
    (33 0 nil)
    (34 0 nil)
    (35 0 nil)
    (36 0 nil)
    (37 0 nil)
    (38 0 nil)
    (39 0 nil)
    (40 0 nil)
    (41 0 nil)
    (42 0 nil)
    (43 0 nil)
    (44 0 nil)
    (45 0 nil)
    (46 0 nil)
    (47 0 2)
    (47 6 2)
    (47 7 2)
    (47 8 2)
    (47 9 2)
    (48 0 nil)
    (49 0 nil)
    (50 0 nil)
    (51 0 nil)
    (52 0 nil)
    (53 0 nil)
    (54 0 nil)
    (55 0 nil)
    (56 0 nil)
    (57 0 2)
    (57 9 2)
    (57 36 2)
    ;; Inside inStringerVar
    (62 0 nil)
    (63 0 nil)
    (64 0 nil)
    (65 0 nil)
    (66 0 nil)
    ))

;; line, column, expected result of (seed7-line-inside-array-definition-block 0)
(defconst seed7--inside-array-definition-block-test
  '((22 0 nil)
    (23 0 nil)
    (28 0 nil)
    (31 0 nil)
    (32 0 (0 "array" 1194 1904))
    (33 0 (0 "array" 1194 1904))
    (34 0 (0 "array" 1194 1904))
    (35 0 (0 "array" 1194 1904))
    (36 0 (0 "array" 1194 1904))
    (37 0 (0 "array" 1194 1904))
    (38 0 (0 "array" 1194 1904))
    (39 0 (0 "array" 1194 1904))
    (40 0 (0 "array" 1194 1904))
    (41 0 (0 "array" 1194 1904))
    (42 0 (0 "array" 1194 1904))
    (43 0 (0 "array" 1194 1904))
    (44 0 (0 "array" 1194 1904))
    (45 0 (0 "array" 1194 1904))
    (46 0 (0 "array" 1194 1904))
    (46 7 (0 "array" 1194 1904))
    (46 8 (0 "array" 1194 1904))
    (46 9 (0 "array" 1194 1904))
    (46 15 (0 "array" 1194 1904))
    (46 16 (0 "array" 1194 1904))
    (46 33 (0 "array" 1194 1904))
    (46 34 (0 "array" 1194 1904))
    (46 48 (0 "array" 1194 1904))        ; at eol
    (47 0  (0 "array" 1194 1904))
    (47 7  (0 "array" 1194 1904))
    (47 8  (0 "array" 1194 1904))        ; at ;
    (47 9  (0 "array" 1194 1904))        ; at eol
    (48 0 nil)
    (49 0 nil)
    (50 0 (0 "array" 1907 2274))
    (51 0 (0 "array" 1907 2274))
    (52 0 (0 "array" 1907 2274))
    (53 0 (0 "array" 1907 2274))
    (54 0 (0 "array" 1907 2274))
    (55 0 (0 "array" 1907 2274))
    (56 0 (0 "array" 1907 2274))
    (57 0 (0 "array" 1907 2274))
    (57 9 (0 "array" 1907 2274))
    (58 0 nil)
    ;; Inside inStringerVar
    (62 0 nil)
    (63 0 nil)
    (64 0 nil)
    (65 0 nil)
    (66 0 nil)
    ))

(ert-deftest ert-test-endof-array-definition-block ()
  "Test ability to detect end of array definition block."
  (let ((test-seed7-code-filename (expand-file-name "arrays-01.sd7"
                                                    seed7-test-seed7-code-dirname)))

    ;; -- seed7-line-at-endof-array-definition-block
    (find-file-read-only test-seed7-code-filename)
    (should (string=  (buffer-file-name) test-seed7-code-filename))

    (dolist (test-spec seed7--endof-array-definition-block-tests)
      (goto-char (point-min))
      (forward-line (1- (nth 0 test-spec)))
      (forward-char (nth 1 test-spec))
      (should (pel-eq (seed7-line-at-endof-array-definition-block 0)
                      (nth 2 test-spec)
                      test-spec)))

    ;; -- seed7-line-inside-array-definition-block
    (dolist (test-spec seed7--inside-array-definition-block-test)
      (goto-char (point-min))
      (forward-line (1- (nth 0 test-spec)))
      (forward-char (nth 1 test-spec))
      (should (pel-equal (seed7-line-inside-array-definition-block 0)
                         (nth 2 test-spec)
                         test-spec)))))



;;; --------------------------------------------------------------------------
(provide 'seed7-test-arrays-01)

;;; seed7-test-arrays-01.el ends here
