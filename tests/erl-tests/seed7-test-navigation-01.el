;;; seed7-test-navigation-01.el --- ERT-based test of Seed7 navigation commands.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 28 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>

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
;; ERT-based tests for seed7-mode navigation commands.
;;
;; These tests verify that the navigation functions correctly find the
;; beginning and end of function and procedure definitions, and that
;; declaration-detection predicates work correctly.
;;
;; The tests use the Seed7 fixture file navigation-01.sd7, which contains:
;;
;;   Line 23: const func integer: add (in integer: a, in integer: b) is func
;;   Line 24:   local
;;   Line 25:     var integer: result is 0;
;;   Line 26:   begin
;;   Line 27:     result := a + b;
;;   Line 28:     return result;
;;   Line 29:   end func;
;;   Line 30: (blank)
;;   Line 31: const func string: greet (in string: name) is func
;;   Line 32:   local
;;   Line 33:     var string: msg is "";
;;   Line 34:   begin
;;   Line 35:     msg := "Hello " <& name;
;;   Line 36:     return msg;
;;   Line 37:   end func;
;;   Line 38: (blank)
;;   Line 39: const proc: printValue (in integer: n) is func
;;   Line 40:   begin
;;   Line 41:     writeln(n);
;;   Line 42:   end func;

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Code:
;;

(defconst seed7-test-nav--seed7-code-dirname
  (expand-file-name
   "seed7-code"
   (file-name-directory
    (directory-file-name
     (file-name-directory
      (directory-file-name
       (buffer-file-name))))))
  "Absolute path of the directory holding the Seed7 test code files.
The current file is in          : seed7-mode/tests/erl-tests
The Seed7 test code files are in: seed7-mode/tests/seed7-code")

;; Line numbers of key elements in navigation-01.sd7.
;; Update these if the fixture file is modified.
(defconst seed7-test-nav--add-decl-line     23
  "Line number of the 'add' function declaration in navigation-01.sd7.")
(defconst seed7-test-nav--add-body-line     27
  "Line number of the body of the 'add' function (result := a + b).")
(defconst seed7-test-nav--add-end-line      29
  "Line number of 'end func;' closing the 'add' function.")
(defconst seed7-test-nav--greet-decl-line   31
  "Line number of the 'greet' function declaration in navigation-01.sd7.")
(defconst seed7-test-nav--greet-body-line   35
  "Line number of the body of the 'greet' function.")
(defconst seed7-test-nav--greet-end-line    37
  "Line number of 'end func;' closing the 'greet' function.")
(defconst seed7-test-nav--proc-decl-line    39
  "Line number of the 'printValue' procedure declaration.")
(defconst seed7-test-nav--proc-end-line     42
  "Line number of 'end func;' closing 'printValue'.")

;; ---------------------------------------------------------------------------
;;** Helper

(defun seed7-test-nav--goto-line (n)
  "Move point to the beginning of line N (1-based) in the current buffer."
  (goto-char (point-min))
  (forward-line (1- n)))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-beg-of-defun

(ert-deftest seed7-test-beg-of-defun-from-body ()
  "Test that `seed7-beg-of-defun' moves from a function body to its declaration."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start inside the body of 'add' (line 27: result := a + b;)
          (seed7-test-nav--goto-line seed7-test-nav--add-body-line)
          (seed7-beg-of-defun 1 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--add-decl-line)))
      (kill-buffer (current-buffer)))))

(ert-deftest seed7-test-beg-of-defun-from-end ()
  "Test `seed7-beg-of-defun' moving from the end of a function to its declaration."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start on the 'end func;' line of 'greet' (line 37)
          (seed7-test-nav--goto-line seed7-test-nav--greet-end-line)
          (seed7-beg-of-defun 1 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--greet-decl-line)))
      (kill-buffer (current-buffer)))))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-end-of-defun

(ert-deftest seed7-test-end-of-defun-from-body ()
  "Test that `seed7-end-of-defun' moves from inside a function to its end."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start inside the body of 'add' (line 27)
          (seed7-test-nav--goto-line seed7-test-nav--add-body-line)
          (seed7-end-of-defun 1 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--add-end-line)))
      (kill-buffer (current-buffer)))))

(ert-deftest seed7-test-end-of-defun-proc ()
  "Test that `seed7-end-of-defun' works for a procedure definition."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start on the 'printValue' procedure declaration line (line 39)
          (seed7-test-nav--goto-line seed7-test-nav--proc-decl-line)
          (seed7-end-of-defun 1 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--proc-end-line)))
      (kill-buffer (current-buffer)))))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-beg-of-next-defun

(ert-deftest seed7-test-beg-of-next-defun ()
  "Test that `seed7-beg-of-next-defun' moves to the next function declaration."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start at the beginning of 'add' declaration (line 23)
          (seed7-test-nav--goto-line seed7-test-nav--add-decl-line)
          (seed7-beg-of-next-defun 1 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--greet-decl-line)))
      (kill-buffer (current-buffer)))))

(ert-deftest seed7-test-beg-of-next-defun-twice ()
  "Test that `seed7-beg-of-next-defun' with N=2 skips two functions."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Start at the beginning of 'add' declaration (line 23)
          (seed7-test-nav--goto-line seed7-test-nav--add-decl-line)
          (seed7-beg-of-next-defun 2 'silent :dont-push-mark)
          (should (= (seed7-current-line-number)
                     seed7-test-nav--proc-decl-line)))
      (kill-buffer (current-buffer)))))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-line-is-procfunc-beg-of-decl

(ert-deftest seed7-test-line-is-procfunc-beg-on-declaration ()
  "Test that `seed7-line-is-procfunc-beg-of-decl' is non-nil on a declaration line."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Line 23 is "const func integer: add (...)  is func"
          (seed7-test-nav--goto-line seed7-test-nav--add-decl-line)
          (should (seed7-line-is-procfunc-beg-of-decl 0)))
      (kill-buffer (current-buffer)))))

(ert-deftest seed7-test-line-is-procfunc-beg-on-proc-declaration ()
  "Test that `seed7-line-is-procfunc-beg-of-decl' is non-nil on a proc declaration."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Line 39 is "const proc: printValue (...) is func"
          (seed7-test-nav--goto-line seed7-test-nav--proc-decl-line)
          (should (seed7-line-is-procfunc-beg-of-decl 0)))
      (kill-buffer (current-buffer)))))

(ert-deftest seed7-test-line-is-procfunc-beg-not-on-body ()
  "Test that `seed7-line-is-procfunc-beg-of-decl' is nil inside a function body."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Line 27 is "    result := a + b;"
          (seed7-test-nav--goto-line seed7-test-nav--add-body-line)
          (should-not (seed7-line-is-procfunc-beg-of-decl 0)))
      (kill-buffer (current-buffer)))))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-current-line-number

(ert-deftest seed7-test-current-line-number ()
  "Test that `seed7-current-line-number' returns the correct line number."
  (let ((nav-file (expand-file-name "navigation-01.sd7"
                                    seed7-test-nav--seed7-code-dirname)))
    (find-file-read-only nav-file)
    (unwind-protect
        (progn
          ;; Move to line 23 and verify.
          (seed7-test-nav--goto-line 23)
          (should (= (seed7-current-line-number) 23))
          ;; Move to line 1 and verify.
          (seed7-test-nav--goto-line 1)
          (should (= (seed7-current-line-number) 1)))
      (kill-buffer (current-buffer)))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-navigation-01)

;;; seed7-test-navigation-01.el ends here