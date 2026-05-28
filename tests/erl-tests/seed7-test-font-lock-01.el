;;; seed7-test-font-lock-01.el --- ERT-based test of Seed7 font-lock (syntax highlighting).  -*- lexical-binding: t; -*-

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
;; ERT-based tests for seed7-mode font-lock (syntax highlighting).
;;
;; These tests verify that seed7-mode correctly assigns font-lock faces to
;; various Seed7 language constructs: keywords, types, literals, comments,
;; strings, includes, and pragma directives.
;;
;; Tests use (with-temp-buffer) to avoid file I/O; each test inserts a small
;; Seed7 snippet, activates seed7-mode, and calls font-lock-ensure to apply
;; highlighting, then checks the face assigned at specific buffer positions.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;;** Helper

(defun seed7-test--face-at (pos)
  "Return the font-lock face at POS.
Handles both a single face symbol and a list of faces by returning
the first element of the list."
  (let ((face (get-text-property pos 'face)))
    (if (listp face) (car face) face)))

(defun seed7-test--face-includes-p (pos expected-face)
  "Return non-nil when the face at POS includes EXPECTED-FACE.
Accepts both a single face symbol and a list of faces at POS."
  (let ((face (get-text-property pos 'face)))
    (if (listp face)
        (memq expected-face face)
      (eq face expected-face))))

;; ---------------------------------------------------------------------------
;;** Tests: include directive

(ert-deftest seed7-test-font-lock-include ()
  "Test that '$ include' is highlighted with `seed7-include-face'."
  (with-temp-buffer
    ;; "$ include \"seed7_05.s7i\";\n"
    ;;  1234567890123456789012345678
    ;;  ^         ^
    ;;  1         10
    (insert "$ include \"seed7_05.s7i\";\n")
    (seed7-mode)
    (font-lock-ensure)
    ;; "$ include" is matched by seed7-include-regexp:
    ;; group 1 captures from $ to the 'e' of include.
    ;; Position 1 is '$', position 3 is 'i' of "include".
    (should (seed7-test--face-includes-p 3 'seed7-include-face))))

;; ---------------------------------------------------------------------------
;;** Tests: statement-enclosing keywords (if, for, while, func, proc, ...)

(ert-deftest seed7-test-font-lock-keyword-if ()
  "Test that 'if' at start of line gets `seed7-statement-introducing-keyword-face'."
  (with-temp-buffer
    ;; "if x > 0 then\n"
    ;;  123456789012345
    ;;  ^
    ;;  1
    (insert "if x > 0 then\n")
    (seed7-mode)
    (font-lock-ensure)
    ;; "if" is in seed7--statement-enclosing-keywords
    ;; regexp: "^[[:blank:]]*\<(if)\>"  - group 1 captures "if"
    (should (seed7-test--face-includes-p 1 'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-test-font-lock-keyword-for ()
  "Test that 'for' at start of line gets `seed7-statement-introducing-keyword-face'."
  (with-temp-buffer
    ;; "for i range 1 to 10 do\n"
    ;;  1234567890123456789012345
    ;;  ^
    ;;  1
    (insert "for i range 1 to 10 do\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 1 'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-test-font-lock-keyword-while ()
  "Test that 'while' at start of line gets `seed7-statement-introducing-keyword-face'."
  (with-temp-buffer
    (insert "while x > 0 do\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 1 'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-test-font-lock-keyword-func ()
  "Test that 'func' at start of line gets `seed7-statement-introducing-keyword-face'."
  (with-temp-buffer
    ;; "func\n"  - start of a func block
    (insert "func\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 1 'seed7-statement-introducing-keyword-face))))

;; ---------------------------------------------------------------------------
;;** Tests: declaration introduction keywords (const, var)

(ert-deftest seed7-test-font-lock-keyword-const ()
  "Test that 'const' gets `seed7-intro-statement-keyword-face'."
  (with-temp-buffer
    ;; "const integer: x is 42;\n"
    ;;  1234567890123456789012345
    ;;  ^
    ;;  1
    (insert "const integer: x is 42;\n")
    (seed7-mode)
    (font-lock-ensure)
    ;; "const" matches seed7-declaration-intro-keywords-regexp
    (should (seed7-test--face-includes-p 1 'seed7-intro-statement-keyword-face))))

(ert-deftest seed7-test-font-lock-keyword-var ()
  "Test that 'var' gets `seed7-intro-statement-keyword-face'."
  (with-temp-buffer
    ;; "  var integer: count is 0;\n"
    ;;   1234567890123456789012345678
    ;;   ^   = pos 1 (with-temp-buffer; no leading space at buffer start...)
    ;; Let's insert without leading spaces so 'var' is at pos 1.
    (insert "var integer: count is 0;\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 1 'seed7-intro-statement-keyword-face))))

;; ---------------------------------------------------------------------------
;;** Tests: predefined types

(ert-deftest seed7-test-font-lock-type-integer ()
  "Test that 'integer' gets `font-lock-type-face'."
  (with-temp-buffer
    ;; "var integer: x is 0;\n"
    ;;  123456789012345678901
    ;;      ^
    ;;  pos 5 = 'i' of "integer"
    (insert "var integer: x is 0;\n")
    (seed7-mode)
    (font-lock-ensure)
    ;; "integer" starts at position 5 (after "var ")
    (should (seed7-test--face-includes-p 5 'font-lock-type-face))))

(ert-deftest seed7-test-font-lock-type-string ()
  "Test that 'string' gets `font-lock-type-face'."
  (with-temp-buffer
    ;; "var string: name is \"\";\n"
    ;;  1234567890123456789012345
    ;;      ^
    ;;  pos 5 = 's' of "string"
    (insert "var string: name is \"\";\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 5 'font-lock-type-face))))

;; ---------------------------------------------------------------------------
;;** Tests: numeric literals

(ert-deftest seed7-test-font-lock-integer-literal ()
  "Test that an integer literal gets `seed7-integer-face'."
  (with-temp-buffer
    ;; "x := 42;\n"
    ;;  1234567890
    ;;       ^
    ;;  pos 6 = '4' of "42"
    (insert "x := 42;\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 6 'seed7-integer-face))))

(ert-deftest seed7-test-font-lock-float-literal ()
  "Test that a float literal gets `seed7-float-face'."
  (with-temp-buffer
    ;; "x := 3.14;\n"
    ;;  12345678901
    ;;       ^
    ;;  pos 6 = '3' of "3.14"
    (insert "x := 3.14;\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 6 'seed7-float-face))))

;; ---------------------------------------------------------------------------
;;** Tests: comments

(ert-deftest seed7-test-font-lock-line-comment ()
  "Test that text after '#' gets `font-lock-comment-face'."
  (with-temp-buffer
    ;; "# this is a comment\n"
    ;;  12345678901234567890
    ;;    ^
    ;;  pos 3 = 't' of "this" (inside line comment)
    (insert "# this is a comment\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 3 'font-lock-comment-face))))

(ert-deftest seed7-test-font-lock-block-comment ()
  "Test that text inside '(* *)' gets `font-lock-comment-face'."
  (with-temp-buffer
    ;; "(* block comment *)\n"
    ;;  12345678901234567890
    ;;      ^
    ;;  pos 5 = 'b' of "block" (inside block comment)
    (insert "(* block comment *)\n")
    (seed7-mode)
    (font-lock-ensure)
    (should (seed7-test--face-includes-p 5 'font-lock-comment-face))))

;; ---------------------------------------------------------------------------
;;** Tests: string literals

(ert-deftest seed7-test-font-lock-string-literal ()
  "Test that text inside double quotes gets `font-lock-string-face'."
  (with-temp-buffer
    ;; "x := \"hello\";\n"
    ;;  123456789012345
    ;;           ^
    ;;  pos 7 = opening quote of "hello"
    ;;  pos 8 = 'h' inside string
    (insert "x := \"hello\";\n")
    (seed7-mode)
    (font-lock-ensure)
    ;; Check position 8, which is 'h' inside the string literal.
    ;; Note: the opening quote itself may or may not have string-face
    ;; depending on Emacs version; checking inside the string is safer.
    (should (seed7-test--face-includes-p 8 'font-lock-string-face))))

;; ---------------------------------------------------------------------------
;;** Tests: seed7-inside-comment-p and seed7-inside-string-p

(ert-deftest seed7-test-inside-line-comment ()
  "Test that `seed7-inside-comment-p' returns non-nil inside a line comment."
  (with-temp-buffer
    ;; "x := 1; # comment\n"
    ;;  12345678901234567890
    ;;           ^
    ;;  pos 10 = '#'  (delimiter)
    ;;  pos 12 = 'c' of "comment"  (inside comment body)
    (insert "x := 1; # comment\n")
    (seed7-mode)
    (font-lock-ensure)
    (goto-char 12)
    (should (seed7-inside-comment-p))))

(ert-deftest seed7-test-inside-block-comment ()
  "Test that `seed7-inside-comment-p' returns non-nil inside a block comment."
  (with-temp-buffer
    ;; "(* a block comment *)\n"
    ;;  12345678901234567890
    ;;      ^
    ;;  pos 5 = 'a' of "a block comment"
    (insert "(* a block comment *)\n")
    (seed7-mode)
    (font-lock-ensure)
    (goto-char 5)
    (should (seed7-inside-comment-p))))

(ert-deftest seed7-test-not-inside-comment ()
  "Test that `seed7-inside-comment-p' returns nil for normal code."
  (with-temp-buffer
    ;; "x := 42;\n"
    ;;  123456789
    ;;  ^
    ;;  pos 1 = 'x' (normal code)
    (insert "x := 42;\n")
    (seed7-mode)
    (font-lock-ensure)
    (goto-char 1)
    (should-not (seed7-inside-comment-p))))

(ert-deftest seed7-test-inside-string ()
  "Test that `seed7-inside-string-p' returns non-nil inside a string."
  (with-temp-buffer
    ;; "x := \"hello\";\n"
    ;;  12345678901234
    ;;         ^
    ;;  pos 8 = 'h' inside string (note: opening quote is pos 7)
    (insert "x := \"hello\";\n")
    (seed7-mode)
    (font-lock-ensure)
    (goto-char 8)
    (should (seed7-inside-string-p))))

(ert-deftest seed7-test-not-inside-string ()
  "Test that `seed7-inside-string-p' returns nil for normal code."
  (with-temp-buffer
    (insert "x := 42;\n")
    (seed7-mode)
    (font-lock-ensure)
    (goto-char 1)
    (should-not (seed7-inside-string-p))))

;; ---------------------------------------------------------------------------
;;** Tests: blank line detection

(ert-deftest seed7-test-blank-line-true ()
  "Test that `seed7-blank-line-p' returns non-nil on an empty line."
  (with-temp-buffer
    (insert "code\n\nmore code\n")
    (seed7-mode)
    ;; go to line 2 (the blank line)
    (goto-char (point-min))
    (forward-line 1)
    (should (seed7-blank-line-p))))

(ert-deftest seed7-test-blank-line-false ()
  "Test that `seed7-blank-line-p' returns nil on a non-blank line."
  (with-temp-buffer
    (insert "x := 42;\n")
    (seed7-mode)
    (goto-char (point-min))
    (should-not (seed7-blank-line-p))))

;; ---------------------------------------------------------------------------
;;** Tests: line indentation

(ert-deftest seed7-test-current-line-indent ()
  "Test that `seed7-current-line-indent' returns the correct indentation."
  (with-temp-buffer
    ;; A line with 4-space indentation
    (insert "    result := 0;\n")
    (seed7-mode)
    (goto-char (point-min))
    (should (= (seed7-current-line-indent) 4))))

(ert-deftest seed7-test-current-line-indent-zero ()
  "Test that `seed7-current-line-indent' returns 0 for unindented lines."
  (with-temp-buffer
    (insert "result := 0;\n")
    (seed7-mode)
    (goto-char (point-min))
    (should (= (seed7-current-line-indent) 0))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-font-lock-01)

;;; seed7-test-font-lock-01.el ends here