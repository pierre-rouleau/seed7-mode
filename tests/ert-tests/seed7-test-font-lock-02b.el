;;; seed7-test-font-lock-02b.el --- ERT tests: seed7-font-lock-keywords — all fontifications  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>

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
;; Regression tests for every font-lock face defined in
;; `seed7-font-lock-keywords'.
;;
;; The block-comment delimiter faces are covered by
;; `seed7-test-font-lock-01.el'.  The present file covers all remaining
;; entries in the keyword table:
;;
;;   Section P  — Pragma keywords            → seed7-pragma-keyword-face
;;   Section I  — Include directive          → seed7-include-face
;;   Section K1 — Lead-in keywords           → seed7-in-statement-keyword-face1
;;   Section K2 — In-statement keywords      → seed7-in-statement-keyword-face2
;;   Section K3 — Statement-enclosing kw.    → seed7-statement-introducing-keyword-face
;;   Section K4 — Is-statement keywords      → seed7-in-middle-statement-keyword-face1
;;   Section K5 — In-middle-statement kw.    → seed7-in-middle-statement-keyword-face2
;;   Section K6 — Declaration-intro kw.      → seed7-intro-statement-keyword-face
;;   Section T  — Predefined types           → font-lock-type-face
;;   Section N1 — Float numbers              → seed7-float-face
;;   Section N2 — Invalid float numbers      → font-lock-warning-face
;;   Section N3 — Number with exponent       → seed7-integer-face
;;   Section N4 — Number with neg. exponent  → font-lock-warning-face
;;   Section N5 — Base-x integers            → seed7-integer-face
;;   Section N6 — Base-x big integers        → seed7-big-integer-face
;;   Section N7 — Big integers (underscore)  → seed7-big-integer-face
;;   Section N8 — Plain integers             → seed7-integer-face
;;   Section N9 — Invalid 0x integers        → font-lock-warning-face
;;   Section C  — Predefined constants       → font-lock-constant-face
;;   Section V  — Predefined variables       → seed7-predefined-variables-face
;;   Section E  — Errinfo values             → seed7-errinfo-value-face
;;   Section O1 — Operator symbols (word)    → font-lock-keyword-face
;;   Section O2 — Assignment operators       → font-lock-keyword-face
;;   Section O3 — Other predefined operators → font-lock-keyword-face
;;   Section O4 — Comparison operators       → font-lock-keyword-face
;;   Section O5 — Arithmetic operators       → font-lock-keyword-face
;;   Section O6 — Division / operator        → font-lock-keyword-face
;;   Section O7 — Power ** operator          → font-lock-keyword-face
;;   Section O8 — Logic & | operators        → font-lock-keyword-face
;;   Section O9 — Minus/plus operators       → font-lock-keyword-face
;;   Section O10— Tilde ~ and range .. ops   → font-lock-keyword-face
;;   Section W  — Invalid char literals      → font-lock-warning-face
;;   Section ID — Name identifiers           → seed7-name-identifier-face
;;
;; To run from the command line:
;;
;;   emacs --batch -L . \
;;         -l ert \
;;         -l tests/ert-tests/seed7-test-font-lock-02b.el \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:

(eval-and-compile
  (let* ((this-file (or load-file-name
                        (and (boundp 'byte-compile-current-file)
                             (stringp byte-compile-current-file)
                             byte-compile-current-file)
                        buffer-file-name)))
    (when this-file
      (let ((ert-tests-dir (file-name-directory (expand-file-name this-file))))
        (unless (member ert-tests-dir load-path)
          (push ert-tests-dir load-path))))))

(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Shared helpers (parallel to seed7-test-font-lock-01.el)

(defmacro seed7-test2--with-fontified-buffer (text &rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing TEXT, fully fontified."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (seed7-mode)
     (syntax-propertize (point-max))
     (font-lock-ensure)
     ,@body))

(defun seed7-test2--pos (offset)
  "Return buffer position for 0-based character OFFSET."
  (+ (point-min) offset))

(defun seed7-test2--has-face-p (pos expected-face)
  "Return non-nil if EXPECTED-FACE is applied at buffer position POS."
  (let ((face (get-text-property pos 'face)))
    (cond
     ((null face)  nil)
     ((listp face) (memq expected-face face))
     (t            (eq  face expected-face)))))

(defun seed7-test2--face-at (pos)
  "Return the face (or list of faces) at POS, for diagnostics."
  (get-text-property pos 'face))

;;; --------------------------------------------------------------------------
;;; Section P — Pragma keywords → seed7-pragma-keyword-face
;;; --------------------------------------------------------------------------
;;
;; The regexp is:  ^\\<\\(\\$ +<keyword>\\)\\>
;; Group 1 covers the entire "$ keyword" token.
;; Offset 0 is the '$', offset 2 (after "$ ") is the first letter of the kw.

(ert-deftest seed7-font-lock/pragma-library-has-pragma-face ()
  "Section P.1 — `$ library' at start of line → seed7-pragma-keyword-face on `$'.
Buffer: \"$ library \\\"s7i/string.s7i\\\"\" — group 1 begins at offset 0."
  (seed7-test2--with-fontified-buffer "$ library \"s7i/string.s7i\""
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-pragma-keyword-face))))

(ert-deftest seed7-font-lock/pragma-message-has-pragma-face ()
  "Section P.2 — `$ message' at start of line → seed7-pragma-keyword-face."
  (seed7-test2--with-fontified-buffer "$ message \"hello\""
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-pragma-keyword-face))))

(ert-deftest seed7-font-lock/pragma-syntax-has-pragma-face ()
  "Section P.3 — `$ syntax' at start of line → seed7-pragma-keyword-face."
  (seed7-test2--with-fontified-buffer "$ syntax expr"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-pragma-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section I — Include directive → seed7-include-face
;;; --------------------------------------------------------------------------
;;
;; The regexp is: ^\\(\\$? *\\(?:include\\)\\)
;; Group 1 covers "include" (or "$include") at the start of a line.
;; In "include \"foo.s7i\"", offset 0..6 = "include".

(ert-deftest seed7-font-lock/include-has-include-face ()
  "Section I.1 — `include' at start of line → seed7-include-face.
Buffer: \"include \\\"s7i/string.s7i\\\"\" — group 1 is offsets 0..6."
  (seed7-test2--with-fontified-buffer "include \"s7i/string.s7i\""
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-include-face))))

(ert-deftest seed7-font-lock/dollar-include-has-include-face ()
  "Section I.2 — `$include' at start of line → seed7-include-face.
Buffer: \"$include \\\"s7i/string.s7i\\\"\" — offset 0 is '$'."
  (seed7-test2--with-fontified-buffer "$include \"s7i/string.s7i\""
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-include-face))))

;;; --------------------------------------------------------------------------
;;; Section K1 — Lead-in statement keywords → seed7-in-statement-keyword-face1
;;; --------------------------------------------------------------------------
;;
;; Keywords: "raise", "return"
;; Regexp: ^ *\\<\\(raise|return\\)\\>
;; These must appear at the beginning of a line (with optional leading spaces).

(ert-deftest seed7-font-lock/raise-has-face1 ()
  "Section K1.1 — `raise' at start of line → seed7-in-statement-keyword-face1.
Buffer: \"raise RANGE_ERROR;\" — 'r' at offset 0."
  (seed7-test2--with-fontified-buffer "raise RANGE_ERROR;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-in-statement-keyword-face1))))

(ert-deftest seed7-font-lock/return-has-face1 ()
  "Section K1.2 — `return' at start of line → seed7-in-statement-keyword-face1.
Buffer: \"return x;\" — 'r' at offset 0."
  (seed7-test2--with-fontified-buffer "return x;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-in-statement-keyword-face1))))

(ert-deftest seed7-font-lock/return-with-leading-spaces-has-face1 ()
  "Section K1.3 — `return' with leading spaces → seed7-in-statement-keyword-face1.
Buffer: \"  return x;\" — 'r' at offset 2."
  (seed7-test2--with-fontified-buffer "  return x;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'seed7-in-statement-keyword-face1))))

;;; --------------------------------------------------------------------------
;;; Section K2 — In-statement keywords → seed7-in-statement-keyword-face2
;;; --------------------------------------------------------------------------
;;
;; Keywords: "is", "noop"
;; Regexp: . \\<\\(is|noop\\)\\>
;; Requires a preceding character + space before the keyword.

(ert-deftest seed7-font-lock/is-keyword-has-face2 ()
  "Section K2.1 — `is' following a declaration → seed7-in-statement-keyword-face2.
Buffer: \"const integer: x is 1;\"
The word 'is' starts at offset 19; we check offset 19."
  ;; "const integer: x is 1;"
  ;;  0              1
  ;;  0123456789012345678901
  ;;                   ^^-- 'is' at offset 18..19
  (seed7-test2--with-fontified-buffer "const integer: x is 1;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 18)
                                     'seed7-in-statement-keyword-face2))))

(ert-deftest seed7-font-lock/noop-has-face2 ()
  "Section K2.2 — `noop' in a statement → seed7-in-statement-keyword-face2.
Buffer: \"x noop;\" — 'n' at offset 2."
  ;; "x noop;"
  ;;  0123456
  (seed7-test2--with-fontified-buffer "x noop;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'seed7-in-statement-keyword-face2))))

;;; --------------------------------------------------------------------------
;;; Section K3 — Statement-enclosing keywords → seed7-statement-introducing-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Keywords: "if", "while", "for", "func", "proc", "begin", "end if",
;;           "repeat", "struct", "case", "block", "enum", etc.
;; Regexp: ^[[:blank:]]*\\<\\(<kw>\\)\\>
;; These must appear at the start of a line (with optional leading whitespace).

(ert-deftest seed7-font-lock/if-keyword-has-introducing-face ()
  "Section K3.1 — `if' at start of line → seed7-statement-introducing-keyword-face.
Buffer: \"if x > 0 then\" — 'i' at offset 0."
  (seed7-test2--with-fontified-buffer "if x > 0 then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-font-lock/while-keyword-has-introducing-face ()
  "Section K3.2 — `while' at start of line → seed7-statement-introducing-keyword-face."
  (seed7-test2--with-fontified-buffer "while x > 0 do"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-font-lock/for-keyword-has-introducing-face ()
  "Section K3.3 — `for' at start of line → seed7-statement-introducing-keyword-face."
  (seed7-test2--with-fontified-buffer "for i range 1 to 10 do"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-font-lock/func-keyword-has-introducing-face ()
  "Section K3.4 — `func' at start of line → seed7-statement-introducing-keyword-face."
  (seed7-test2--with-fontified-buffer "func integer: add(in integer: a, in integer: b) is"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-statement-introducing-keyword-face))))

(ert-deftest seed7-font-lock/end-if-keyword-has-introducing-face ()
  "Section K3.5 — `end if' at start of line → seed7-statement-introducing-keyword-face.
Buffer: \"end if\" — 'e' at offset 0."
  (seed7-test2--with-fontified-buffer "end if"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-statement-introducing-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section K4 — Is-statement keywords → seed7-in-middle-statement-keyword-face1
;;; --------------------------------------------------------------------------
;;
;; Keywords: "forward", "DYNAMIC", "new", "sub", "action"
;; These appear after "is" in a declaration: "func ... is forward"

(ert-deftest seed7-font-lock/forward-after-is-has-face-k4 ()
  "Section K4.1 — `forward' after `is' → seed7-in-middle-statement-keyword-face1.
Buffer: \"func integer: add is forward;\"
'forward' starts at offset 21; we check offset 21."
  ;; "func integer: add is forward;"
  ;;  0         1         2
  ;;  012345678901234567890123456789
  ;;                       ^-- 'f' of 'forward' at offset 21
  (seed7-test2--with-fontified-buffer "func integer: add is forward;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 21)
                                     'seed7-in-middle-statement-keyword-face1))))

(ert-deftest seed7-font-lock/action-after-is-has-face-k4 ()
  "Section K4.2 — `action' after `is' → seed7-in-middle-statement-keyword-face1.
Buffer: \"proc foo is action;\" — 'action' starts at offset 12."
  ;; "proc foo is action;"
  ;;  0         1
  ;;  0123456789012345678
  ;;             ^-- 'a' at 12
  (seed7-test2--with-fontified-buffer "proc foo is action;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 12)
                                     'seed7-in-middle-statement-keyword-face1))))

;;; --------------------------------------------------------------------------
;;; Section K5 — In-middle-statement keywords → seed7-in-middle-statement-keyword-face2
;;; --------------------------------------------------------------------------
;;
;; Keywords: "begin", "do", "then", "to", "downto", "range", "step",
;;           "result", "local", "default", "until", "of", "otherwise",
;;           "exception", "key", "len", "fixLen", "param"
;; Regexp: [[:space:]]\\(<kw>\\)\\>
;; The keyword must be preceded by whitespace.

(ert-deftest seed7-font-lock/then-has-face-k5 ()
  "Section K5.1 — `then' preceded by space → seed7-in-middle-statement-keyword-face2.
Buffer: \"if x > 0 then\" — ' then' has space at offset 8; 't' at offset 9."
  ;; "if x > 0 then"
  ;;  0         1
  ;;  01234567890123
  ;;          ^^-- space at 8, 't' at 9
  (seed7-test2--with-fontified-buffer "if x > 0 then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 9)
                                     'seed7-in-middle-statement-keyword-face2))))

(ert-deftest seed7-font-lock/do-has-face-k5 ()
  "Section K5.2 — `do' preceded by space → seed7-in-middle-statement-keyword-face2.
Buffer: \"while x > 0 do\" — 'd' at offset 13."
  ;; "while x > 0 do"
  ;;  0         1
  ;;  012345678901234
  ;;              ^-- 'd' at 13
  (seed7-test2--with-fontified-buffer "while x > 0 do"
    (should (seed7-test2--has-face-p (seed7-test2--pos 13)
                                     'seed7-in-middle-statement-keyword-face2))))

(ert-deftest seed7-font-lock/to-has-face-k5 ()
  "Section K5.3 — `to' preceded by space → seed7-in-middle-statement-keyword-face2.
Buffer: \"for i range 1 to 10 do\" — 't' at offset 14."
  ;; "for i range 1 to 10 do"
  ;;  0         1         2
  ;;  0123456789012345678901
  ;;               ^-- 't' at 14
  (seed7-test2--with-fontified-buffer "for i range 1 to 10 do"
    (should (seed7-test2--has-face-p (seed7-test2--pos 14)
                                     'seed7-in-middle-statement-keyword-face2))))

(ert-deftest seed7-font-lock/result-has-face-k5 ()
  "Section K5.4 — `result' preceded by space → seed7-in-middle-statement-keyword-face2.
Buffer: \"func integer: answer() is\\n  result\" — 'result' at start of second line.
In the flat string, 'r' of 'result' is at offset 28 (after the newline at 25)."
  ;; "func integer: answer() is\n  result"
  ;;  0         1         2
  ;;  01234567890123456789012345678901234
  ;;                            ^-- 'r' at 28 ("\n  result")
  ;;  The space before 'result' is at offset 27.
  (seed7-test2--with-fontified-buffer "func integer: answer() is\n  result"
    ;; 'r' of 'result' at offset 28
    (should (seed7-test2--has-face-p (seed7-test2--pos 28)
                                     'seed7-in-middle-statement-keyword-face2))))

;;; --------------------------------------------------------------------------
;;; Section K6 — Declaration-intro keywords → seed7-intro-statement-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Keywords: "attr", "const", "in", "inout", "ref", "val", "var"
;; Regexp: \\<\\(<kw>\\)\\>

(ert-deftest seed7-font-lock/const-has-intro-face ()
  "Section K6.1 — `const' → seed7-intro-statement-keyword-face.
Buffer: \"const integer: x is 42;\" — 'c' at offset 0."
  (seed7-test2--with-fontified-buffer "const integer: x is 42;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-intro-statement-keyword-face))))

(ert-deftest seed7-font-lock/var-has-intro-face ()
  "Section K6.2 — `var' → seed7-intro-statement-keyword-face.
Buffer: \"var integer: counter is 0;\" — 'v' at offset 0."
  (seed7-test2--with-fontified-buffer "var integer: counter is 0;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-intro-statement-keyword-face))))

(ert-deftest seed7-font-lock/in-param-has-intro-face ()
  "Section K6.3 — `in' in a parameter list → seed7-intro-statement-keyword-face.
Buffer: \"func void: foo(in integer: x) is\" — 'i' of 'in' at offset 15."
  ;; "func void: foo(in integer: x) is"
  ;;  0         1
  ;;  012345678901234567
  ;;                ^-- 'i' of 'in' at 15
  (seed7-test2--with-fontified-buffer "func void: foo(in integer: x) is"
    (should (seed7-test2--has-face-p (seed7-test2--pos 15)
                                     'seed7-intro-statement-keyword-face))))

(ert-deftest seed7-font-lock/inout-has-intro-face ()
  "Section K6.4 — `inout' → seed7-intro-statement-keyword-face.
Buffer: \"func void: swap(inout integer: x) is\" — 'i' of 'inout' at offset 16."
  ;; "func void: swap(inout integer: x) is"
  ;;  0         1
  ;;  0123456789012345678
  ;;                 ^-- 'i' of 'inout' at 16
  (seed7-test2--with-fontified-buffer "func void: swap(inout integer: x) is"
    (should (seed7-test2--has-face-p (seed7-test2--pos 16)
                                     'seed7-intro-statement-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section T — Predefined types → font-lock-type-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/type-integer-has-type-face ()
  "Section T.1 — `integer' → font-lock-type-face.
Buffer: \"var integer: x is 0;\" — 'i' of 'integer' at offset 4."
  ;; "var integer: x is 0;"
  ;;  0   4
  (seed7-test2--with-fontified-buffer "var integer: x is 0;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 4)
                                     'font-lock-type-face))))

(ert-deftest seed7-font-lock/type-boolean-has-type-face ()
  "Section T.2 — `boolean' → font-lock-type-face.
Buffer: \"var boolean: flag is FALSE;\" — 'b' at offset 4."
  (seed7-test2--with-fontified-buffer "var boolean: flag is FALSE;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 4)
                                     'font-lock-type-face))))

(ert-deftest seed7-font-lock/type-string-has-type-face ()
  "Section T.3 — `string' → font-lock-type-face.
Buffer: \"var string: s is \\\"hello\\\";\" — 's' of 'string' at offset 4."
  (seed7-test2--with-fontified-buffer "var string: s is \"hello\";"
    (should (seed7-test2--has-face-p (seed7-test2--pos 4)
                                     'font-lock-type-face))))

(ert-deftest seed7-font-lock/type-float-has-type-face ()
  "Section T.4 — `float' → font-lock-type-face.
Buffer: \"var float: pi is 3.14;\" — 'f' of 'float' at offset 4."
  (seed7-test2--with-fontified-buffer "var float: pi is 3.14;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 4)
                                     'font-lock-type-face))))

(ert-deftest seed7-font-lock/type-char-has-type-face ()
  "Section T.5 — `char' → font-lock-type-face.
Buffer: \"var char: c is 'a';\" — 'c' of 'char' at offset 4."
  (seed7-test2--with-fontified-buffer "var char: c is 'a';"
    (should (seed7-test2--has-face-p (seed7-test2--pos 4)
                                     'font-lock-type-face))))

;;; --------------------------------------------------------------------------
;;; Section N1 — Float numbers → seed7-float-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 0): [0-9]+\.[0-9]+(?:(?:[eE][+-]?)?[0-9]+)?

(ert-deftest seed7-font-lock/float-314-has-float-face ()
  "Section N1.1 — `3.14' → seed7-float-face on group 0.
Buffer: \"x := 3.14;\" — '3' at offset 5."
  ;; "x := 3.14;"
  ;;  0    5
  (seed7-test2--with-fontified-buffer "x := 3.14;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-float-face))))

(ert-deftest seed7-font-lock/float-with-exponent-has-float-face ()
  "Section N1.2 — `1.5e10' → seed7-float-face.
Buffer: \"x := 1.5e10;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 1.5e10;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-float-face))))

;;; --------------------------------------------------------------------------
;;; Section N2 — Invalid float numbers → font-lock-warning-face
;;; --------------------------------------------------------------------------
;;
;; Invalid float form 1: [^[:alnum:]]\(\.[0-9]+\) — a lone leading dot: .5
;; Invalid float form 2: [^[:alnum:]]\([0-9]+\.\)[^[:alnum:]] — trailing dot: 5.

(ert-deftest seed7-font-lock/invalid-float-leading-dot-has-warning-face ()
  "Section N2.1 — `.5' (leading dot) → font-lock-warning-face.
Buffer: \"x := .5;\"
The regexp matches the non-alnum char before '.', putting '.5' in group 1.
Here '.' is at offset 5."
  ;; "x := .5;"
  ;;  0    5
  (seed7-test2--with-fontified-buffer "x := .5;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-warning-face))))

(ert-deftest seed7-font-lock/invalid-float-trailing-dot-has-warning-face ()
  "Section N2.2 — `5.' (trailing dot) → font-lock-warning-face.
Buffer: \"x := 5. ;\"
The regexp matches space-5.-space; group 1 is '5.' starting at offset 5."
  ;; "x := 5. ;"
  ;;  0    5
  (seed7-test2--with-fontified-buffer "x := 5. ;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-warning-face))))

;;; --------------------------------------------------------------------------
;;; Section N3 — Numbers with positive exponent → seed7-integer-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 0): [0-9]+[eE]\+?[0-9]+

(ert-deftest seed7-font-lock/number-with-exponent-has-integer-face ()
  "Section N3.1 — `1e5' → seed7-integer-face (group 0).
Buffer: \"x := 1e5;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 1e5;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-integer-face))))

(ert-deftest seed7-font-lock/number-with-plus-exponent-has-integer-face ()
  "Section N3.2 — `10E+3' → seed7-integer-face.
Buffer: \"x := 10E+3;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 10E+3;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-integer-face))))

;;; --------------------------------------------------------------------------
;;; Section N4 — Numbers with NEGATIVE exponent → font-lock-warning-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 0): [0-9]+[eE]-[0-9]+  — invalid in Seed7.

(ert-deftest seed7-font-lock/number-with-negative-exponent-has-warning-face ()
  "Section N4 — `1e-5' → font-lock-warning-face (invalid in Seed7).
Buffer: \"x := 1e-5;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 1e-5;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-warning-face))))

;;; --------------------------------------------------------------------------
;;; Section N5 — Base-x integer literals → seed7-integer-face
;;; --------------------------------------------------------------------------
;;
;; Examples: 16#FF, 2#1010, 8#77

(ert-deftest seed7-font-lock/base16-integer-has-integer-face ()
  "Section N5.1 — `16#FF' → seed7-integer-face (group 1).
Buffer: \"x := 16#FF;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 16#FF;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-integer-face))))

(ert-deftest seed7-font-lock/base2-integer-has-integer-face ()
  "Section N5.2 — `2#1010' → seed7-integer-face.
Buffer: \"x := 2#1010;\" — '2' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 2#1010;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-integer-face))))

;;; --------------------------------------------------------------------------
;;; Section N6 — Base-x big integer literals → seed7-big-integer-face
;;; --------------------------------------------------------------------------
;;
;; Big integers contain a trailing underscore separator: 16#FF_

(ert-deftest seed7-font-lock/base16-big-integer-has-big-integer-face ()
  "Section N6 — `16#FF_' → seed7-big-integer-face.
Buffer: \"x := 16#FF_;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 16#FF_;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-big-integer-face))))

;;; --------------------------------------------------------------------------
;;; Section N7 — Big integer literals (decimal with underscore) → seed7-big-integer-face
;;; --------------------------------------------------------------------------
;;
;; Regexp group 1: \\(\\(?:\\([2-9]\\|1[0-9]\\|2[0-9]\\|3[0-6]\\)#\\)?[0-9]+_\\)
;; Matches: 1_ or 1234322_ or 1_000_

(ert-deftest seed7-font-lock/big-integer-underscore-has-big-integer-face ()
  "Section N7.1 — `1000_' → seed7-big-integer-face.
Buffer: \"x := 1000_;\" — '1' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 1000_;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-big-integer-face))))

(ert-deftest seed7-font-lock/big-integer-minimal-has-big-integer-face2 ()
  "Section N7.2 — `1_' → seed7-big-integer-face."
  (seed7-test2--with-fontified-buffer "x := 1_;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-big-integer-face))))

;;; --------------------------------------------------------------------------
;;; Section N8 — Plain integer literals → seed7-integer-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 1): \\([[:digit:]]+\\)

(ert-deftest seed7-font-lock/plain-integer-has-integer-face ()
  "Section N8 — `42' → seed7-integer-face (group 1).
Buffer: \"x := 42;\" — '4' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 42;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'seed7-integer-face))))

;;; --------------------------------------------------------------------------
;;; Section N9 — Invalid 0x hex integer → font-lock-warning-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 1): \\(0x[[:digit:]]+\\)
;; Seed7 does NOT use 0x notation — only base#digits notation.

(ert-deftest seed7-font-lock/invalid-0x-integer-has-warning-face ()
  "Section N9 — `0x1F' → font-lock-warning-face (invalid Seed7 hex notation).
Buffer: \"x := 0x1F;\" — '0' at offset 5."
  (seed7-test2--with-fontified-buffer "x := 0x1F;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-warning-face))))

;;; --------------------------------------------------------------------------
;;; Section C — Predefined constants → font-lock-constant-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/constant-TRUE-has-constant-face ()
  "Section C.1 — `TRUE' → font-lock-constant-face.
Buffer: \"x := TRUE;\" — 'T' at offset 5."
  (seed7-test2--with-fontified-buffer "x := TRUE;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-constant-face))))

(ert-deftest seed7-font-lock/constant-FALSE-has-constant-face ()
  "Section C.2 — `FALSE' → font-lock-constant-face.
Buffer: \"x := FALSE;\" — 'F' at offset 5."
  (seed7-test2--with-fontified-buffer "x := FALSE;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-constant-face))))

(ert-deftest seed7-font-lock/constant-NIL-has-constant-face ()
  "Section C.3 — `NIL' → font-lock-constant-face.
Buffer: \"ptr := NIL;\" — 'N' at offset 7."
  (seed7-test2--with-fontified-buffer "ptr := NIL;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 7)
                                     'font-lock-constant-face))))

(ert-deftest seed7-font-lock/constant-PI-has-constant-face ()
  "Section C.4 — `PI' → font-lock-constant-face.
Buffer: \"x := PI;\" — 'P' at offset 5."
  (seed7-test2--with-fontified-buffer "x := PI;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-constant-face))))

(ert-deftest seed7-font-lock/constant-EOF-has-constant-face ()
  "Section C.5 — `EOF' → font-lock-constant-face.
Buffer: \"if c = EOF then\" — 'E' of 'EOF' at offset 7."
  ;; "if c = EOF then"
  ;;  0       7
  (seed7-test2--with-fontified-buffer "if c = EOF then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 7)
                                     'font-lock-constant-face))))

;;; --------------------------------------------------------------------------
;;; Section V — Predefined variables → seed7-predefined-variables-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/variable-IN-has-predefined-var-face ()
  "Section V.1 — `IN' → seed7-predefined-variables-face.
Buffer: \"writeln(IN, \\\"hello\\\");\" — 'I' of 'IN' at offset 8."
  ;; "writeln(IN, \"hello\");"
  ;;  0       8
  (seed7-test2--with-fontified-buffer "writeln(IN, \"hello\");"
    (should (seed7-test2--has-face-p (seed7-test2--pos 8)
                                     'seed7-predefined-variables-face))))

(ert-deftest seed7-font-lock/variable-OUT-has-predefined-var-face ()
  "Section V.2 — `OUT' → seed7-predefined-variables-face.
Buffer: \"writeln(OUT, x);\" — 'O' of 'OUT' at offset 8."
  (seed7-test2--with-fontified-buffer "writeln(OUT, x);"
    (should (seed7-test2--has-face-p (seed7-test2--pos 8)
                                     'seed7-predefined-variables-face))))

(ert-deftest seed7-font-lock/variable-STD-OUT-has-predefined-var-face ()
  "Section V.3 — `STD_OUT' → seed7-predefined-variables-face.
Buffer: \"write(STD_OUT, x);\" — 'S' of 'STD_OUT' at offset 6."
  ;; "write(STD_OUT, x);"
  ;;  0     6
  (seed7-test2--with-fontified-buffer "write(STD_OUT, x);"
    (should (seed7-test2--has-face-p (seed7-test2--pos 6)
                                     'seed7-predefined-variables-face))))

;;; --------------------------------------------------------------------------
;;; Section E — Errinfo values → seed7-errinfo-value-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/errinfo-OKAY-has-errinfo-face ()
  "Section E.1 — `OKAY_NO_ERROR' → seed7-errinfo-value-face.
Buffer: \"err := OKAY_NO_ERROR;\" — 'O' at offset 7."
  ;; "err := OKAY_NO_ERROR;"
  ;;  0      7
  (seed7-test2--with-fontified-buffer "err := OKAY_NO_ERROR;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 7)
                                     'seed7-errinfo-value-face))))

(ert-deftest seed7-font-lock/errinfo-RANGE-has-errinfo-face ()
  "Section E.2 — `RANGE_ERROR' → seed7-errinfo-value-face.
Buffer: \"raise RANGE_ERROR;\" — 'R' of 'RANGE_ERROR' at offset 6."
  (seed7-test2--with-fontified-buffer "raise RANGE_ERROR;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 6)
                                     'seed7-errinfo-value-face))))

(ert-deftest seed7-font-lock/errinfo-NUMERIC-has-errinfo-face ()
  "Section E.3 — `NUMERIC_ERROR' → seed7-errinfo-value-face.
Buffer: \"raise NUMERIC_ERROR;\" — 'N' at offset 6."
  (seed7-test2--with-fontified-buffer "raise NUMERIC_ERROR;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 6)
                                     'seed7-errinfo-value-face))))

;;; --------------------------------------------------------------------------
;;; Section O1 — Operator symbols (word) → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Keywords: "and", "or", "not", "div", "mod", "rem", "in", "conv",
;;           "exp", "mult", "times", "lpad", "lpad0", "rpad", "mdiv",
;;           "parse", "radix", "RADIX", "sci", "digits"

(ert-deftest seed7-font-lock/op-and-has-keyword-face ()
  "Section O1.1 — `and' operator → font-lock-keyword-face.
Buffer: \"x and y\" — 'a' of 'and' at offset 2."
  (seed7-test2--with-fontified-buffer "x and y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-or-has-keyword-face ()
  "Section O1.2 — `or' operator → font-lock-keyword-face.
Buffer: \"x or y\" — 'o' of 'or' at offset 2."
  (seed7-test2--with-fontified-buffer "x or y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-not-has-keyword-face ()
  "Section O1.3 — `not' operator → font-lock-keyword-face.
Buffer: \"not x\" — 'n' at offset 0 (word boundary at start of line)."
  (seed7-test2--with-fontified-buffer "not x"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-div-has-keyword-face ()
  "Section O1.4 — `div' operator → font-lock-keyword-face.
Buffer: \"x div y\" — 'd' of 'div' at offset 2."
  (seed7-test2--with-fontified-buffer "x div y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-mod-has-keyword-face ()
  "Section O1.5 — `mod' operator → font-lock-keyword-face.
Buffer: \"x mod y\" — 'm' of 'mod' at offset 2."
  (seed7-test2--with-fontified-buffer "x mod y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O2 — Assignment operator `:=' → font-lock-keyword-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/assignment-op-has-keyword-face ()
  "Section O2 — `:=' → font-lock-keyword-face.
Buffer: \"x := 42;\" — ':' at offset 2."
  ;; "x := 42;"
  ;;  0  2
  (seed7-test2--with-fontified-buffer "x := 42;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O3 — Other predefined operators → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 0): [!?]\\|<<\\|>>\\|><\\|<&

(ert-deftest seed7-font-lock/op-lshift-has-keyword-face ()
  "Section O3.1 — `<<' → font-lock-keyword-face.
Buffer: \"x << 2\" — first '<' at offset 2."
  (seed7-test2--with-fontified-buffer "x << 2"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-rshift-has-keyword-face ()
  "Section O3.2 — `>>' → font-lock-keyword-face.
Buffer: \"x >> 2\" — first '>' at offset 2."
  (seed7-test2--with-fontified-buffer "x >> 2"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-bang-has-keyword-face ()
  "Section O3.3 — `!' → font-lock-keyword-face.
Buffer: \"!x\" — '!' at offset 0."
  (seed7-test2--with-fontified-buffer "!x"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O4 — Comparison operators → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 0): \\(?:[=><]\\|\\(?:<>\\|<=\\|>=\\)\\)

(ert-deftest seed7-font-lock/op-eq-has-keyword-face ()
  "Section O4.1 — `=' → font-lock-keyword-face.
Buffer: \"if x = 0 then\" — '=' at offset 5."
  ;; "if x = 0 then"
  ;;  0    5
  (seed7-test2--with-fontified-buffer "if x = 0 then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-neq-has-keyword-face ()
  "Section O4.2 — `<>' → font-lock-keyword-face.
Buffer: \"if x <> 0 then\" — '<' at offset 5."
  (seed7-test2--with-fontified-buffer "if x <> 0 then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-le-has-keyword-face ()
  "Section O4.3 — `<=' → font-lock-keyword-face.
Buffer: \"if x <= 0 then\" — '<' at offset 5."
  (seed7-test2--with-fontified-buffer "if x <= 0 then"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O5 — Arithmetic operators `*' and `/' → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp: [[:alnum:]_ )]\\([/*]\\)[[:alnum:]_ (]  — group 1

(ert-deftest seed7-font-lock/op-mul-has-keyword-face ()
  "Section O5.1 — `*' in multiplication → font-lock-keyword-face.
Buffer: \"x * y\" — '*' at offset 2."
  (seed7-test2--with-fontified-buffer "x * y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-div-symbol-has-keyword-face ()
  "Section O5.2 — `/' in division → font-lock-keyword-face.
Buffer: \"x / y\" — '/' at offset 2."
  (seed7-test2--with-fontified-buffer "x / y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O6 — Power operator `**' → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp: [[:alnum:] _)]\\(\\*\\*\\)[[:alnum:] _(]  — group 1

(ert-deftest seed7-font-lock/op-power-has-keyword-face ()
  "Section O6 — `**' power operator → font-lock-keyword-face.
Buffer: \"x**2\" — first '*' at offset 1."
  ;; "x**2"
  ;;  0123
  ;;   ^-- first '*' at 1
  (seed7-test2--with-fontified-buffer "x**2"
    (should (seed7-test2--has-face-p (seed7-test2--pos 1)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O7 — Logic operators `&' and `|' → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp: [\n[:alnum:] _)\"']\\([&|]\\)[\n[:alnum:] _(\"']  — group 1

(ert-deftest seed7-font-lock/op-bitand-has-keyword-face ()
  "Section O7.1 — `&' bitwise AND → font-lock-keyword-face.
Buffer: \"x&y\" — '&' at offset 1."
  (seed7-test2--with-fontified-buffer "x&y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 1)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-bitor-has-keyword-face ()
  "Section O7.2 — `|' bitwise OR → font-lock-keyword-face.
Buffer: \"x|y\" — '|' at offset 1."
  (seed7-test2--with-fontified-buffer "x|y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 1)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O8 — Minus / plus operators → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 1): [^+-]\\([+-]\\)[^+-]

(ert-deftest seed7-font-lock/op-minus-has-keyword-face ()
  "Section O8.1 — `-' subtraction → font-lock-keyword-face.
Buffer: \"x - y\" — '-' at offset 2."
  (seed7-test2--with-fontified-buffer "x - y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-plus-has-keyword-face ()
  "Section O8.2 — `+' addition → font-lock-keyword-face.
Buffer: \"x + y\" — '+' at offset 2."
  (seed7-test2--with-fontified-buffer "x + y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 2)
                                     'font-lock-keyword-face))))

;;; --------------------------------------------------------------------------
;;; Section O9 — Tilde `~' and range `..` operators → font-lock-keyword-face
;;; --------------------------------------------------------------------------
;;
;; Regexp: [[:print:]]\\(\\(?:~\\)\\|\\(?:\\.\\.\\)\\)[[:print:]]  — group 1

(ert-deftest seed7-font-lock/op-tilde-has-keyword-face ()
  "Section O9.1 — `~' tilde operator → font-lock-keyword-face.
Buffer: \"x~y\" — '~' at offset 1."
  (seed7-test2--with-fontified-buffer "x~y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 1)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-range-has-keyword-face ()
  "Section O9.2 — `..' range operator → font-lock-keyword-face.
Use identifier context `x..y' to avoid interaction with integer font-lock
rules that fire earlier in `seed7-font-lock-keywords'."
  (seed7-test2--with-fontified-buffer "x..y"
    (should (seed7-test2--has-face-p (seed7-test2--pos 1)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-range-has-keyword-face-2 ()
  "Section O9.2 — `..' range operator → font-lock-keyword-face.
Use identifier context `x..y' to avoid interaction with integer font-lock
rules that fire earlier in `seed7-font-lock-keywords'."
  (seed7-test2--with-fontified-buffer "stri := stri[2 ..];"
    ;;                                 0123456789012345
    (should (seed7-test2--has-face-p (seed7-test2--pos 15)
                                     'font-lock-keyword-face))
    (should (seed7-test2--has-face-p (seed7-test2--pos 16)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-range-has-keyword-face-3 ()
  "Section O9.2 — `..' range operator → font-lock-keyword-face.
Use identifier context `x..y' to avoid interaction with integer font-lock
rules that fire earlier in `seed7-font-lock-keywords'."
  (seed7-test2--with-fontified-buffer "array [0 .. 15] "
    ;;                                 01234567890
    (should (seed7-test2--has-face-p (seed7-test2--pos 9)
                                     'font-lock-keyword-face))
    (should (seed7-test2--has-face-p (seed7-test2--pos 10)
                                     'font-lock-keyword-face))))

(ert-deftest seed7-font-lock/op-range-error-has-warning-face ()
  "Section O9.2 — `..' range operator error → font-lock-warning-face."
  (seed7-test2--with-fontified-buffer "[1..10]"
    ;;                                 012345
    (dolist (pos '(1 2 3 4 5))
      (should (seed7-test2--has-face-p (seed7-test2--pos pos)
                                       'font-lock-warning-face)))))

;;; --------------------------------------------------------------------------
;;; Section W — Invalid char literals → font-lock-warning-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 1): [^'\\]\\(\\(?:''\\)\\|\\(?:'[^\\].+?'\\)\\|\\('\\\\[[:digit:]]+'\\)\\)[^']
;; The simplest invalid form is '' (empty char literal) — two consecutive quotes.

(ert-deftest seed7-font-lock/invalid-char-empty-quotes-has-warning-face ()
  "Section W.1 — `''' (empty char literal) → font-lock-warning-face.
Buffer: \"x := '' ;\"
The regexp needs a non-quote, non-backslash before and after.
The two quotes '' are at offsets 5-6; the face is on group 1 = offsets 5-6."
  ;; "x := '' ;"
  ;;  0    5
  (seed7-test2--with-fontified-buffer "x := '' ;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 5)
                                     'font-lock-warning-face))))

;;; --------------------------------------------------------------------------
;;; Section ID — Name identifiers → seed7-name-identifier-face
;;; --------------------------------------------------------------------------
;;
;; Regexp (group 1): a valid Seed7 name identifier.
;; A name identifier is a word starting with a letter and containing
;; letters, digits, and underscores.

(ert-deftest seed7-font-lock/identifier-has-name-identifier-face ()
  "Section ID.1 — a user-defined identifier → seed7-name-identifier-face.
Buffer: \"myVariable := 1;\" — 'm' at offset 0."
  (seed7-test2--with-fontified-buffer "myVariable := 1;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-name-identifier-face))))

(ert-deftest seed7-font-lock/identifier-with-digits-has-name-identifier-face ()
  "Section ID.2 — identifier with digits → seed7-name-identifier-face.
Buffer: \"f10_fct := 0;\" — 'f' at offset 0."
  (seed7-test2--with-fontified-buffer "f10_fct := 0;"
    (should (seed7-test2--has-face-p (seed7-test2--pos 0)
                                     'seed7-name-identifier-face))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-font-lock-02b)

;;; seed7-test-font-lock-02b.el ends here
