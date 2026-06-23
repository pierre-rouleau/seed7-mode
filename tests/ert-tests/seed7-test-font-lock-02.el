;;; seed7-test-font-lock-02.el --- ERT font-lock tests for all seed7-mode keyword categories  -*- lexical-binding: t; -*-

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
;; ERT regression tests verifying that, after a complete `font-lock-ensure'
;; pass, every entry in `seed7-font-lock-keywords' applies the correct visual
;; face.  Sections are numbered to match the order of entries in that table.
;;
;; Block-comment and line-comment delimiter tests are in
;; `seed7-test-font-lock-01.el'.
;;
;; Design principle:
;;   All tests use `seed7-fl2--face-in' which fontifies a fresh temp buffer,
;;   searches for a literal target string, and returns the `face' text property
;;   at the beginning of the first match.  This avoids brittle hardcoded buffer
;;   offsets.
;;
;; Important font-lock-ordering note:
;;   The keyword "in" appears in BOTH `seed7--declaration-intro-keywords'
;;   (rule fires earlier → face `seed7-intro-statement-keyword-face') and
;;   `seed7--operator-symbols' (rule fires later).  Because the earlier rule
;;   wins and has no override flag, ALL occurrences of "in" receive
;;   `seed7-intro-statement-keyword-face' regardless of context.  This is
;;   tested in Section 8; Section 17 omits "in" for this reason.
;;
;; To run from the command line:
;;
;;   emacs --batch -L . \
;;         -l ert \
;;         -l tests/ert-tests/seed7-test-font-lock-02.el \
;;         --eval '(setq ert-batch-print-length nil ert-batch-print-level nil)' \
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
      (let* ((ert-tests-dir (file-name-directory (expand-file-name this-file))))
        (unless (member ert-tests-dir load-path)
          (push ert-tests-dir load-path))))))

(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Helpers
;;; --------------------------------------------------------------------------

(defun seed7-fl2--face-in (text search-str)
  "Return the face at the first char of SEARCH-STR in a fontified seed7-mode
buffer containing TEXT.  Returns nil if SEARCH-STR is not found or has no face."
  (with-temp-buffer
    (insert text)
    (seed7-mode)
    (syntax-propertize (point-max))
    (font-lock-ensure)
    (goto-char (point-min))
    (when (search-forward search-str nil t)
      (get-text-property (match-beginning 0) 'face))))

(defun seed7-fl2--has-face-p (text search-str expected-face)
  "Non-nil if EXPECTED-FACE is among the face(s) at the start of SEARCH-STR
in a fontified seed7-mode buffer containing TEXT."
  (let ((face (seed7-fl2--face-in text search-str)))
    (cond
     ((null face)   nil)
     ((listp face)  (memq expected-face face))
     (t             (eq face expected-face)))))

;;; --------------------------------------------------------------------------
;;; Section 1 — Pragma keywords (`seed7-pragma-keywords-regexp')
;;;             → seed7-pragma-keyword-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/pragma-library ()
  "\"$ library\" at line start: \"library\" must have seed7-pragma-keyword-face."
  (should (seed7-fl2--has-face-p "$ library" "library" 'seed7-pragma-keyword-face)))

(ert-deftest seed7-font-lock/pragma-trace ()
  "\"$ trace\" at line start: \"trace\" must have seed7-pragma-keyword-face."
  (should (seed7-fl2--has-face-p "$ trace" "trace" 'seed7-pragma-keyword-face)))

(ert-deftest seed7-font-lock/pragma-syntax ()
  "\"$ syntax\" at line start: \"syntax\" must have seed7-pragma-keyword-face."
  (should (seed7-fl2--has-face-p "$ syntax" "syntax" 'seed7-pragma-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 2 — Include directive (`seed7-include-regexp')
;;;             → seed7-include-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/include-dollar-form ()
  "\"$ include\" at line start must give \"include\" seed7-include-face."
  (should (seed7-fl2--has-face-p "$ include \"foo.s7i\"" "include" 'seed7-include-face)))

(ert-deftest seed7-font-lock/include-bare-form ()
  "\"include\" without leading $ at line start must have seed7-include-face."
  (should (seed7-fl2--has-face-p "include \"foo.s7i\"" "include" 'seed7-include-face)))

;;; --------------------------------------------------------------------------
;;; Section 3 — Lead-in statement keywords (`seed7-lead-in-statement-keywords-regexp')
;;;             → seed7-in-statement-keyword-face1
;;;             Keywords: raise  return
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/lead-in-raise ()
  "\"raise\" at the start of a line must have seed7-in-statement-keyword-face1."
  (should (seed7-fl2--has-face-p "raise RANGE_ERROR with \"bad\";"
                                 "raise"
                                 'seed7-in-statement-keyword-face1)))

(ert-deftest seed7-font-lock/lead-in-return ()
  "\"return\" at the start of a line must have seed7-in-statement-keyword-face1."
  (should (seed7-fl2--has-face-p "return result;"
                                 "return"
                                 'seed7-in-statement-keyword-face1)))

(ert-deftest seed7-font-lock/lead-in-return-not-mid-line ()
  "\"return\" embedded inside a line must NOT have seed7-in-statement-keyword-face1.
The lead-in regexp anchors to line start (^ with optional spaces)."
  (should-not (seed7-fl2--has-face-p "x := return_value;"
                                     "return"
                                     'seed7-in-statement-keyword-face1)))

;;; --------------------------------------------------------------------------
;;; Section 4 — In-statement keywords (`seed7-in-statement-keywords-regexp')
;;;             → seed7-in-statement-keyword-face2
;;;             Keywords: is  noop
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/in-stmt-is ()
  "\"is\" preceded by a character and space must have seed7-in-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "x is integer" "is" 'seed7-in-statement-keyword-face2)))

(ert-deftest seed7-font-lock/in-stmt-noop ()
  "\"noop\" preceded by a character and space must have seed7-in-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "x noop" "noop" 'seed7-in-statement-keyword-face2)))

;;; --------------------------------------------------------------------------
;;; Section 5 — Statement-enclosing keywords (`seed7-statement-enclosing-keywords-regexp')
;;;             → seed7-statement-introducing-keyword-face
;;;             E.g.: block  case  when  for  func  if  else  elsif  while  …
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/stmt-enclosing-if ()
  "\"if\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "if x > 0 then"
                                 "if"
                                 'seed7-statement-introducing-keyword-face)))

(ert-deftest seed7-font-lock/stmt-enclosing-else ()
  "\"else\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "else"
                                 "else"
                                 'seed7-statement-introducing-keyword-face)))

(ert-deftest seed7-font-lock/stmt-enclosing-end-if ()
  "\"end if\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "end if"
                                 "end if"
                                 'seed7-statement-introducing-keyword-face)))

(ert-deftest seed7-font-lock/stmt-enclosing-func ()
  "\"func\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "func (in integer: n) return integer is"
                                 "func"
                                 'seed7-statement-introducing-keyword-face)))

(ert-deftest seed7-font-lock/stmt-enclosing-for ()
  "\"for\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "for i range 1 to 10 do"
                                 "for"
                                 'seed7-statement-introducing-keyword-face)))

(ert-deftest seed7-font-lock/stmt-enclosing-while ()
  "\"while\" at line start must have seed7-statement-introducing-keyword-face."
  (should (seed7-fl2--has-face-p "while x > 0 do"
                                 "while"
                                 'seed7-statement-introducing-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 6 — Is-statement keywords (`seed7--is-statement-keywords-regexp')
;;;             → seed7-in-middle-statement-keyword-face1
;;;             Keywords after "is": forward  DYNAMIC  new  sub  action
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/is-stmt-forward ()
  "\"forward\" after \"is\" must have seed7-in-middle-statement-keyword-face1."
  (should (seed7-fl2--has-face-p "const proc: foo is forward;"
                                 "forward"
                                 'seed7-in-middle-statement-keyword-face1)))

(ert-deftest seed7-font-lock/is-stmt-new ()
  "\"new\" after \"is\" must have seed7-in-middle-statement-keyword-face1."
  (should (seed7-fl2--has-face-p "const type: myType is new struct"
                                 "new"
                                 'seed7-in-middle-statement-keyword-face1)))

(ert-deftest seed7-font-lock/is-stmt-action ()
  "\"action\" after \"is\" must have seed7-in-middle-statement-keyword-face1."
  (should (seed7-fl2--has-face-p "const proc: prc is action \"ACT_NAME\";"
                                 "action"
                                 'seed7-in-middle-statement-keyword-face1)))

;;; --------------------------------------------------------------------------
;;; Section 7 — In-middle-statement keywords (`seed7-in-middle-statement-keywords-regexp')
;;;             → seed7-in-middle-statement-keyword-face2
;;;             E.g.: begin  do  downto  local  range  result  step  then  to
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/in-middle-then ()
  "\"then\" preceded by space must have seed7-in-middle-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "if x > 0 then"
                                 "then"
                                 'seed7-in-middle-statement-keyword-face2)))

(ert-deftest seed7-font-lock/in-middle-do ()
  "\"do\" preceded by space must have seed7-in-middle-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "for i range 1 to 10 do"
                                 "do"
                                 'seed7-in-middle-statement-keyword-face2)))

(ert-deftest seed7-font-lock/in-middle-to ()
  "\"to\" preceded by space must have seed7-in-middle-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "for i range 1 to 10 do"
                                 "to"
                                 'seed7-in-middle-statement-keyword-face2)))

(ert-deftest seed7-font-lock/in-middle-result ()
  "\"result\" preceded by spaces must have seed7-in-middle-statement-keyword-face2."
  ;; In a function body, \"result\" is a section keyword preceded by whitespace.
  (should (seed7-fl2--has-face-p "func foo is\n  result"
                                 "result"
                                 'seed7-in-middle-statement-keyword-face2)))

(ert-deftest seed7-font-lock/in-middle-local ()
  "\"local\" preceded by spaces must have seed7-in-middle-statement-keyword-face2."
  (should (seed7-fl2--has-face-p "func foo is\n  local"
                                 "local"
                                 'seed7-in-middle-statement-keyword-face2)))

;;; --------------------------------------------------------------------------
;;; Section 8 — Declaration-intro keywords (`seed7-declaration-intro-keywords-regexp')
;;;             → seed7-intro-statement-keyword-face
;;;             Keywords: attr  const  in  inout  ref  val  var
;;;
;;; Note on "in": it also appears in seed7--operator-symbols but the
;;; declaration-intro rule fires first (earlier in seed7-font-lock-keywords),
;;; so ALL occurrences of "in" receive seed7-intro-statement-keyword-face.
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/decl-intro-const ()
  "\"const\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "  const integer: x is 5;"
                                 "const"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-var ()
  "\"var\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "  var integer: count is 0;"
                                 "var"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-in ()
  "\"in\" must have seed7-intro-statement-keyword-face.
This includes \"in\" used as a set-membership operator, because the
declaration-intro rule fires before the operator-symbols rule."
  (should (seed7-fl2--has-face-p "const proc: foo (in integer: n) is"
                                 "in"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-inout ()
  "\"inout\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "(inout integer: x)"
                                 "inout"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-ref ()
  "\"ref\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "(ref integer: x)"
                                 "ref"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-val ()
  "\"val\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "(val integer: x)"
                                 "val"
                                 'seed7-intro-statement-keyword-face)))

(ert-deftest seed7-font-lock/decl-intro-attr ()
  "\"attr\" must have seed7-intro-statement-keyword-face."
  (should (seed7-fl2--has-face-p "attr myType: x is"
                                 "attr"
                                 'seed7-intro-statement-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 9 — Predefined types (`seed7-predefined-types-regexp')
;;;             → font-lock-type-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/type-integer ()
  "\"integer\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "const integer: x is 0;"
                                 "integer"
                                 'font-lock-type-face)))

(ert-deftest seed7-font-lock/type-string ()
  "\"string\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "var string: s is \"\";"
                                 "string"
                                 'font-lock-type-face)))

(ert-deftest seed7-font-lock/type-boolean ()
  "\"boolean\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "var boolean: flag is FALSE;"
                                 "boolean"
                                 'font-lock-type-face)))

(ert-deftest seed7-font-lock/type-char ()
  "\"char\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "var char: c is 'a';"
                                 "char"
                                 'font-lock-type-face)))

(ert-deftest seed7-font-lock/type-float ()
  "\"float\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "var float: x is 0.0;"
                                 "float"
                                 'font-lock-type-face)))

(ert-deftest seed7-font-lock/type-array ()
  "\"array\" must have font-lock-type-face."
  (should (seed7-fl2--has-face-p "var array integer: arr is 0 times 0;"
                                 "array"
                                 'font-lock-type-face)))

;;; --------------------------------------------------------------------------
;;; Section 10 — Invalid float numbers (`seed7-float-number-invalid1-re',
;;;              `seed7-float-number-invalid2-re')
;;;              → font-lock-warning-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/float-invalid-leading-dot ()
  "\".42\" (no integer part) must have font-lock-warning-face.
Pattern: [^[:alnum:]](\\.[[:digit:]]+) — group 1 = \".42\"."
  (should (seed7-fl2--has-face-p "x := .42;" ".42" 'font-lock-warning-face)))

(ert-deftest seed7-font-lock/float-invalid-trailing-dot ()
  "\"42.\" (no fractional part) must have font-lock-warning-face.
Pattern: [^[:alnum:]]([[:digit:]]+\\.)[^[:alnum:]] — group 1 = \"42.\"."
  ;; The \";\" after \"42.\" satisfies [^[:alnum:]] on the right.
  (should (seed7-fl2--has-face-p "x := 42.;" "42." 'font-lock-warning-face)))

;;; --------------------------------------------------------------------------
;;; Section 11 — Valid float numbers (`seed7-float-number-re')
;;;              → seed7-float-face  (group 0 = whole match)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/float-simple ()
  "\"3.14\" must have seed7-float-face on its first digit."
  ;; Group 0 covers the whole match; checking the first character suffices.
  (should (seed7-fl2--has-face-p "x := 3.14;" "3" 'seed7-float-face)))

(ert-deftest seed7-font-lock/float-with-positive-exponent ()
  "\"1.0e10\" must have seed7-float-face."
  (should (seed7-fl2--has-face-p "x := 1.0e10;" "1" 'seed7-float-face)))

;;; --------------------------------------------------------------------------
;;; Section 12 — Numbers with exponents
;;;
;;; `seed7-number-with-negative-exponent-re' → font-lock-warning-face
;;; `seed7-number-with-exponent-re'          → seed7-integer-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/number-negative-exponent-is-warning ()
  "\"1e-5\" (integer literal with negative exponent) must have font-lock-warning-face.
Negative exponents are invalid on integer literals in Seed7."
  (should (seed7-fl2--has-face-p "x := 1e-5;" "1e-5" 'font-lock-warning-face)))

(ert-deftest seed7-font-lock/number-positive-exponent ()
  "\"1e5\" (integer literal with positive exponent) must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 1e5;" "1e5" 'seed7-integer-face)))

;;; --------------------------------------------------------------------------
;;; Section 13 — Base-N integer literals
;;;
;;; `seed7-base-x-big-integer-re' → seed7-big-integer-face
;;; `seed7-base-x-integer-re'     → seed7-integer-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/base-x-integer-hex ()
  "\"16#FF\" hexadecimal literal must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 16#FF;" "16#FF" 'seed7-integer-face)))

(ert-deftest seed7-font-lock/base-x-integer-binary ()
  "\"2#1010\" binary literal must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 2#1010;" "2#1010" 'seed7-integer-face)))

(ert-deftest seed7-font-lock/base-x-integer-octal ()
  "\"8#777\" octal literal must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 8#777;" "8#777" 'seed7-integer-face)))

(ert-deftest seed7-font-lock/base-x-big-integer-hex ()
  "\"16#FF_\" base-x big integer (trailing underscore) must have seed7-big-integer-face."
  (should (seed7-fl2--has-face-p "x := 16#FF_;" "16#FF_" 'seed7-big-integer-face)))

;;; --------------------------------------------------------------------------
;;; Section 14 — Predefined constants (`seed7-predefined-constants-regexp')
;;;              → font-lock-constant-face
;;;              Values: E  EOF  FALSE  Infinity  NIL  NaN  PI  TRUE  empty
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/constant-TRUE ()
  "\"TRUE\" must have font-lock-constant-face."
  (should (seed7-fl2--has-face-p "x := TRUE;" "TRUE" 'font-lock-constant-face)))

(ert-deftest seed7-font-lock/constant-FALSE ()
  "\"FALSE\" must have font-lock-constant-face."
  (should (seed7-fl2--has-face-p "x := FALSE;" "FALSE" 'font-lock-constant-face)))

(ert-deftest seed7-font-lock/constant-NIL ()
  "\"NIL\" must have font-lock-constant-face."
  (should (seed7-fl2--has-face-p "x := NIL;" "NIL" 'font-lock-constant-face)))

(ert-deftest seed7-font-lock/constant-PI ()
  "\"PI\" must have font-lock-constant-face."
  (should (seed7-fl2--has-face-p "r := PI * radius;" "PI" 'font-lock-constant-face)))

(ert-deftest seed7-font-lock/constant-E ()
  "\"E\" (Euler's number) must have font-lock-constant-face.
The float-number rules fire before predefined-constants, but a standalone
\"E\" does not match any float pattern so it receives the constant face."
  (should (seed7-fl2--has-face-p "x := E;" "E" 'font-lock-constant-face)))

;;; --------------------------------------------------------------------------
;;; Section 15 — Predefined variables (`seed7-predefined-variables-regexp')
;;;              → seed7-predefined-variables-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/predvar-STD_OUT ()
  "\"STD_OUT\" must have seed7-predefined-variables-face."
  (should (seed7-fl2--has-face-p "write(STD_OUT, \"hello\");"
                                 "STD_OUT"
                                 'seed7-predefined-variables-face)))

(ert-deftest seed7-font-lock/predvar-STD_IN ()
  "\"STD_IN\" must have seed7-predefined-variables-face."
  (should (seed7-fl2--has-face-p "c := getc(STD_IN);"
                                 "STD_IN"
                                 'seed7-predefined-variables-face)))

(ert-deftest seed7-font-lock/predvar-OUT ()
  "\"OUT\" must have seed7-predefined-variables-face."
  (should (seed7-fl2--has-face-p "writeln(OUT, msg);"
                                 "OUT"
                                 'seed7-predefined-variables-face)))

;;; --------------------------------------------------------------------------
;;; Section 16 — Predefined errinfo values (`seed7-errinfo-values-regexp')
;;;              → seed7-errinfo-value-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/errinfo-RANGE_ERROR ()
  "\"RANGE_ERROR\" must have seed7-errinfo-value-face."
  (should (seed7-fl2--has-face-p "raise RANGE_ERROR;"
                                 "RANGE_ERROR"
                                 'seed7-errinfo-value-face)))

(ert-deftest seed7-font-lock/errinfo-MEMORY_ERROR ()
  "\"MEMORY_ERROR\" must have seed7-errinfo-value-face."
  (should (seed7-fl2--has-face-p "raise MEMORY_ERROR;"
                                 "MEMORY_ERROR"
                                 'seed7-errinfo-value-face)))

(ert-deftest seed7-font-lock/errinfo-OKAY_NO_ERROR ()
  "\"OKAY_NO_ERROR\" must have seed7-errinfo-value-face."
  (should (seed7-fl2--has-face-p "err := OKAY_NO_ERROR;"
                                 "OKAY_NO_ERROR"
                                 'seed7-errinfo-value-face)))

;;; --------------------------------------------------------------------------
;;; Section 17 — Operator word symbols (`seed7-operator-symbols-regexp')
;;;              → font-lock-keyword-face
;;;              Note: "in" is intentionally omitted — see Commentary.
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-and ()
  "\"and\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a and b;" "and" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-or ()
  "\"or\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a or b;" "or" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-not ()
  "\"not\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := not b;" "not" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-div ()
  "\"div\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "q := a div b;" "div" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-mod ()
  "\"mod\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "r := a mod b;" "mod" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-rem ()
  "\"rem\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "r := a rem b;" "rem" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-times ()
  "\"times\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "arr := 5 times 0;" "times" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 18 — Assignment operators (`seed7-predef-assignment-operator-regexp')
;;;              → font-lock-keyword-face  (group 0 = whole match)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/assign-op-simple ()
  "\":=\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := 5;" ":=" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/assign-op-plus ()
  "\"+:=\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x +:= 1;" "+:=" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/assign-op-shift-left ()
  "\"<<:=\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x <<:= 2;" "<<:=" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/assign-op-concat ()
  "\"&:=\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "s &:= \"more\";" "&:=" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 19 — Other predefined operators (`seed7-other-predef-operator-regexp')
;;;              → font-lock-keyword-face  (group 0)
;;;              Operators: !  ?  <<  >>  ><  <&
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-shift-left ()
  "\"<<\" shift-left operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a << 2;" "<<" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-shift-right ()
  "\">>\" shift-right operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a >> 2;" ">>" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-xor ()
  "\"><\" XOR operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a >< b;" "><" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 20 — Predefined comparison operators
;;;              (`seed7-predef-comparison-operator-regexp')
;;;              → font-lock-keyword-face  (group 0)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-equal ()
  "\"=\" equality operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "if x = y then" "=" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-not-equal ()
  "\"<>\" not-equal operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "if x <> y then" "<>" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-less-than ()
  "\"<\" less-than operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "if x < y then" "<" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-greater-equal ()
  "\">=\" greater-or-equal operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "if x >= y then" ">=" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 21 — Arithmetic operators (`seed7-arithmetic-operator-regexp',
;;;              inline `/' rule, inline `**' rule)
;;;              → font-lock-keyword-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-multiply ()
  "\"*\" arithmetic multiplication must have font-lock-keyword-face.
Pattern: [[:alnum:]_ )]([/*])[[:alnum:]_ (] — group 1 = operator char."
  (should (seed7-fl2--has-face-p "x := a * b;" "*" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-divide ()
  "\"/\" arithmetic division must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a / b;" "/" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-power ()
  "\"**\" power operator must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a ** 2;" "**" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 22 — Logic operators `&' and `|' (inline rules)
;;;              → font-lock-keyword-face
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-bitand ()
  "\"&\" bitwise-AND must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a & b;" "&" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-bitor ()
  "\"|\" bitwise-OR must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a | b;" "|" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 23 — Invalid single-quote char literals
;;;              (`seed7--invalid-char-literal-re')
;;;              → font-lock-warning-face  (group 1)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/invalid-char-empty-quotes ()
  "\"''\" (empty char literal) must have font-lock-warning-face.
Seed7 char literals require exactly one character between single quotes."
  (should (seed7-fl2--has-face-p "x := '';" "''" 'font-lock-warning-face)))

(ert-deftest seed7-font-lock/invalid-char-multi-chars ()
  "\"'ab'\" (multi-char literal) must have font-lock-warning-face."
  (should (seed7-fl2--has-face-p "x := 'ab';" "'ab'" 'font-lock-warning-face)))

;;; --------------------------------------------------------------------------
;;; Section 24 — Identifiers (`seed7-name-identifier-re')
;;;              → seed7-name-identifier-face  (group 1)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/identifier-simple ()
  "A plain alphabetic identifier must have seed7-name-identifier-face."
  (should (seed7-fl2--has-face-p "myVar := 0;" "myVar" 'seed7-name-identifier-face)))

(ert-deftest seed7-font-lock/identifier-with-underscore ()
  "An identifier containing underscores must have seed7-name-identifier-face."
  (should (seed7-fl2--has-face-p "my_func(x);" "my_func" 'seed7-name-identifier-face)))

(ert-deftest seed7-font-lock/identifier-with-digits ()
  "An identifier containing embedded digits must have seed7-name-identifier-face.
Note: the identifier rule fires before the integer rule, so \"foo42\" is
recognised as an identifier, not a number prefix."
  (should (seed7-fl2--has-face-p "foo42 := 1;" "foo42" 'seed7-name-identifier-face)))

;;; --------------------------------------------------------------------------
;;; Section 25 — Big integers (`seed7-big-integer-re')
;;;              → seed7-big-integer-face  (group 1)
;;;              Pattern: optional-base [0-9]+_
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/big-integer-trailing-underscore ()
  "A decimal integer with trailing underscore must have seed7-big-integer-face.
\"1000_\" is the Seed7 big-integer notation for 1000."
  (should (seed7-fl2--has-face-p "x := 1000_;" "1000_" 'seed7-big-integer-face)))

(ert-deftest seed7-font-lock/big-integer-minimal ()
  "\"1_\" (minimal big integer) must have seed7-big-integer-face."
  (should (seed7-fl2--has-face-p "x := 1_;" "1_" 'seed7-big-integer-face)))

;;; --------------------------------------------------------------------------
;;; Section 26 — Regular integers (`seed7-integer-re')
;;;              → seed7-integer-face  (group 1)
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/integer-simple ()
  "A plain decimal integer must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 42;" "42" 'seed7-integer-face)))

(ert-deftest seed7-font-lock/integer-zero ()
  "\"0\" must have seed7-integer-face."
  (should (seed7-fl2--has-face-p "x := 0;" "0" 'seed7-integer-face)))

;;; --------------------------------------------------------------------------
;;; Section 27 — Arithmetic minus and plus operators
;;;              (`seed7-minus-operator-regexp')
;;;              → font-lock-keyword-face  (group 1)
;;;              Pattern: [^+-]([+-])[^+-]
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-minus ()
  "\"-\" arithmetic subtraction must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a - b;" "-" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-plus ()
  "\"+\" arithmetic addition must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a + b;" "+" 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------
;;; Section 28 — Tilde `~' and range `..` (inline rules)
;;;              → font-lock-keyword-face  (group 1)
;;;              Pattern: [[:print:]]([~]|(\.\.))[[:print:]]
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/op-tilde ()
  "\"~\" must have font-lock-keyword-face."
  (should (seed7-fl2--has-face-p "x := a~b;" "~" 'font-lock-keyword-face)))

(ert-deftest seed7-font-lock/op-range ()
  "\"..\" range operator must have font-lock-keyword-face.
Uses plain identifier context \"a..b\" to avoid conflict with integer or
keyword rules that fire earlier in seed7-font-lock-keywords with override.
Pattern: [[:print:]]([~]|(\\.\\.))[[:print:]] — group 1 = the operator."
  (should (seed7-fl2--has-face-p "x := a..b;" ".." 'font-lock-keyword-face)))

;;; --------------------------------------------------------------------------

(provide 'seed7-test-font-lock-02)

;;; seed7-test-font-lock-02.el ends here
