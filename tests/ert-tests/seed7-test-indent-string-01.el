;;; seed7-test-indent-string-01.el --- ERT tests: string-line indentation.  -*- lexical-binding: t; -*-

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
;; Regression tests for string-line indentation in `seed7-calc-indent',
;; introduced/stabilised in PR#107.  The tests exercise every branch of the
;; `((seed7-line-isa-string 0) ...)' cond arm, using minimal self-contained
;; Seed7 code fixtures embedded directly as strings.
;;
;; Branches under test (matched in priority order by seed7-calc-indent):
;;
;;   Case 1 — ASSIGNMENT-OP CONTINUATION
;;     Current line starts with `"'.  Previous non-empty line ends with an
;;     assignment operator (`:=', `&:=', etc.).
;;     → indent = column-of-operator + seed7-indent-width
;;
;;   Case 2 — CONSECUTIVE STRING LINES
;;     Current line starts with `"'.  Previous non-empty line also starts
;;     with `"'.
;;     → indent = column of the leading `"' on the previous line
;;
;;   Case 3 — `<&' END WITH QUOTE ON PREVIOUS LINE
;;     Current line starts with `"'.  Previous non-empty line ends with `<&'
;;     AND that previous line itself contains a `"'.
;;     → indent = column of the first `"' found on the previous line
;;     This is the primary pattern from bas7.sd7 around line 696 and similar
;;     files that build strings with `<&' multi-line concatenation.
;;
;;   Case 4 — `&' END WITH ASSIGNMENT ON PREVIOUS LINE
;;     Current line starts with `"'.  Previous non-empty line ends with `&'
;;     (plain or `<&') AND that line contains an assignment operator.
;;     → indent = column of the first non-whitespace char after the operator
;;     (the RHS start column of the assignment expression).
;;     Models lib/bmp.s7i line 888:
;;       stri &:= bytes(rawDataSize + 54, UNSIGNED, LE, 4) &
;;                "\0\0" mult 4  &          ← aligns to column of `bytes('
;;
;;   Case 5 — FALLBACK: previous non-string line indentation
;;     Current line starts with `"'.  None of the above matched.
;;     → indent = indentation of the nearest preceding non-string line.
;;
;; Fixture line layout (1-based) is documented inside each defconst below.
;;
;; To run from the command line:
;;
;;   emacs --batch -l seed7-mode.el \
;;         -l tests/ert-tests/seed7-test-indent-string-01.el \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Shared test helper
;;

(defmacro seed7-test-indent--with-buffer (code &rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing CODE string."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,code)
     (seed7-mode)
     ,@body))

(defun seed7-test-indent--goto-bol (line)
  "Move point to the beginning of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line)))

;;; --------------------------------------------------------------------------
;;; Case 1 — Assignment-operator continuation
;;
;; Fixture layout:
;;   1  (blank — buffer starts with newline for 1-based indexing)
;;   2  const string: textA is
;;   3    stri :=
;;   4    "some text" & randDigitStri(length);
;;
;; Target line: 4.
;; Line 3 ends with `:='.  The `:=' token starts at column 9
;; (2 leading spaces + "stri " = 7 chars, then `:' at column 7).
;;
;;   col: 0123456789...
;;        "  stri :="
;;                 ^--- `:' at column 7, so seed7-line-ends-with returns 7
;;
;; Expected indentation of line 4 = 7 + seed7-indent-width = 7 + 2 = 9.
;;
(defconst seed7-test-indent--case1-code
  "\
const string: textA is\n\
  stri :=\n\
          \"some text\" & randDigitStri(length);\n"
  "Fixture: string-line after a bare assignment operator continuation.
Line layout (1-based):
  1  const string: textA is
  2    stri :=
  3    \"some text\" & randDigitStri(length);")

(ert-deftest seed7-indent-string/case1-assignment-op-continuation ()
  "Case 1: line starting with `\"' after prev line ending with `:='.
`seed7-calc-indent' must return column-of-`:=' + seed7-indent-width.
Exercises: bas7.sd7-style `stri :=\\n\"text\"...' patterns."
  (seed7-test-indent--with-buffer seed7-test-indent--case1-code
    ;; Line 2 is `  stri :='.  The `:=' starts at column 7.
    ;; Expected indent for line 3 = 7 + 2 = 9.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 9))))

;;; --------------------------------------------------------------------------
;;; Case 2 — Consecutive string lines
;;
;; Fixture layout:
;;   1  const string: msg is
;;   2    "first part of the message" <&
;;   3    "second part";
;;
;; Target line: 3.
;; Line 2 starts with `"' at column 4 (4 leading spaces).
;; Expected indent = column 4 (align to the leading `"' of line 2).
;;
(defconst seed7-test-indent--case2-code
  "\
const string: msg is\n\
    \"first part of the message\" <&\n\
    \"second part\";\n"
  "Fixture: two consecutive string lines separated by `<&'.
Line layout (1-based):
  1  const string: msg is
  2      \"first part of the message\" <&
  3      \"second part\";")

(ert-deftest seed7-indent-string/case2-consecutive-string-lines ()
  "Case 2: line starting with `\"' after another string line.
`seed7-calc-indent' must return the column of the leading `\"' on the
previous string line (column 4 in the fixture)."
  (seed7-test-indent--with-buffer seed7-test-indent--case2-code
    ;; Line 2: `    "first part...' — leading `"' at column 4.
    ;; Line 3 should also be indented to column 4.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 4))))

;;; --------------------------------------------------------------------------
;;; Case 3 — `<&' end with quote on previous line
;;
;; This is the primary pattern from bas7.sd7 around line 696 and from
;; lib/bmp.s7i (bitfield formatting with `<&').
;;
;; Fixture layout:
;;   1  const string: result is
;;   2    return "(" <& bitfield.mask radix 2 lpad0 32 <&
;;   3           ", " <& bitfield.rShift lpad 2 <&
;;   4           ", " <& bitfield.scale <& ")";
;;
;; Target line: 3 (first continuation).
;; Line 2: `  return "(" <& ...' ends with `<&' AND contains `"' at column 9.
;;   col:  0123456789...
;;         "  return \"(\" <& ..."
;;                   ^--- `"' at column 9
;; Expected indent of line 3 = 9 (search-forward `"' → column 10, 1- → 9).
;;
;; Target line: 4 (second continuation, prev is line 3 which starts with `"').
;; Line 3: `         ", " <& ...' starts with `"' at column 9.
;; This now falls into Case 2 (consecutive string lines), also giving column 9.
;;
(defconst seed7-test-indent--case3-code
  "\
const string: result is\n\
  return \"(\" <& bitfield.mask radix 2 lpad0 32 <&\n\
         \", \" <& bitfield.rShift lpad 2 <&\n\
         \", \" <& bitfield.scale <& \")\";\n"
  "Fixture: multi-line `<&' concatenation with a `\"' on the first line.
Exercises the `<&'+`\"' alignment case from bas7.sd7 line 696 area.
Line layout (1-based):
  1  const string: result is
  2    return \"(\" <& bitfield.mask radix 2 lpad0 32 <&
  3           \", \" <& bitfield.rShift lpad 2 <&
  4           \", \" <& bitfield.scale <& \")\";")

(ert-deftest seed7-indent-string/case3-angle-concat-with-quote ()
  "Case 3: line starting with `\"' after prev ending with `<&' that has a `\"'.
`seed7-calc-indent' must align the `\"' to the column of the first `\"' on
the previous line.  This exercises the bas7.sd7-around-line-696 pattern."
  (seed7-test-indent--with-buffer seed7-test-indent--case3-code
    ;; Line 2: `  return "(" <& ...' — `"' at column 9, ends with `<&'.
    ;; Case 3 fires: expected indent for line 3 = 9.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 9))
    ;; Line 3 now starts with `"' at column 9 and also ends with `<&'.
    ;; Case 2 (consecutive string lines) fires for line 4: expected = 9.
    (seed7-test-indent--goto-bol 4)
    (should (= (seed7-calc-indent) 9))))

;;; --------------------------------------------------------------------------
;;; Case 3b — `<&' end, previous line has NO `"' (case 3 does NOT fire)
;;
;; When the previous line ends with `<&' but contains no `"', case 3 does
;; NOT fire.  If that previous line also has no assignment operator, cases
;; 3 and 4 both miss and the fallback (case 5) takes over.
;;
;; Fixture layout:
;;   1  const string: header is
;;   2    writeln(result <&
;;   3    "suffix");
;;
;; Target line: 3.
;; Line 2: `  writeln(result <&' ends with `<&' but has no `"'.
;; Line 2 also contains no assignment operator.
;; Fallback (case 5): look for previous non-string line.
;; The previous non-string line is line 2 `  writeln(result <&': indented 2.
;; Expected: 2.
;;
(defconst seed7-test-indent--case3b-code
  "\
const string: header is\n\
  writeln(result <&\n\
  \"suffix\");\n"
  "Fixture: `<&' on prev line but no `\"' on that line, no assignment op.
The fallback must use the indentation of the nearest preceding non-string line.
Line layout (1-based):
  1  const string: header is
  2    writeln(result <&
  3    \"suffix\");")

(ert-deftest seed7-indent-string/case3b-angle-concat-no-quote-on-prev ()
  "Case 3 does NOT fire when prev ends with `<&' but has no `\"'.
The fallback (case 5) must return the indentation of the previous
non-string line (column 2 in the fixture)."
  (seed7-test-indent--with-buffer seed7-test-indent--case3b-code
    ;; Line 2 is `  writeln(result <&': indented at column 2, no `"'.
    ;; Cases 1,2,3 do not fire; case 4 does not fire (no assignment op).
    ;; Fallback returns indentation of line 2 = 2.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 2))))

;;; --------------------------------------------------------------------------
;;; Case 4 — `&' end with assignment operator on previous line
;;
;; Models lib/bmp.s7i line 888:
;;   stri &:= bytes(rawDataSize + 54, UNSIGNED, LE, 4) &
;;            "\0\0" mult 4  &
;;
;; Fixture layout:
;;   1  const string: bmpHeader is
;;   2    stri &:= bytes(rawDataSize, UNSIGNED, LE, 4) &
;;   3             "PAD" mult 4 &
;;   4             "HDR";
;;
;; Target line: 3.
;; Line 2: `  stri &:= bytes(rawDataSize, UNSIGNED, LE, 4) &'
;;   - ends with plain `&' (not `<&'), so case 3 does not fire.
;;   - Contains assignment op `&:=', so case 4 fires.
;;   - `&:=' starts at column 7 (`  stri ` → `s' at col 2, `&' at col 7).
;;   - After matching `&:=', point is at col 10 (after `=').
;;   - `(skip-chars-forward " \t")' skips the space → `b' of `bytes(' at col 11.
;;
;;   col: 0  1  2  3  4  5  6  7  8  9 10 11 ...
;;        ` '` ''s''t''r''i'' ''&'':''='' ''b'...
;;                               ^           ^
;;                               &:= at 7    b at 11
;;
;; Expected indent of line 3 = 11.
;;
(defconst seed7-test-indent--case4-code
  "\
const string: bmpHeader is\n\
  stri &:= bytes(rawDataSize, UNSIGNED, LE, 4) &\n\
           \"PAD\" mult 4 &\n\
           \"HDR\";\n"
  "Fixture: `&' continuation after a line with both `&:=' and `bytes('.
Exercises the bmp.s7i-line-888-style RHS-alignment case.
Line layout (1-based):
  1  const string: bmpHeader is
  2    stri &:= bytes(rawDataSize, UNSIGNED, LE, 4) &
  3             \"PAD\" mult 4 &
  4             \"HDR\";")

(ert-deftest seed7-indent-string/case4-amp-end-with-assignment ()
  "Case 4: string line after prev ending with `&', prev has assignment op.
`seed7-calc-indent' must return the column of the first non-whitespace
character after the assignment operator on the previous line (RHS start).
Models lib/bmp.s7i line 888."
  (seed7-test-indent--with-buffer seed7-test-indent--case4-code
    ;; Line 2: `  stri &:= bytes(...)  &' — `b' of `bytes(' at column 11.
    ;; Case 4 fires: expected indent for line 3 = 11.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 11))
    ;; Line 3 starts with `"' at column 11, also ends with `&'.
    ;; Line 3 has no assignment op → case 4 does NOT fire for line 4.
    ;; Line 3 DOES start with `"' → case 2 (consecutive strings) fires.
    ;; Expected indent for line 4 = 11 (same `"' column as line 3).
    (seed7-test-indent--goto-bol 4)
    (should (= (seed7-calc-indent) 11))))

;;; --------------------------------------------------------------------------
;;; Case 5 — Fallback: previous non-string line indentation
;;
;; When none of cases 1–4 apply, `seed7-calc-indent' uses the indentation
;; of the nearest preceding non-blank, non-comment, non-string line.
;;
;; Fixture layout:
;;   1  const string: banner is
;;   2    someFunc(arg) &
;;   3    "suffix";
;;
;; Target line: 3.
;; Line 2: `  someFunc(arg) &' ends with `&' but has no assignment op and
;; no `"' in it, so cases 1–4 all fail.
;; Fallback: use indentation of line 2 = 2.
;;
(defconst seed7-test-indent--case5-code
  "\
const string: banner is\n\
  someFunc(arg) &\n\
  \"suffix\";\n"
  "Fixture: string line after a plain `&' continuation with no assignment op.
The fallback must use the indentation of the nearest non-string line.
Line layout (1-based):
  1  const string: banner is
  2    someFunc(arg) &
  3    \"suffix\";")

(ert-deftest seed7-indent-string/case5-fallback-non-string-indent ()
  "Case 5 (fallback): string line when cases 1–4 do not apply.
`seed7-calc-indent' must return the indentation of the nearest preceding
non-string, non-blank, non-comment line."
  (seed7-test-indent--with-buffer seed7-test-indent--case5-code
    ;; Line 2 `  someFunc(arg) &': indented at column 2.
    ;; Cases 1–4 do not fire.  Fallback returns column 2.
    (seed7-test-indent--goto-bol 3)
    (should (= (seed7-calc-indent) 2))))

;;; --------------------------------------------------------------------------
;;; Integration — seed7-indent-line modifies the buffer correctly
;;
;; Verify that calling `seed7-indent-line' (the interactive command that
;; dispatches to `seed7-calc-indent') actually re-indents the line in place.
;;
(defconst seed7-test-indent--integ-code
  "\
const string: result is\n\
  return \"(\" <& bitfield.mask radix 2 lpad0 32 <&\n\
\", \" <& bitfield.rShift <& \")\";\n"
  "Fixture: line 3 is deliberately mis-indented (column 0).
After `seed7-indent-line', it must move to column 9 (aligned to the `\"'
on line 2).
Line layout (1-based):
  1  const string: result is
  2    return \"(\" <& bitfield.mask radix 2 lpad0 32 <&
  3  \", \" <& bitfield.rShift <& \")\";     ← WRONG: should be at column 9")

(ert-deftest seed7-indent-string/integration-indent-line-fixes-column ()
  "Integration: `seed7-indent-line' corrects a mis-indented string line.
Line 3 starts at column 0; after indentation it must be at column 9,
matching the `\"' position on the preceding `<&' line."
  (seed7-test-indent--with-buffer seed7-test-indent--integ-code
    ;; Confirm the fixture is intentionally mis-indented.
    (seed7-test-indent--goto-bol 3)
    (should (= (current-indentation) 0))
    ;; Apply indentation.
    (seed7-indent-line)
    ;; The `"' should now be at column 9.
    (seed7-test-indent--goto-bol 3)
    (should (= (current-indentation) 9))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-indent-string-01)

;;; seed7-test-indent-string-01.el ends here