;;; seed7-test-nav-nested-01.el --- ERT regression tests: nested callable navigation.  -*- lexical-binding: t; -*-

;; Created   : Saturday, June 13 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-17 10:49:27 EDT, updated by Pierre Rouleau>

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
;; Regression tests for `seed7-end-of-defun' / `seed7-beg-of-defun'.
;;
;; IMPORTANT: Forward tests call `seed7-end-of-defun' directly, NOT via
;; `end-of-defun' (the Emacs built-in wrapper).  The Emacs built-in:
;;   1. Pre-positions point using `(end-of-line)' + `(beginning-of-defun-raw)'
;;      before calling `end-of-defun-function', so the seed7 function runs from
;;      a location different from the test's starting line.
;;   2. Unconditionally does `(unless (eobp) (forward-line 1))' after the call,
;;      shifting the result one line further.
;; Both effects make the expected line numbers unpredictable from the test.
;;
;; Backward tests call `beginning-of-defun' (via `beginning-of-defun-function')
;; which has no such pre-processing and is safe to use.
;;
;; The Seed7 fixture code is an excerpt from chkbig.sd7 (original lines 35-74)
;; embedded directly in this file so the test is fully self-contained.
;;
;; Line-number correspondence:
;;   embedded line N  =  original chkbig.sd7 line (N + 34)
;;
;; Test matrix (embedded line numbers):
;;
;;   forward navigation (seed7-end-of-defun from BOL):
;;     line  2 → line  7   (long func bigintExpr: end func;)
;;     line 10 → line 11   (short func intExpr: return …;)
;;     line 14 → line 15   (short func striExpr: return …;)
;;     line 18 → line 19   (short func boolExpr: return …;)
;;     line 22 → line 39   (proc DECLARE_RAISES_RANGE_ERROR: outer end func;)
;;     line 25 → line 37   (nested func raisesRangeError: inner end func;)
;;
;;   backward navigation (beginning-of-defun from EOL):
;;     line  7 → line  2   (end func; of bigintExpr)
;;     line 11 → line 10   (return of intExpr)
;;     line 15 → line 14   (return of striExpr)
;;     line 19 → line 18   (return of boolExpr)
;;     line 37 → line 25   (end func; of raisesRangeError)
;;     line 39 → line 22   (end func; of DECLARE_RAISES_RANGE_ERROR)

;; To run from the command line use:
;;
;;      emacs --batch -l seed7-mode.el \
;;            -l tests/ert-tests/seed7-test-nav-nested-01.el \
;;            -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Fixture
;;

(defconst seed7-test--nested-nav-code
  ;; Line 1  — blank (mirrors original chkbig.sd7 line 35)
  "\n\
const func bigInteger: bigintExpr (in bigInteger: number) is func\n\
  result\n\
    var bigInteger: exprResult is 0_;\n\
  begin\n\
    exprResult := number;\n\
  end func;\n\
\n\
\n\
const func integer: intExpr (in integer: number) is\n\
  return number + length(str(rand(1, 9))[2 ..]);\n\
\n\
\n\
const func string: striExpr (in string: stri) is\n\
  return stri & str(rand(1, 9))[2 ..];\n\
\n\
\n\
const func boolean: boolExpr (in boolean: okay) is\n\
  return boolean(ord(okay) + length(str(rand(1, 9))[2 ..]));\n\
\n\
\n\
const proc: DECLARE_RAISES_RANGE_ERROR (in type: aType) is func\n\
  begin\n\
\n\
    const func boolean: raisesRangeError (in func aType: expression) is func\n\
      result\n\
        var boolean: raisesRangeError is FALSE;\n\
      local\n\
        var aType: exprResult is aType.value;\n\
      begin\n\
        block\n\
          exprResult := expression;\n\
        exception\n\
          catch RANGE_ERROR:\n\
            raisesRangeError := TRUE;\n\
        end block;\n\
      end func;\n\
\n\
  end func;\n\
\n"
  "Seed7 fixture code for nested callable navigation regression tests.
Corresponds to chkbig.sd7 lines 35-74; embedded-line N = original-line (N+34).

Line layout (1-based):
  1   blank
  2   const func bigInteger: bigintExpr … is func
  3     result
  4       var bigInteger: exprResult is 0_;
  5     begin
  6       exprResult := number;
  7     end func;
  8   blank
  9   blank
 10   const func integer: intExpr … is
 11     return …;
 12   blank
 13   blank
 14   const func string: striExpr … is
 15     return …;
 16   blank
 17   blank
 18   const func boolean: boolExpr … is
 19     return …;
 20   blank
 21   blank
 22   const proc: DECLARE_RAISES_RANGE_ERROR … is func
 23     begin
 24   blank
 25       const func boolean: raisesRangeError … is func
 26         result
 27           var boolean: raisesRangeError is FALSE;
 28         local
 29           var aType: exprResult is aType.value;
 30         begin
 31           block
 32             exprResult := expression;
 33           exception
 34             catch RANGE_ERROR:
 35               raisesRangeError := TRUE;
 36           end block;
 37         end func;
 38   blank
 39     end func;
 40   blank (trailing newline)")

;;; --------------------------------------------------------------------------
;;; Helpers
;;

(defmacro seed7-test--with-nav-buffer (&rest body)
  "Evaluate BODY inside a `seed7-mode' temp buffer containing the nav fixture."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert seed7-test--nested-nav-code)
     (seed7-mode)
     ,@body))

(defun seed7-test--goto-bol (line)
  "Move point to the beginning of LINE (1-based) in current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test--goto-eol (line)
  "Move point to the end of LINE (1-based) in current buffer."
  (seed7-test--goto-bol line)
  (end-of-line))

(defun seed7-test--current-line ()
  "Return 1-based line number at current point."
  (line-number-at-pos))

;;; --------------------------------------------------------------------------
;;; Tests — forward navigation
;;
;; Uses `seed7-end-of-defun' directly (NOT `end-of-defun') to avoid the
;; Emacs built-in's point pre-processing and forward-line post-processing.
;; Expected line = the line that contains the closing token itself.
;;

(ert-deftest seed7-nav-nested/forward-long-func-bigintExpr ()
  "From BOL line 2, seed7-end-of-defun leaves point on line 7 (end func;)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 2)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 7))))

(ert-deftest seed7-nav-nested/forward-short-func-intExpr ()
  "From BOL line 10, seed7-end-of-defun leaves point on line 11 (return)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 10)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 11))))

(ert-deftest seed7-nav-nested/forward-short-func-striExpr ()
  "From BOL line 14, seed7-end-of-defun leaves point on line 15 (return)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 14)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 15))))

(ert-deftest seed7-nav-nested/forward-short-func-boolExpr ()
  "From BOL line 18, seed7-end-of-defun leaves point on line 19 (return)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 18)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 19))))

(ert-deftest seed7-nav-nested/forward-proc-with-nested-func ()
  "From BOL line 22, seed7-end-of-defun leaves point on line 39 (outer end func;)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 22)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 39))))

(ert-deftest seed7-nav-nested/forward-nested-func-raisesRangeError ()
  "From BOL line 25, seed7-end-of-defun leaves point on line 37 (inner end func;)."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 25)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 37))))

;;; --------------------------------------------------------------------------
;;; Tests — backward navigation
;;
;; Uses `beginning-of-defun' (dispatches to `seed7-nav-beginning-of-defun')
;; which has no pre-processing side effects on point.
;; Expected line = the line that contains the opening declaration keyword.
;;

(ert-deftest seed7-nav-nested/backward-from-end-of-bigintExpr ()
  "From EOL line 7 (end func;), beginning-of-defun reaches line 2."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 7)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 2))))

(ert-deftest seed7-nav-nested/backward-from-return-of-intExpr ()
  "From EOL line 11 (return), beginning-of-defun reaches line 10."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 11)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 10))))

(ert-deftest seed7-nav-nested/backward-from-return-of-striExpr ()
  "From EOL line 15 (return), beginning-of-defun reaches line 14."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 15)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 14))))

(ert-deftest seed7-nav-nested/backward-from-return-of-boolExpr ()
  "From EOL line 19 (return), beginning-of-defun reaches line 18."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 19)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 18))))

(ert-deftest seed7-nav-nested/backward-from-end-of-raisesRangeError ()
  "From EOL line 37 (inner end func;), beginning-of-defun reaches line 25."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 37)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 25))))

(ert-deftest seed7-nav-nested/backward-from-end-of-proc ()
  "From EOL line 39 (outer end func;), beginning-of-defun reaches line 22."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 39)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 22))))

;;; --------------------------------------------------------------------------
;;; Tests — repeated navigation
;;

(ert-deftest seed7-nav-nested/forward-twice-from-bigintExpr ()
  "Two seed7-end-of-defun calls from line 2: first lands on line 7, second on line 11."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-bol 2)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 7))
    ;; Move past the closing token before the next call.
    (forward-line 1)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--current-line) 11))))

(ert-deftest seed7-nav-nested/backward-twice-from-end-of-proc ()
  "Two beginning-of-defun calls from EOL line 39 land on line 22 then line 18."
  (seed7-test--with-nav-buffer
    (seed7-test--goto-eol 39)
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 22))
    (beginning-of-defun)
    (should (= (seed7-test--current-line) 18))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-nav-nested-01)

;;; seed7-test-nav-nested-01.el ends here
