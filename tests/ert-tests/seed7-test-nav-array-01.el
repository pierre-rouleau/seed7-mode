;;; seed7-test-nav-array-01.el --- ERT regression tests: array.s7i-style navigation fixes.  -*- lexical-binding: t; -*-

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
;; Regression tests for navigation fixes introduced in PR#83, exercising code
;; shaped like Seed7's lib/array.s7i (lines 56-584).  The fixture is a minimal
;; self-contained excerpt that reproduces all four failure modes that were fixed:
;;
;;   Fix 1 — forward navigation from an outer long function skips nested
;;            short-function returns (`seed7-end-of-defun' short-func nesting
;;            guard; `(<= short-func-decl-pos original-pos)' → `(not (and
;;            final-pos ...))'  regression fix also covered).
;;
;;   Fix 2 — backward navigation from `end func;' of an outer long function
;;            skips nested action declarations and short-function declarations
;;            (`long-body-only' flag in `seed7-beg-of-defun'; Block B skipped
;;            when `long-body-only' is t).
;;
;;   Fix 3 — `backward-sexp' from after the terminating `;' of a multi-line
;;            short-function return reaches the enclosing short-function
;;            declaration rather than the opening parenthesis of the return
;;            expression (`seed7--at-multiline-short-func-end-p').
;;
;;   Fix 4 — `backward-sexp' from after `end global;' calls
;;            `seed7-to-block-backward' (requires `"global"' in
;;            `seed7-block-end-regexp').
;;
;; IMPORTANT: Forward tests call `seed7-end-of-defun' directly, NOT via
;; `end-of-defun'.  The Emacs built-in wrapper pre-processes point and
;; appends `forward-line 1', which would perturb expected line numbers.
;;
;; Backward defun tests call `beginning-of-defun', which dispatches to
;; `seed7-beg-of-defun' with no wrapper side effects.
;;
;; Backward sexp tests call `seed7--forward-sexp-function' directly with -1,
;; since the cases exercised (multi-line return end, `end global;') are handled
;; exclusively inside `seed7--forward-sexp-function', not in `seed7-beg-of-defun'.
;;
;; Fixture line layout (1-based):
;;
;;   1   blank
;;   2   const func type: outerFunc (in type: baseType) is func
;;   3     result
;;   4       var type: arrayType is void;
;;   5     begin
;;   6       if arrayType = void then
;;   7         global
;;   8         const boolean: isValid (attr arrayType)                       is TRUE;
;;   9         const proc: (inout arrayType: dest) := (in arrayType: source) is action "ARR_CPY";
;;  10         const func arrayType: (in integer: factor) times (in baseType: element) is action "ARR_TIMES";
;;  11         const func arrayType: (in ARRAY_IDX_RANGE: indexRange) times
;;  12             (in baseType: element) is
;;  13           return [indexRange.minIdx] (someConv conv
;;  14                  (succ(indexRange.maxIdx - indexRange.minIdx) times element));
;;  15   blank
;;  16         const proc: forRange (inout baseType: forVar, in arrayType: arr) is func
;;  17           begin
;;  18             forVar := arr[1];
;;  19           end func;
;;  20   blank
;;  21         end global;
;;  22       end if;
;;  23     end func;
;;  24   blank
;;
;; To run from the command line:
;;
;;   emacs --batch -l seed7-mode.el \
;;         -l tests/ert-tests/seed7-test-nav-array-01.el \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Fixture
;;

(defconst seed7-test--array-nav-code
  ;; Line 1 — blank
  "\n\
const func type: outerFunc (in type: baseType) is func\n\
  result\n\
    var type: arrayType is void;\n\
  begin\n\
    if arrayType = void then\n\
      global\n\
      const boolean: isValid (attr arrayType)                       is TRUE;\n\
      const proc: (inout arrayType: dest) := (in arrayType: source) is action \"ARR_CPY\";\n\
      const func arrayType: (in integer: factor) times (in baseType: element) is action \"ARR_TIMES\";\n\
      const func arrayType: (in ARRAY_IDX_RANGE: indexRange) times\n\
          (in baseType: element) is\n\
        return [indexRange.minIdx] (someConv conv\n\
               (succ(indexRange.maxIdx - indexRange.minIdx) times element));\n\
\n\
      const proc: forRange (inout baseType: forVar, in arrayType: arr) is func\n\
        begin\n\
          forVar := arr[1];\n\
        end func;\n\
\n\
      end global;\n\
    end if;\n\
  end func;\n\
\n"
  "Seed7 fixture code shaped like lib/array.s7i for navigation regression tests.
See Commentary for the line layout.")

;;; --------------------------------------------------------------------------
;;; Helpers
;;

(defmacro seed7-test--with-array-nav-buffer (&rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing the array nav fixture."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert seed7-test--array-nav-code)
     (seed7-mode)
     ,@body))

(defun seed7-test--array-goto-bol (line)
  "Move point to the beginning of LINE (1-based) in current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test--array-goto-eol (line)
  "Move point to the end of LINE (1-based) in current buffer."
  (seed7-test--array-goto-bol line)
  (end-of-line))

(defun seed7-test--array-current-line ()
  "Return the 1-based line number at current point."
  (line-number-at-pos))

;;; --------------------------------------------------------------------------
;;; Tests — forward navigation  (seed7-end-of-defun called directly)
;;
;; Expected line = line containing the closing token of the callable.
;;

(ert-deftest seed7-nav-array/forward-outer-func-past-nested-short ()
  "Fix 1: seed7-end-of-defun from outer long func decl (BOL line 2) must reach
the outer `end func;' (line 23) and must NOT stop at the nested multi-line
short-function return ending on line 14."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-bol 2)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--array-current-line) 23))))

(ert-deftest seed7-nav-array/forward-multiline-short-func ()
  "seed7-end-of-defun from nested multi-line short-func decl (BOL line 11)
reaches the terminating return `;' on line 14."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-bol 11)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--array-current-line) 14))))

(ert-deftest seed7-nav-array/forward-nested-long-proc ()
  "seed7-end-of-defun from nested long proc decl (BOL line 16) reaches
its `end func;' on line 19."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-bol 16)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--array-current-line) 19))))

;;; --------------------------------------------------------------------------
;;; Tests — backward navigation  (beginning-of-defun)
;;
;; Expected line = line containing the opening callable declaration.
;;

(ert-deftest seed7-nav-array/backward-from-outer-end-func ()
  "Fix 2: beginning-of-defun from EOL of outer `end func;' (line 23) must
reach the outer long-func declaration on line 2 and must NOT stop at
any nested action declaration (lines 9-10) or short-func declaration (line 11)."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-eol 23)
    (beginning-of-defun)
    (should (= (seed7-test--array-current-line) 2))))

(ert-deftest seed7-nav-array/backward-from-nested-proc-end-func ()
  "beginning-of-defun from EOL of nested long proc `end func;' (line 19)
reaches the nested proc declaration on line 16."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-eol 19)
    (beginning-of-defun)
    (should (= (seed7-test--array-current-line) 16))))

;;; --------------------------------------------------------------------------
;;; Tests — backward sexp  (seed7--forward-sexp-function called with -1)
;;
;; These cases are handled inside `seed7--forward-sexp-function', not by
;; `seed7-beg-of-defun', so they must be tested via the sexp function directly.
;;

(ert-deftest seed7-nav-array/backward-sexp-from-multiline-return-end ()
  "Fix 3: seed7--forward-sexp-function(-1) from EOL of the terminating `;' of
a multi-line short-function return (line 14) must reach the short-func
declaration starting on line 11.
Must NOT land at the opening parenthesis `(' of the return expression."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-eol 14)
    (seed7--forward-sexp-function -1)
    (should (= (seed7-test--array-current-line) 11))))

(ert-deftest seed7-nav-array/backward-sexp-from-end-global ()
  "Fix 4: seed7--forward-sexp-function(-1) from EOL of `end global;' (line 21)
must invoke seed7-to-block-backward and reach the matching `global' on line 7.
Requires `\"global\"' to be present in `seed7-block-end-regexp'."
  (seed7-test--with-array-nav-buffer
    (seed7-test--array-goto-eol 21)
    (seed7--forward-sexp-function -1)
    (should (= (seed7-test--array-current-line) 7))))

;;; --------------------------------------------------------------------------
;;; Tests — repeated navigation
;;

(ert-deftest seed7-nav-array/forward-sequential-short-then-long ()
  "Sequential seed7-end-of-defun calls across adjacent callables:
  call 1 from BOL line 11 (nested short-func decl) → EOD on line 14;
  call 2 from BOL line 16 (nested long proc decl)  → EOD on line 19."
  (seed7-test--with-array-nav-buffer
    ;; First call: multi-line short func.
    (seed7-test--array-goto-bol 11)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--array-current-line) 14))
    ;; Skip the blank line 15 and position at the next callable.
    (seed7-test--array-goto-bol 16)
    ;; Second call: nested long proc.
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--array-current-line) 19))))

(ert-deftest seed7-nav-array/backward-sequential-inner-then-outer ()
  "Sequential beginning-of-defun calls moving outward from the innermost callable:
  call 1 from EOL line 19 (nested long proc end) → line 16 (nested proc decl);
  call 2 from EOL line 23 (outer end func;)      → line  2 (outer func decl)."
  (seed7-test--with-array-nav-buffer
    ;; First call: inner long proc.
    (seed7-test--array-goto-eol 19)
    (beginning-of-defun)
    (should (= (seed7-test--array-current-line) 16))
    ;; Second call: outer long func (Fix 2: must skip action decls and short-func).
    (seed7-test--array-goto-eol 23)
    (beginning-of-defun)
    (should (= (seed7-test--array-current-line) 2))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-nav-array-01)

;;; seed7-test-nav-array-01.el ends here
