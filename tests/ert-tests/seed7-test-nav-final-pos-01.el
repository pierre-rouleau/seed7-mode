;;; seed7-test-nav-final-pos-01.el --- ERT regression tests: long-body-final-pos separation  -*- lexical-binding: t; -*-

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
;; Regression tests for the condition:
;;
;;   "Do not use mutable final-pos for nested rejection.  After the short-
;;   function branch sets final-pos, the new forward/action guards can reject
;;   a top-level forward/action declaration that appears before a later short
;;   function.  Keep a separate long-body-final-pos and use that only for
;;   nested-declaration rejection."
;;
;; In `seed7-end-of-defun', the 3rd (forward-declaration) and 4th
;; (action-declaration) search paths contain a nested-candidate rejection
;; guard of the form:
;;
;;   (not (and long-body-final-pos decl-pos
;;             (< original-pos decl-pos long-body-final-pos)))
;;
;; The guard must use `long-body-final-pos' — the end position of the nearest
;; long-body callable found by the first search path — NOT `final-pos', which
;; may already be set to the end of a short function found by the second path.
;;
;; Failure mode (if `final-pos' were used instead of `long-body-final-pos'):
;;
;;   Suppose:
;;     original-pos  = line 1  (blank, before everything)
;;     action-decl   = line 2  (top-level, no enclosing long-body callable)
;;     short-func end= line 5  (top-level, later in the file)
;;
;;   Execution order inside `seed7-end-of-defun':
;;     1. Long-body scan: nothing found.  long-body-final-pos = nil.
;;     2. Short-func path: final-pos ← 5.
;;     3. Action-decl path: decl-pos = 2.
;;        WRONG guard:   (< 1 2 5) = t  → rejected!  final-pos stays at 5.
;;        CORRECT guard: (and nil ...)  = nil → (not nil) = t → accepted.
;;                       (< 2 5) = t    → final-pos ← 2.
;;
;;   With the correct code the action declaration wins (it is nearer).
;;   With the wrong code the short function wins — an incorrect result.
;;
;; The same failure mode applies to the forward-declaration path.
;;
;; Fixture line layout (1-based):
;;
;;   1   blank
;;   2   const proc: firstAction (in integer: src) is action "ACT_FOO";
;;   3   blank
;;   4   const func boolean: firstShort (in integer: x) is
;;   5     return x > 0;
;;   6   blank
;;   7   const func boolean: secondFwdDecl (in integer: y) is forward;
;;   8   blank
;;   9   const func boolean: secondShort (in integer: z) is
;;  10     return z < 0;
;;  11   blank
;;
;; All four callables are top-level (no enclosing long-body callable).
;;
;; To run from the command line:
;;
;;   emacs --batch -l seed7-mode.el \
;;         -l tests/ert-tests/seed7-test-nav-final-pos-01.el \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Fixture
;;

(defconst seed7-test--final-pos-code
  ;; Line  1 — blank
  "\n\
const proc: firstAction (in integer: src) is action \"ACT_FOO\";\n\
\n\
const func boolean: firstShort (in integer: x) is\n\
  return x > 0;\n\
\n\
const func boolean: secondFwdDecl (in integer: y) is forward;\n\
\n\
const func boolean: secondShort (in integer: z) is\n\
  return z < 0;\n\
\n"
  ;; Line  2 — const proc: firstAction  ... is action \"ACT_FOO\";
  ;; Line  3 — blank
  ;; Line  4 — const func boolean: firstShort ...  is
  ;; Line  5 —   return x > 0;
  ;; Line  6 — blank
  ;; Line  7 — const func boolean: secondFwdDecl ... is forward;
  ;; Line  8 — blank
  ;; Line  9 — const func boolean: secondShort ... is
  ;; Line 10 —   return z < 0;
  ;; Line 11 — blank
  "Fixture: four top-level callables (action, short func, forward, short func)
with no enclosing long-body callable.  Used to verify that `seed7-end-of-defun'
uses `long-body-final-pos', not `final-pos', in the nested-rejection guards of
the action-declaration and forward-declaration search paths.")

;;; --------------------------------------------------------------------------
;;; Helpers
;;

(defmacro seed7-test--with-final-pos-buffer (&rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing the final-pos fixture."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert seed7-test--final-pos-code)
     (seed7-mode)
     ,@body))

(defun seed7-test--fp-goto-bol (line)
  "Move point to the beginning of LINE (1-based) in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test--fp-current-line ()
  "Return the 1-based line number of the current point."
  (line-number-at-pos))

;;; --------------------------------------------------------------------------
;;; Tests — core condition: long-body-final-pos must NOT be replaced by final-pos
;;

(ert-deftest seed7-nav-final-pos/action-before-short-func ()
  "Top-level action decl (line 2) lies before top-level short func (lines 4-5).
Starting from BOL line 1, `seed7-end-of-defun' must stop at line 2 (the
action declaration), NOT at line 5 (the short-func end).

This test fails when the action-declaration search path uses `final-pos'
instead of `long-body-final-pos' in its nested-rejection guard:
  short-func branch sets final-pos = 5;
  WRONG:   (not (and 5 2 (< 1 2 5))) = (not t) = nil  -> action rejected;
  CORRECT: (not (and nil 2 ...))     = (not nil) = t  -> action accepted;
           (< 2 5) -> action wins, final-pos <- 2."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 1)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 2))))

(ert-deftest seed7-nav-final-pos/fwd-decl-before-short-func ()
  "Top-level forward decl (line 7) lies before top-level short func (lines 9-10).
Starting from BOL line 6 (between the two groups), `seed7-end-of-defun' must
stop at line 7 (the forward declaration), NOT at line 10 (the second short-func
end).

Parallel to `seed7-nav-final-pos/action-before-short-func' but exercising the
forward-declaration search path.  Fails when that path's guard uses `final-pos'
instead of `long-body-final-pos':
  short-func branch sets final-pos = 10;
  WRONG:   (not (and 10 7 (< 6 7 10))) = (not t) = nil  -> fwd decl rejected;
  CORRECT: (not (and nil 7 ...))        = (not nil) = t  -> fwd decl accepted;
           (< 7 10) -> fwd decl wins, final-pos <- 7."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 6)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 7))))

;;; --------------------------------------------------------------------------
;;; Tests — baseline: individual navigation from each callable's declaration
;;

(ert-deftest seed7-nav-final-pos/action-from-its-own-decl ()
  "Baseline: `seed7-end-of-defun' from BOL line 2 (action decl) ends at line 2."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 2)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 2))))

(ert-deftest seed7-nav-final-pos/short-func-from-its-own-decl ()
  "Baseline: `seed7-end-of-defun' from BOL line 4 (short func decl) ends at
line 5 (the `return' terminator)."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 4)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 5))))

(ert-deftest seed7-nav-final-pos/fwd-decl-from-its-own-decl ()
  "Baseline: `seed7-end-of-defun' from BOL line 7 (forward decl) ends at
line 7 itself."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 7)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 7))))

(ert-deftest seed7-nav-final-pos/second-short-func-from-its-own-decl ()
  "Baseline: `seed7-end-of-defun' from BOL line 9 (second short func decl) ends
at line 10 (the `return' terminator)."
  (seed7-test--with-final-pos-buffer
    (seed7-test--fp-goto-bol 9)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 10))))

;;; --------------------------------------------------------------------------
;;; Tests — sequential navigation visits all callables in document order
;;

(ert-deftest seed7-nav-final-pos/sequential-all-four-callables ()
  "Sequential `seed7-end-of-defun' calls visit all four top-level callables in
document order: line 2 (action) → line 5 (short func) → line 7 (fwd decl) →
line 10 (short func).

This also verifies that after reaching line 2 via the first call, subsequent
calls continue forward correctly and do not re-apply the wrong rejection."
  (seed7-test--with-final-pos-buffer
    ;; Step 1: action declaration at line 2.
    (seed7-test--fp-goto-bol 1)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 2))
    ;; Step 2: first short function ending at line 5.
    (seed7-test--fp-goto-bol 4)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 5))
    ;; Step 3: forward declaration at line 7.
    (seed7-test--fp-goto-bol 7)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 7))
    ;; Step 4: second short function ending at line 10.
    (seed7-test--fp-goto-bol 9)
    (seed7-end-of-defun 1 :silent)
    (should (= (seed7-test--fp-current-line) 10))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-nav-final-pos-01)

;;; seed7-test-nav-final-pos-01.el ends here
