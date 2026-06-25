;;; seed7-test-syntax-propertize-01.el --- ERT regression tests: seed7-mode-syntax-propertize  -*- lexical-binding: t; -*-

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
;; Regression tests for `seed7-mode-syntax-propertize'.  They serve as a
;; safety net for the planned refactoring that replaces runtime
;;   (string-to-syntax "< 1bn")  …  (string-to-syntax "> 4bn")
;; calls inside the function with precomputed `defconst' values.
;; The refactoring must not change *which* text properties are placed or
;; *where* — only how the property values are computed internally.
;;
;; Every assertion compares
;;   (get-text-property pos 'syntax-table)
;; against
;;   (string-to-syntax "…")
;; so the tests document the *required* property values regardless of whether
;; those values are produced inline or via a precomputed constant.
;;
;; `seed7-mode-syntax-propertize' contains two loops:
;;
;;   Loop 1 (`seed7-char-literal-re-no-comments'):
;;     • `#' in a number-base literal  →  syntax `_' (symbol constituent)
;;       on the `#' character only.
;;     • Single-quoted char literal  →  `(7 . ?')' (string fence) on the
;;       opening `'' and closing `'' only.
;;
;;   Loop 2 (`seed7-block-comment-delim-re' = `"(\\*\\|\\*)"'):
;;     • `(*' opener: `(' gets `"< 1bn"', `*' gets `"< 2bn"'.
;;     • `*)' closer: `*' gets `"> 3bn"', `)' gets `"> 4bn"'.
;;     Note: Loop 2 uses plain `re-search-forward', so every `(*' and `*)'
;;     in the buffer range gets text properties, including occurrences inside
;;     Seed7 string literals ("…").  Correct nesting behaviour relies on
;;     `syntax-ppss' at runtime, not on the propertize function.
;;
;; Sections:
;;   A. Loop 1 — `#' number-base separator.
;;   B. Loop 1 — single-quoted character literals.
;;   C. Loop 2 — `(*' block-comment opener.
;;   D. Loop 2 — `*)' block-comment closer.
;;   E. Negative cases — characters that must NOT receive an override.
;;   F. Multiple and nested block comments.
;;   G. Combined Loop 1 + Loop 2 in a single buffer.
;;
;; To run from the command line:
;;
;;   emacs --batch -L . \
;;         -l ert \
;;         -l tests/ert-tests/seed7-test-syntax-propertize-01.el \
;;         --eval '(setq ert-batch-print-length nil ert-batch-print-level nil)' \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:

(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Helpers

(defmacro seed7-test--with-propertized-buffer (text &rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing TEXT.
`syntax-propertize' is called over the whole buffer before BODY runs,
so all `syntax-table' text properties are in place."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (seed7-mode)
     (syntax-propertize (point-max))   ; propertize entire buffer up to point-max
     ,@body))

(defun seed7-test--syntax-prop (pos)
  "Return the `syntax-table' text property at buffer position POS (1-based)."
  (get-text-property pos 'syntax-table))

(defun seed7-test--buf-pos (offset)
  "Return the buffer position corresponding to 0-based character OFFSET.
In a fresh temp buffer `point-min' is 1, so `offset' 0 maps to position 1."
  (+ (point-min) offset))

;;; --------------------------------------------------------------------------
;;; Section A: Loop 1 — `#' number-base separator
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/hash-sep-gets-symbol-syntax ()
  "Loop 1: `#' in `16#FF' gets symbol-constituent `_' syntax.
This prevents the `#' from being interpreted as a line-comment starter."
  ;; Buffer: 1 6 # F F
  ;; Offset: 0 1 2 3 4   (0-based)
  (seed7-test--with-propertized-buffer "16#FF"
    (let ((hash-pos (seed7-test--buf-pos 2)))
      (should (eq (char-after hash-pos) ?#))
      (should (equal (seed7-test--syntax-prop hash-pos)
                     (string-to-syntax "_"))))))

(ert-deftest seed7-syntax-propertize/hash-sep-multiple ()
  "Loop 1: All `#' separators in a multi-expression line each get `_' syntax."
  ;; Buffer: 16#FF + 8#77
  ;; Offsets of `#': 2 and 8
  (seed7-test--with-propertized-buffer "16#FF + 8#77"
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 2))
                   (string-to-syntax "_")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 9))
                   (string-to-syntax "_")))))

(ert-deftest seed7-syntax-propertize/hash-sep-surrounding-digits-not-marked ()
  "Loop 1: Digits adjacent to `#' in `16#FF' receive no text property override."
  (seed7-test--with-propertized-buffer "16#FF"
    ;; `1' at offset 0, `6' at offset 1 — neither should have an override.
    (should (null (seed7-test--syntax-prop (seed7-test--buf-pos 0))))
    (should (null (seed7-test--syntax-prop (seed7-test--buf-pos 1))))))

;;; --------------------------------------------------------------------------
;;; Section B: Loop 1 — single-quoted character literals
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/char-literal-simple-opening-quote ()
  "Loop 1: Opening `'' of `'a'' gets string-fence syntax `(7 . ?')'."
  ;; Buffer: ' a '
  ;; Offset: 0 1 2
  (seed7-test--with-propertized-buffer "'a'"
    (let ((open-pos (seed7-test--buf-pos 0)))
      (should (eq (char-after open-pos) ?'))
      (should (equal (seed7-test--syntax-prop open-pos)
                     '(7 . ?'))))))

(ert-deftest seed7-syntax-propertize/char-literal-simple-closing-quote ()
  "Loop 1: Closing `'' of `'a'' gets string-fence syntax `(7 . ?')'."
  (seed7-test--with-propertized-buffer "'a'"
    (let ((close-pos (seed7-test--buf-pos 2)))
      (should (eq (char-after close-pos) ?'))
      (should (equal (seed7-test--syntax-prop close-pos)
                     '(7 . ?'))))))

(ert-deftest seed7-syntax-propertize/char-literal-simple-inner-char-not-marked ()
  "Loop 1: The character `a' between the quotes in `'a'' has no property override."
  (seed7-test--with-propertized-buffer "'a'"
    (should (null (seed7-test--syntax-prop (seed7-test--buf-pos 1))))))

(ert-deftest seed7-syntax-propertize/char-literal-backslash-opening-quote ()
  "Loop 1: Opening `'' of `'\\n'' gets string-fence syntax `(7 . ?')'."
  ;; Actual buffer content: '  \  n  '   (4 characters)
  ;; Offsets:               0  1  2  3
  (seed7-test--with-propertized-buffer "'\\n'"
    (let ((open-pos (seed7-test--buf-pos 0)))
      (should (eq (char-after open-pos) ?'))
      (should (equal (seed7-test--syntax-prop open-pos)
                     '(7 . ?'))))))

(ert-deftest seed7-syntax-propertize/char-literal-backslash-closing-quote ()
  "Loop 1: Closing `'' of `'\\n'' gets string-fence syntax `(7 . ?')'."
  (seed7-test--with-propertized-buffer "'\\n'"
    (let ((close-pos (seed7-test--buf-pos 3)))
      (should (eq (char-after close-pos) ?'))
      (should (equal (seed7-test--syntax-prop close-pos)
                     '(7 . ?'))))))

(ert-deftest seed7-syntax-propertize/char-literal-uppercase-escape-opening ()
  "Loop 1: Opening `'' of `'\\A'' (Seed7 Ctrl-A) gets string-fence syntax."
  ;; Actual buffer: '  \  A  '
  (seed7-test--with-propertized-buffer "'\\A'"
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   '(7 . ?')))))

(ert-deftest seed7-syntax-propertize/char-literal-uppercase-escape-closing ()
  "Loop 1: Closing `'' of `'\\A'' gets string-fence syntax."
  (seed7-test--with-propertized-buffer "'\\A'"
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 3))
                   '(7 . ?')))))

;;; --------------------------------------------------------------------------
;;; Section C: Loop 2 — `(*' block-comment opener
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/bc-opener-paren-gets-comment-start-1 ()
  "Loop 2: `(' of `(*' gets style-b comment-start-1 syntax `< 1bn'."
  ;; Buffer: ( *   a   c o m m e n t   * )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13
  (seed7-test--with-propertized-buffer "(* a comment *)"
    (let ((paren-pos (seed7-test--buf-pos 0)))
      (should (eq (char-after paren-pos) ?\())
      (should (equal (seed7-test--syntax-prop paren-pos)
                     (string-to-syntax "< 1bn"))))))

(ert-deftest seed7-syntax-propertize/bc-opener-star-gets-comment-start-2 ()
  "Loop 2: `*' of `(*' gets style-b comment-start-2 syntax `< 2bn'."
  (seed7-test--with-propertized-buffer "(* a comment *)"
    (let ((star-pos (seed7-test--buf-pos 1)))
      (should (eq (char-after star-pos) ?*))
      (should (equal (seed7-test--syntax-prop star-pos)
                     (string-to-syntax "< 2bn"))))))

(ert-deftest seed7-syntax-propertize/bc-opener-at-start-of-buffer ()
  "Loop 2: `(*' at the very start of the buffer still gets both properties."
  (seed7-test--with-propertized-buffer "(*"
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   (string-to-syntax "< 1bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 1))
                   (string-to-syntax "< 2bn")))))

;;; --------------------------------------------------------------------------
;;; Section D: Loop 2 — `*)' block-comment closer
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/bc-closer-star-gets-comment-end-3 ()
  "Loop 2: `*' of `*)' gets style-b comment-end-3 syntax `> 3bn'."
  ;; Buffer: ( *   a   c o m m e n t   *  )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13
  ;; Closing `*' is at offset 12, closing `)' at offset 13.
  (seed7-test--with-propertized-buffer "(* a comment *)"
    (let* ((buf "(* a comment *)")
           (star-pos (seed7-test--buf-pos (- (length buf) 2))))
      (should (eq (char-after star-pos) ?*))
      (should (equal (seed7-test--syntax-prop star-pos)
                     (string-to-syntax "> 3bn"))))))

(ert-deftest seed7-syntax-propertize/bc-closer-paren-gets-comment-end-4 ()
  "Loop 2: `)' of `*)' gets style-b comment-end-4 syntax `> 4bn'."
  (seed7-test--with-propertized-buffer "(* a comment *)"
    (let* ((buf "(* a comment *)")
           (paren-pos (seed7-test--buf-pos (- (length buf) 1))))
      (should (eq (char-after paren-pos) ?\)))
      (should (equal (seed7-test--syntax-prop paren-pos)
                     (string-to-syntax "> 4bn"))))))

(ert-deftest seed7-syntax-propertize/bc-closer-at-end-of-buffer ()
  "Loop 2: `*)' at the very end of the buffer still gets both properties."
  (seed7-test--with-propertized-buffer "*)"
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   (string-to-syntax "> 3bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 1))
                   (string-to-syntax "> 4bn")))))

;;; --------------------------------------------------------------------------
;;; Section E: Negative cases — characters that must NOT get an override
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/plain-paren-not-followed-by-star ()
  "Loop 2: `(' in `foo(x)' gets no text property override.
`seed7-block-comment-delim-re' matches `(*' as a unit; a `(' not followed
immediately by `*' must not receive the `< 1bn' property."
  ;; Buffer: f o o ( x )
  ;; Offset: 0 1 2 3 4 5
  (seed7-test--with-propertized-buffer "foo(x)"
    (let ((paren-pos (seed7-test--buf-pos 3)))
      (should (eq (char-after paren-pos) ?\())
      (should (null (seed7-test--syntax-prop paren-pos))))))

(ert-deftest seed7-syntax-propertize/plain-star-not-followed-by-paren ()
  "Loop 2: `*' in `x * y' gets no text property override.
`seed7-block-comment-delim-re' also matches `*)' as a unit; a `*' not
followed immediately by `)' must not receive the `> 3bn' property."
  ;; Buffer: x   *   y
  ;; Offset: 0 1 2 3 4
  (seed7-test--with-propertized-buffer "x * y"
    (let ((star-pos (seed7-test--buf-pos 2)))
      (should (eq (char-after star-pos) ?*))
      (should (null (seed7-test--syntax-prop star-pos))))))

(ert-deftest seed7-syntax-propertize/plain-star-followed-by-alnum ()
  "Loop 2: `*' in `x*y' (multiplication) gets no text property override."
  (seed7-test--with-propertized-buffer "x*y"
    (let ((star-pos (seed7-test--buf-pos 1)))
      (should (eq (char-after star-pos) ?*))
      (should (null (seed7-test--syntax-prop star-pos))))))

(ert-deftest seed7-syntax-propertize/double-star-power-operator ()
  "Loop 2: Neither `*' in `x**y' (power operator) gets a comment property."
  ;; Buffer: x * * y
  ;; Offset: 0 1 2 3
  (seed7-test--with-propertized-buffer "x**y"
    (should (null (seed7-test--syntax-prop (seed7-test--buf-pos 1))))
    (should (null (seed7-test--syntax-prop (seed7-test--buf-pos 2))))))

(ert-deftest seed7-syntax-propertize/no-properties-on-plain-identifiers ()
  "Neither Loop 1 nor Loop 2 places any property on plain identifier text."
  (seed7-test--with-propertized-buffer "x := y + z;"
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (should (null (seed7-test--syntax-prop pos)))
        (setq pos (1+ pos))))))

;;; --------------------------------------------------------------------------
;;; Section F: Multiple and nested block comments
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/two-separate-block-comments ()
  "Loop 2: Two separate `(*..*)' pairs each get the correct four properties.
Verifies the loop iterates beyond the first delimiter pair."
  ;; Buffer: ( *   f i r s t   *  )     (  *     s e c o n d   *  )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
  ;;                                        ^--- second `(' at offset 12
  ;; "(* first *) (* second *)" has length 24.
  (seed7-test--with-propertized-buffer "(* first *) (* second *)"
    ;; First opener: offsets 0 (`(') and 1 (`*').
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   (string-to-syntax "< 1bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 1))
                   (string-to-syntax "< 2bn")))
    ;; First closer: offsets 9 (`*') and 10 (`)').
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 9))
                   (string-to-syntax "> 3bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 10))
                   (string-to-syntax "> 4bn")))
    ;; Second opener: offsets 12 (`(') and 13 (`*').
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 12))
                   (string-to-syntax "< 1bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 13))
                   (string-to-syntax "< 2bn")))
    ;; Second closer: offsets 22 (`*') and 23 (`)').
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 22))
                   (string-to-syntax "> 3bn")))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 23))
                   (string-to-syntax "> 4bn")))))

(ert-deftest seed7-syntax-propertize/nested-block-comment-all-four-delimiters ()
  "Loop 2: All four delimiter pairs in `(* outer (* inner *) end-outer *)' get
the correct text properties.  Nesting is tracked at runtime by `syntax-ppss'
using the `n' flag; the propertize function marks every `(*' and `*)' pair
unconditionally."
  ;; Buffer (offsets):
  ;; ( * ' ' o u t e r ' ' (  *     i  n  n  e  r     *  )     e  n  d  -  o  u  t  e  r     *  )
  ;; 0 1 2  3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
  ;; Outer opener: 0 (`(') and 1 (`*').
  ;; Inner opener: 9 (`(') and 10 (`*').  [" (* " starts at offset 8 (space) ... let me recount]
  ;;
  ;; "(* outer (* inner *) end-outer *)"
  ;;  0123456789012345678901234567890123
  ;;           1111111111222222222233333
  ;;
  ;; Char positions (0-based):
  ;;  0: (   outer opener paren
  ;;  1: *   outer opener star
  ;;  9: (   inner opener paren
  ;; 10: *   inner opener star
  ;; 18: *   inner closer star
  ;; 19: )   inner closer paren
  ;; 31: *   outer closer star
  ;; 32: )   outer closer paren
  (seed7-test--with-propertized-buffer "(* outer (* inner *) end-outer *)"
    ;; Outer opener.
    (should (eq (char-after (seed7-test--buf-pos 0)) ?\())
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   (string-to-syntax "< 1bn")))
    (should (eq (char-after (seed7-test--buf-pos 1)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 1))
                   (string-to-syntax "< 2bn")))
    ;; Inner opener.
    (should (eq (char-after (seed7-test--buf-pos 9)) ?\())
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 9))
                   (string-to-syntax "< 1bn")))
    (should (eq (char-after (seed7-test--buf-pos 10)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 10))
                   (string-to-syntax "< 2bn")))
    ;; Inner closer.
    (should (eq (char-after (seed7-test--buf-pos 18)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 18))
                   (string-to-syntax "> 3bn")))
    (should (eq (char-after (seed7-test--buf-pos 19)) ?\)))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 19))
                   (string-to-syntax "> 4bn")))
    ;; Outer closer.
    (should (eq (char-after (seed7-test--buf-pos 31)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 31))
                   (string-to-syntax "> 3bn")))
    (should (eq (char-after (seed7-test--buf-pos 32)) ?\)))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 32))
                   (string-to-syntax "> 4bn")))))

;;; --------------------------------------------------------------------------
;;; Section G: Combined Loop 1 + Loop 2 content in one buffer
;;; --------------------------------------------------------------------------

(ert-deftest seed7-syntax-propertize/combined-hash-and-block-comment ()
  "Loop 1 and Loop 2 both place correct properties in a mixed buffer.
Exercises both loops in a single `syntax-propertize' pass."
  ;; Buffer: 16#FF (* hex value *)
  ;;         01234567890123456789
  ;;                   1111111111
  ;; Offsets:
  ;;  2: `#' → `_'
  ;;  6: `(' → `< 1bn'
  ;;  7: `*' → `< 2bn'
  ;; 18: `*' → `> 3bn'
  ;; 19: `)' → `> 4bn'
  (seed7-test--with-propertized-buffer "16#FF (* hex value *)"
    ;; Loop 1: `#' at offset 2.
    (should (eq (char-after (seed7-test--buf-pos 2)) ?#))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 2))
                   (string-to-syntax "_")))
    ;; Loop 2: `(*' opener at offsets 6-7.
    (should (eq (char-after (seed7-test--buf-pos 6)) ?\())
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 6))
                   (string-to-syntax "< 1bn")))
    (should (eq (char-after (seed7-test--buf-pos 7)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 7))
                   (string-to-syntax "< 2bn")))
    ;; Loop 2: `*)' closer at offsets 19-20.
    (let* ((buf "16#FF (* hex value *)")
           (close-star-off  (- (length buf) 2))
           (close-paren-off (- (length buf) 1)))
      (should (eq (char-after (seed7-test--buf-pos close-star-off)) ?*))
      (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos close-star-off))
                     (string-to-syntax "> 3bn")))
      (should (eq (char-after (seed7-test--buf-pos close-paren-off)) ?\)))
      (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos close-paren-off))
                     (string-to-syntax "> 4bn"))))))

(ert-deftest seed7-syntax-propertize/combined-char-literal-and-block-comment ()
  "Loop 1 char-literal quotes and Loop 2 block-comment delimiters in one buffer."
  ;; Buffer: 'a' (* char *)
  ;;         0123456789012
  ;; Offsets:
  ;;  0: `'' → `(7 . ?')'   (opening quote)
  ;;  2: `'' → `(7 . ?')'   (closing quote)
  ;;  4: `(' → `< 1bn'
  ;;  5: `*' → `< 2bn'
  ;; 11: `*' → `> 3bn'
  ;; 12: `)' → `> 4bn'
  (seed7-test--with-propertized-buffer "'a' (* char *)"
    ;; Loop 1: char literal quotes.
    (should (eq (char-after (seed7-test--buf-pos 0)) ?'))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 0))
                   '(7 . ?')))
    (should (eq (char-after (seed7-test--buf-pos 2)) ?'))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 2))
                   '(7 . ?')))
    ;; Loop 2: block comment delimiters.
    (should (eq (char-after (seed7-test--buf-pos 4)) ?\())
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 4))
                   (string-to-syntax "< 1bn")))
    (should (eq (char-after (seed7-test--buf-pos 5)) ?*))
    (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos 5))
                   (string-to-syntax "< 2bn")))
    (let* ((buf "'a' (* char *)")
           (close-star-off  (- (length buf) 2))
           (close-paren-off (- (length buf) 1)))
      (should (eq (char-after (seed7-test--buf-pos close-star-off)) ?*))
      (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos close-star-off))
                     (string-to-syntax "> 3bn")))
      (should (eq (char-after (seed7-test--buf-pos close-paren-off)) ?\)))
      (should (equal (seed7-test--syntax-prop (seed7-test--buf-pos close-paren-off))
                     (string-to-syntax "> 4bn"))))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-syntax-propertize-01)

;;; seed7-test-syntax-propertize-01.el ends here
