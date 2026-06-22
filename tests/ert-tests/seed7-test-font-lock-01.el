;;; seed7-test-font-lock-01.el --- ERT regression tests: seed7-mode font-lock faces  -*- lexical-binding: t; -*-

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
;; Regression tests for font-lock *face* application in `seed7-mode'.
;;
;; These tests complement `seed7-test-syntax-propertize-01.el', which covers
;; only the `syntax-table' text properties placed by `seed7-mode-syntax-propertize'.
;; The present file verifies that after a full `font-lock-ensure' pass the correct
;; *visual* faces (text property `face') appear on every character of each
;; comment delimiter and comment body.
;;
;; **Primary regression being guarded**
;;
;; The closing `)' of the block-comment closer `*)' MUST receive
;; `font-lock-comment-delimiter-face'.  Without an explicit font-lock keyword
;; rule for block-comment delimiters, Emacs syntactic font-lock may leave the
;; second character of the closer unfontified.  Every test in Section C that
;; checks the closing `)' will FAIL until that fix is in place.
;;
;; **How faces are checked**
;;
;; Font-lock stores faces as the `face' text property.  The helper
;; `seed7-test--has-face-p' handles both a single face symbol and a list of
;; faces (e.g. when multiple rules prepend faces on the same position).
;;
;; Sections:
;;   A. Opening delimiter `(*' — both chars → `font-lock-comment-delimiter-face'.
;;   B. Block-comment body   — inner text  → `font-lock-comment-face'.
;;   C. Closing delimiter `*)' — both chars → `font-lock-comment-delimiter-face'.
;;      C.2 (the `)' character) is the **primary regression point**.
;;   D. Line comment   — `#' and body → appropriate comment faces.
;;   E. Code outside comments — no comment face.
;;   F. Multiple and nested block comments — all four delimiter chars correct.
;;   G. Combined: block comment adjacent to code.
;;
;; To run from the command line:
;;
;;   emacs --batch -L . \
;;         -l ert \
;;         -l tests/ert-tests/seed7-test-font-lock-01.el \
;;         --eval '(setq ert-batch-print-length nil ert-batch-print-level nil)' \
;;         -f ert-run-tests-batch-and-exit

;;; --------------------------------------------------------------------------
;;; Dependencies:

;; Allow byte-compiling and loading from an interactive session or batch mode
;; without requiring `tests/ert-tests/' to be pre-configured in `load-path'.
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
(require 'pel-ert)

;;; --------------------------------------------------------------------------
;;; Helpers

(defmacro seed7-test--with-fontified-buffer (text &rest body)
  "Evaluate BODY in a `seed7-mode' temp buffer containing TEXT, fully fontified.

Activates `seed7-mode', runs `syntax-propertize' over the whole buffer, then
calls `font-lock-ensure' to force a complete font-lock pass.  All `face' text
properties are therefore in place before BODY executes."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (seed7-mode)
     (syntax-propertize (point-max))
     (font-lock-ensure)
     ,@body))

(defun seed7-test--buf-pos (offset)
  "Return the buffer position for 0-based character OFFSET.
In a fresh temp buffer `point-min' is 1, so offset 0 maps to position 1."
  (+ (point-min) offset))

(defun seed7-test--has-face-p (pos expected-face)
  "Return non-nil if EXPECTED-FACE is applied at buffer position POS.
Accepts both a single face symbol and a list of face specs as the `face'
text property value."
  (let ((face (get-text-property pos 'face)))
    (cond
     ((null face)   nil)
     ((listp face)  (memq expected-face face))
     (t             (eq  face expected-face)))))

(defun seed7-test--has-no-comment-face-p (pos)
  "Return non-nil if POS carries neither `font-lock-comment-face' nor
`font-lock-comment-delimiter-face'."
  (and (not (seed7-test--has-face-p pos 'font-lock-comment-face))
       (not (seed7-test--has-face-p pos 'font-lock-comment-delimiter-face))))

;;; --------------------------------------------------------------------------
;;; Section A — Opening block-comment delimiter `(*'
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/bc-opener-paren-has-delimiter-face ()
  "Section A.1 — `(' of `(*' must have `font-lock-comment-delimiter-face'.

Without an explicit font-lock rule this character may be left unfontified
or receive only `font-lock-comment-face' instead of the delimiter face."
  ;; Buffer: ( *   a   c o m m e n t   * )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (should (seed7-test--has-face-p (seed7-test--buf-pos 0)
                                    'font-lock-comment-delimiter-face))))

(ert-deftest seed7-font-lock/bc-opener-star-has-delimiter-face ()
  "Section A.2 — `*' of `(*' must have `font-lock-comment-delimiter-face'."
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (should (seed7-test--has-face-p (seed7-test--buf-pos 1)
                                    'font-lock-comment-delimiter-face))))

;;; --------------------------------------------------------------------------
;;; Section B — Block-comment body
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/bc-body-has-comment-face ()
  "Section B — Characters inside `(* ... *)' must have `font-lock-comment-face'.

Checks a middle character of the comment body.  The body starts at offset 2
(the space after `(*') and ends just before the `*)' at offset 12."
  ;; Buffer: "(* a comment *)"  length 15
  ;; Middle of body at offset 5 = `c'.
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (should (seed7-test--has-face-p (seed7-test--buf-pos 5)
                                    'font-lock-comment-face))))

(ert-deftest seed7-font-lock/bc-body-all-chars-have-comment-face ()
  "Section B — Every character of the comment body has `font-lock-comment-face'.

Buffer: \"(* body *)\" — body occupies offsets 2..6 inclusive (\" body \")."
  ;; Offsets: 0=( 1=* 2=' ' 3=b 4=o 5=d 6=y 7=' ' 8=* 9=)
  (seed7-test--with-fontified-buffer "(* body *)"
    (dolist (offset '(2 3 4 5 6 7))    ; ' ' b o d y ' '
      (should (pel-eq t
                      (seed7-test--has-face-p (seed7-test--buf-pos offset)
                                              'font-lock-comment-face)
                      (list 'offset offset
                            'char (char-to-string
                                   (char-after (seed7-test--buf-pos offset)))))))))

;;; --------------------------------------------------------------------------
;;; Section C — Closing block-comment delimiter `*)'
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/bc-closer-star-has-delimiter-face ()
  "Section C.1 — `*' of `*)' must have `font-lock-comment-delimiter-face'.

Buffer: \"(* a comment *)\" — closing `*' is at offset 13 (length 15, 0-based offset 13)."
  ;; Buffer: ( *   a   c o m m e n t    *  )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
  ;;                                          ^closing *
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (let* ((len (length "(* a comment *)"))
           (star-offset (- len 2)))     ; offset 13
      (should (eq (char-after (seed7-test--buf-pos star-offset)) ?*))
      (should (seed7-test--has-face-p (seed7-test--buf-pos star-offset)
                                      'font-lock-comment-delimiter-face)))))

(ert-deftest seed7-font-lock/bc-closer-paren-has-delimiter-face ()
  "Section C.2 — `)' of `*)' must have `font-lock-comment-delimiter-face'.

*** THIS IS THE PRIMARY REGRESSION TEST. ***

The closing `)' is the second character of the two-character style-b
comment-end delimiter.  Emacs syntactic font-lock may not apply
`font-lock-comment-delimiter-face' to this character automatically;
the fix requires an explicit font-lock keyword matcher for block-comment
delimiters.  This test FAILS until that fix is applied."
  ;; Buffer: ( *   a   c o m m e n t    *  )
  ;; Offset: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14
  ;;                                             ^closing )
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (let* ((len (length "(* a comment *)"))
           (paren-offset (- len 1)))    ; offset 14
      (should (eq (char-after (seed7-test--buf-pos paren-offset)) ?\)))
      (should (seed7-test--has-face-p (seed7-test--buf-pos paren-offset)
                                      'font-lock-comment-delimiter-face)))))

(ert-deftest seed7-font-lock/bc-closer-paren-not-unfontified ()
  "Section C.3 — `)' of `*)' must NOT be completely unfontified.

Companion to C.2: asserts that the face is not nil.  A nil face means the
character was silently dropped from all fontification, which is the current
broken behaviour."
  (seed7-test--with-fontified-buffer "(* a comment *)"
    (let* ((len (length "(* a comment *)"))
           (paren-pos (seed7-test--buf-pos (- len 1))))
      (should (eq (char-after paren-pos) ?\)))
      (should-not (null (get-text-property paren-pos 'face))))))

;;; --------------------------------------------------------------------------
;;; Section D — Line comment (`# ...')
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/line-comment-hash-has-delimiter-face ()
  "Section D.1 — `#' of a line comment must have `font-lock-comment-delimiter-face'.

Buffer: \"# a line comment\"  — `#' is at offset 0."
  (seed7-test--with-fontified-buffer "# a line comment"
    (should (seed7-test--has-face-p (seed7-test--buf-pos 0)
                                    'font-lock-comment-delimiter-face))))

(ert-deftest seed7-font-lock/line-comment-body-has-comment-face ()
  "Section D.2 — Body of a line comment must have `font-lock-comment-face'.

Buffer: \"# hello\"  — body character `h' is at offset 2."
  (seed7-test--with-fontified-buffer "# hello"
    (should (seed7-test--has-face-p (seed7-test--buf-pos 2)
                                    'font-lock-comment-face))))

;;; --------------------------------------------------------------------------
;;; Section E — Code outside comments must not carry comment faces
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/code-outside-comment-has-no-comment-face ()
  "Section E — Plain Seed7 code must not have any comment face.

Buffer: \"x := 42;\"  — none of the characters should carry a comment face."
  (seed7-test--with-fontified-buffer "x := 42;"
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (should (pel-eq t
                        (seed7-test--has-no-comment-face-p pos)
                        (list 'pos pos
                              'char (char-to-string (char-after pos))
                              'face (get-text-property pos 'face))))
        (setq pos (1+ pos))))))

(ert-deftest seed7-font-lock/star-in-expression-has-no-comment-face ()
  "Section E — `*' in multiplication must not carry any comment face.

Buffer: \"x * y\"  — the `*' at offset 2 is a multiplication operator."
  (seed7-test--with-fontified-buffer "x * y"
    (should (seed7-test--has-no-comment-face-p (seed7-test--buf-pos 2)))))

(ert-deftest seed7-font-lock/paren-in-call-has-no-comment-face ()
  "Section E — `)' closing a function call must not carry any comment face.

Buffer: \"foo(x)\"  — the `)' at offset 5 closes the argument list."
  (seed7-test--with-fontified-buffer "foo(x)"
    (should (seed7-test--has-no-comment-face-p (seed7-test--buf-pos 5)))))

;;; --------------------------------------------------------------------------
;;; Section F — Multiple and nested block comments
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/two-block-comments-both-closers-have-delimiter-face ()
  "Section F.1 — Both `*)' closers in two separate block comments are fontified.

Buffer: \"(* one *) (* two *)\"
                           ^--- second `*)' closer paren is at offset 18."
  ;; "(* one *) (* two *)"
  ;;  0         1
  ;;  0123456789012345678
  ;; Closer 1: `*' at 7, `)' at 8.
  ;; Closer 2: `*' at 17, `)' at 18.
  (seed7-test--with-fontified-buffer "(* one *) (* two *)"
    ;; First closer `*'
    (should (seed7-test--has-face-p (seed7-test--buf-pos 7)
                                    'font-lock-comment-delimiter-face))
    ;; First closer `)' — regression point
    (should (seed7-test--has-face-p (seed7-test--buf-pos 8)
                                    'font-lock-comment-delimiter-face))
    ;; Second closer `*'
    (should (seed7-test--has-face-p (seed7-test--buf-pos 17)
                                    'font-lock-comment-delimiter-face))
    ;; Second closer `)' — regression point
    (should (seed7-test--has-face-p (seed7-test--buf-pos 18)
                                    'font-lock-comment-delimiter-face))))

(ert-deftest seed7-font-lock/nested-block-comment-all-closers-have-delimiter-face ()
  "Section F.2 — Both `*)' closers in a nested block comment are fontified.

Buffer: \"(* outer (* inner *) end *)\"
Inner `*)': `*' at 17, `)' at 18.
Outer `*)': `*' at 24, `)' at 25."
  ;; "(* outer (* inner *) end *)"
  ;;  0         1         2
  ;;  0123456789012345678901234567
  (seed7-test--with-fontified-buffer "(* outer (* inner *) end *)"
    ;; Inner closer `*'
    (should (seed7-test--has-face-p (seed7-test--buf-pos 17)
                                    'font-lock-comment-delimiter-face))
    ;; Inner closer `)' — regression point
    (should (seed7-test--has-face-p (seed7-test--buf-pos 18)
                                    'font-lock-comment-delimiter-face))
    ;; Outer closer `*'
    (should (seed7-test--has-face-p (seed7-test--buf-pos 25)
                                    'font-lock-comment-delimiter-face))
    ;; Outer closer `)' — regression point
    (should (seed7-test--has-face-p (seed7-test--buf-pos 26)
                                    'font-lock-comment-delimiter-face))))

;;; --------------------------------------------------------------------------
;;; Section G — Block comment adjacent to code
;;; --------------------------------------------------------------------------

(ert-deftest seed7-font-lock/block-comment-after-code-closer-paren-fontified ()
  "Section G — `)' of `*)' is fontified even when code precedes the comment.

Buffer: \"x := 1; (* note *) y := 2;\"
The block comment runs from offset 8 to 18.
Closing `)' is at offset 18 — regression point."
  ;; "x := 1; (* note *) y := 2;"
  ;;  0         1         2
  ;;  01234567890123456789012345678
  ;; `(' at 8, `*' at 9, body 10-16, `*' at 17, `)' at 18.
  (seed7-test--with-fontified-buffer "x := 1; (* note *) y := 2;"
    ;; Opener
    (should (seed7-test--has-face-p (seed7-test--buf-pos 8)
                                    'font-lock-comment-delimiter-face))
    (should (seed7-test--has-face-p (seed7-test--buf-pos 9)
                                    'font-lock-comment-delimiter-face))
    ;; Closer `*'
    (should (seed7-test--has-face-p (seed7-test--buf-pos 17)
                                    'font-lock-comment-delimiter-face))
    ;; Closer `)' — regression point
    (should (seed7-test--has-face-p (seed7-test--buf-pos 18)
                                    'font-lock-comment-delimiter-face))
    ;; Code after the comment must not have comment face
    (should (seed7-test--has-no-comment-face-p (seed7-test--buf-pos 20)))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-font-lock-01)

;;; seed7-test-font-lock-01.el ends here
