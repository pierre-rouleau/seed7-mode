;;; seed7-test-indent-03.el --- ERT regression test for set-union indentation  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-07-03 00:00:00 EDT, updated by Pierre Rouleau>

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
;; Regression test for a set-definition indentation bug exercised by files
;; such as the Seed7 standard library `chkarr.sd7'.
;;
;; `seed7-line-inside-set-definition-block' finds the start of a set
;; definition with `seed7--line-set-definition-start-regexp' and then calls
;; `seed7--with-forward-sexp' from the opening `{' to locate the matching
;; `}' that closes the block (see `seed7-mode.el').  This works correctly
;; when the whole set literal spans multiple lines *inside a single pair of
;; braces*, e.g.:
;;
;;   var set of string: keywords is {
;;     "for",
;;     "while",
;;     "repeat"};
;;
;; It does not work when a set is built from several *distinct* brace pairs
;; combined with the `|' (union) operator, each pair balanced on its own
;; line, e.g. the `illegalControlChar' pattern already present (but never
;; exercised by an indentation test) in `tests/seed7-code/set-01.sd7':
;;
;;   const set of char: illegalControlChar is {'\0;'  .. '\8;'}
;;                                         |  {'\11;' .. '\12;'}
;;                                         |  {'\14;' .. '\31;'}
;;                                         |  {'\127;'};
;;
;; Here `seed7--with-forward-sexp' closes the sexp at the `}' that ends the
;; *first* line, because that first `{...}' pair is already balanced.  The
;; reported block end position therefore falls at the end of line 1, so
;; `seed7-line-inside-set-definition-block' returns nil for every
;; continuation line that starts with `|'.  Those lines are consequently
;; never recognized as being "inside a set definition block", and
;; `seed7-calc-indent' falls through to an unrelated branch instead of
;; indenting them relative to the set declaration.
;;
;; The tests below reproduce this pattern in isolation.  They are expected
;; to fail with the current `seed7-mode.el' implementation, demonstrating
;; the bug; they should pass once the set/array block detection is made
;; aware of multi-part, `|'-joined set literals.

;; ---------------------------------------------------------------------------
;;; Code:

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

;; ---------------------------------------------------------------------------
;; Test helpers
;; ------------

(defun seed7-test-indent-03--goto-line (line)
  "Move point to the beginning of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test-indent-03--line-indentation (line)
  "Return indentation of LINE (1-based)."
  (save-excursion
    (seed7-test-indent-03--goto-line line)
    (current-indentation)))

;; ---------------------------------------------------------------------------
;; 13. Set definition built from several `|'-joined brace pairs
;; --------------------------------------------------------------
;;
;; This mirrors the `illegalControlChar' set found (but not indentation
;; tested) in `tests/seed7-code/set-01.sd7', and the kind of construct
;; encountered while indenting the Seed7 standard library `chkarr.sd7'.
;;
;;   const set of char: illegalControlChar is {'\0;' .. '\8;'}   ; col 0
;;                                         | {'\11;' .. '\12;'}  ; col 2 (expected)
;;                                         | {'\14;' .. '\31;'}  ; col 2 (expected)
;;                                         | {'\127;'};          ; col 2 (expected)

(defconst seed7-test-indent-03--set-union-correct
  (concat
   "const set of char: illegalControlChar is {'\\0;' .. '\\8;'}\n"
   "  | {'\\11;' .. '\\12;'}\n"
   "  | {'\\14;' .. '\\31;'}\n"
   "  | {'\\127;'};\n")
  "Expected layout for a set built from several `|'-joined brace pairs.")

(defconst seed7-test-indent-03--set-union-misaligned
  (concat
   "const set of char: illegalControlChar is {'\\0;' .. '\\8;'}\n"
   "| {'\\11;' .. '\\12;'}\n"
   "| {'\\14;' .. '\\31;'}\n"
   "| {'\\127;'};\n")
  "Misaligned version of `seed7-test-indent-03--set-union-correct'.")

(ert-deftest seed7-indent/set-union-keeps-correct-layout ()
  "Indenting an already-correct `|'-joined set definition keeps its layout.

This currently FAILS: `seed7-line-inside-set-definition-block' loses
track of the set once the first `{...}' pair closes on line 1, so
continuation lines starting with `|' are not indented relative to the
declaration."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--set-union-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 1) 0))
    (should (= (seed7-test-indent-03--line-indentation 2) 2))
    (should (= (seed7-test-indent-03--line-indentation 3) 2))
    (should (= (seed7-test-indent-03--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-03--set-union-correct))))

(ert-deftest seed7-indent/set-union-fixes-misaligned-layout ()
  "Indenting a misaligned `|'-joined set definition should restore the
expected layout.

This currently FAILS for the same reason as
`seed7-indent/set-union-keeps-correct-layout'."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--set-union-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 1) 0))
    (should (= (seed7-test-indent-03--line-indentation 2) 2))
    (should (= (seed7-test-indent-03--line-indentation 3) 2))
    (should (= (seed7-test-indent-03--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-03--set-union-correct))))

;; ---------------------------------------------------------------------------
(provide 'seed7-test-indent-03)

;;; seed7-test-indent-03.el ends here