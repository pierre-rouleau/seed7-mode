;;; seed7-test-indent-01.el --- ERT tests for Seed7 indentation regressions  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 12:10:06 EDT, updated by Pierre Rouleau>

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
;; Regression tests for continuation lines that start with `(' after a
;; previous non-empty line ending with the logical operators `and' or `or'.
;;
;; These cases previously fell through to the final fallback in
;; `seed7-calc-indent' and raised:
;;
;;   No rule yet to indent line N
;;
;; The main regression shape comes from `prg/bas7.sd7` around Line 696.

;;; Code:

(require 'ert)
(require 'seed7-mode)

(defconst seed7-test-indent--open-paren-after-and-code
  (concat
   "const func boolean: isStringExpr (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "         symbol <> \"\" and\n"
   "(symbol[length(symbol)] = '$' or symbol[1] = '\\\"' or symbol[1] in defstr_var and\n"
   "         not symbol[length(symbol)] in numeric_var_suffix);\n")
  "Fixture modeled after `prg/bas7.sd7`.
Where a continuation line starts with `(' after `and'.")

(defconst seed7-test-indent--open-paren-after-or-code
  (concat
   "const func boolean: needsWrap (in string: symbol) is\n"
   "  return symbol = \"abc\" or\n"
   "         symbol = \"def\" or\n"
   "(symbol[1] = 'A' or symbol[1] = 'B');\n")
  "Fixture for the analogous continuation case after `or'.")

(defun seed7-test-indent--goto-line (line)
  "Move point to the beginning of LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test-indent--line-indentation (line)
  "Return indentation of LINE."
  (save-excursion
    (seed7-test-indent--goto-line line)
    (current-indentation)))

(defun seed7-test-indent--line-first-char (line)
  "Return first non-whitespace character of LINE."
  (save-excursion
    (seed7-test-indent--goto-line line)
    (back-to-indentation)
    (char-after)))

(ert-deftest seed7-indent/open-paren-after-and-indent-region ()
  "Indenting a bas7-style continuation line starting with `(' after `and' must not error."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--open-paren-after-and-code)
    (seed7-mode)

    ;; This previously failed during full-file indentation.
    (indent-region (point-min) (point-max))

    ;; Line 4 starts with `(' and should now be indented as a continuation line.
    (should (eq (seed7-test-indent--line-first-char 4) ?\())
    (should (> (seed7-test-indent--line-indentation 4) 0))

    ;; It should align with the previous continuation line.
    (should (= (seed7-test-indent--line-indentation 4)
               (seed7-test-indent--line-indentation 3)))))

(ert-deftest seed7-indent/open-paren-after-or-indent-region ()
  "Indenting a continuation line starting with `(' after `or' must not error."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--open-paren-after-or-code)
    (seed7-mode)

    (indent-region (point-min) (point-max))

    ;; Line 4 starts with `(' and should be indented as a continuation line.
    (should (eq (seed7-test-indent--line-first-char 4) ?\())
    (should (> (seed7-test-indent--line-indentation 4) 0))
    (should (= (seed7-test-indent--line-indentation 4)
               (seed7-test-indent--line-indentation 3)))))

(provide 'seed7-test-indent-01)

;;; seed7-test-indent-01.el ends here
