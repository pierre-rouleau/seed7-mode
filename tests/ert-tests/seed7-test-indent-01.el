;;; seed7-test-indent-01.el --- ERT tests for Seed7 indentation regressions  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 13:43:30 EDT, updated by Pierre Rouleau>

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
;; The main regression shape comes from `prg/bas7.sd7` around Line 696,
;; in the following Seed7 bas67.sd7 code, shown with their line numbers:
;;
;; 692
;; 693 const func boolean: isStringExpr (in string: symbol) is
;; 694   return symbol in string_var_name or
;; 695          symbol <> "" and
;; 696         (symbol[length(symbol)] = '$' or symbol[1] = '\"' or symbol[1] in defstr_var and
;; 697          not symbol[length(symbol)] in numeric_var_suffix);
;; 698
;;
;;
;; Regression tests for indentation of a multi-line boolean expression
;; like the one in prg/bas7.sd7 where:
;; - one continuation line ends with `and'
;; - the next continuation line starts with `('
;;
;; The expected indentation shape is:
;;
;;   const func boolean: ... is                     ; column 0
;;     return ...                                  ; column 2
;;            symbol <> "" and                     ; column 9
;;           (symbol[length(symbol)] = '$' ...     ; column 8
;;            not symbol[length(symbol)] ...       ; column 9

;; ---------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'seed7-mode)

(defconst seed7-test-indent--bas7-correct-code
  (concat
   "const func boolean: isStringExpr (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "         symbol <> \"\" and\n"
   "         (symbol[length(symbol)] = '$' or symbol[1] = '\\\"' or symbol[1] in defstr_var and\n"
   "          not symbol[length(symbol)] in numeric_var_suffix);\n")
  "Correctly indented bas7-style fixture.")

(defconst seed7-test-indent--bas7-misaligned-code
  (concat
   "const func boolean: isStringExpr (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "symbol <> \"\" and\n"
   "(symbol[length(symbol)] = '$' or symbol[1] = '\\\"' or symbol[1] in defstr_var and\n"
   "not symbol[length(symbol)] in numeric_var_suffix);\n")
  "Misindented version of `seed7-test-indent--bas7-correct-code'.")

(defun seed7-test-indent--goto-line (line)
  "Move point to the beginning of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test-indent--line-indentation (line)
  "Return indentation of LINE (1-based)."
  (save-excursion
    (seed7-test-indent--goto-line line)
    (current-indentation)))

(defun seed7-test-indent--line-first-char (line)
  "Return first non-whitespace character of LINE (1-based)."
  (save-excursion
    (seed7-test-indent--goto-line line)
    (back-to-indentation)
    (char-after)))

(ert-deftest seed7-indent/bas7-shape-indent-region-keeps-correct-layout ()
  "Indenting an already-correct bas7-style expression keeps the same layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--bas7-correct-code)
    (seed7-mode)

    (indent-region (point-min) (point-max))

    (should (= (seed7-test-indent--line-indentation 1) 0))
    (should (= (seed7-test-indent--line-indentation 2) 2))
    (should (= (seed7-test-indent--line-indentation 3) 9))
    (should (= (seed7-test-indent--line-indentation 4) 9))
    (should (= (seed7-test-indent--line-indentation 5) 10))

    (should (eq (seed7-test-indent--line-first-char 4) ?\())
    (should (string= (buffer-string)
                     seed7-test-indent--bas7-correct-code))))

(ert-deftest seed7-indent/bas7-shape-indent-region-fixes-misaligned-layout ()
  "Indenting a misaligned bas7-style expression restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--bas7-misaligned-code)
    (seed7-mode)

    (indent-region (point-min) (point-max))

    (should (= (seed7-test-indent--line-indentation 1) 0))
    (should (= (seed7-test-indent--line-indentation 2) 2))
    (should (= (seed7-test-indent--line-indentation 3) 9))
    (should (= (seed7-test-indent--line-indentation 4) 9))
    (should (= (seed7-test-indent--line-indentation 5) 10))

    (should (eq (seed7-test-indent--line-first-char 4) ?\())
    (should (string= (buffer-string)
                     seed7-test-indent--bas7-correct-code))))

;; ---------------------------------------------------------------------------
(provide 'seed7-test-indent-01)

;;; seed7-test-indent-01.el ends here
