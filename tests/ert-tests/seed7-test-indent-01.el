;;; seed7-test-indent-01.el --- ERT tests for Seed7 indentation regressions  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-07-20 15:58:12 EDT, updated by Pierre Rouleau>

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
;;   const func boolean: ... is                    ; column 0
;;     return ...                                  ; column 2
;;            symbol <> "" and                     ; column 9
;;            (symbol[length(symbol)] = '$' ...    ; column 9
;;             not symbol[length(symbol)] ...      ; column 10

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
;; Testing the bigfile.sd7 pattern

(defconst seed7-test-indent-02--case-when-searchdir-correct
  (concat
   "for fileName range dirElements do\n"
   "  if dirPath = \"/\" then\n"
   "    filePath := \"/\" & fileName;\n"
   "  else\n"
   "    filePath := dirPath & \"/\" & fileName;\n"
   "  end if;\n"
   "  case fileTypeSL(filePath) of\n"
   "    when {FILE_REGULAR}:\n"
   "      fileSize := bigFileSize(filePath);\n"
   "      dirSize +:= fileSize;\n"
   "      fileSizeMap @:= [filePath] fileSize;\n"
   "    when {FILE_DIR}:\n"
   "      dirSize +:= searchDir(filePath, fileSizeMap, dirSizeMap);\n"
   "    when {FILE_SYMLINK}:\n"
   "      noop; # Ignore symbolic link.\n"
   "  end case;\n"
   "end for;\n"))

(defconst seed7-test-indent-02--case-when-searchdir-misaligned
  (concat
   "for fileName range dirElements do\n"
   "  if dirPath = \"/\" then\n"
   "    filePath := \"/\" & fileName;\n"
   "  else\n"
   "    filePath := dirPath & \"/\" & fileName;\n"
   "  end if;\n"
   "  case fileTypeSL(filePath) of\n"
   "    when {FILE_REGULAR}:\n"
   "    fileSize := bigFileSize(filePath);\n"
   "    dirSize +:= fileSize;\n"
   "    fileSizeMap @:= [filePath] fileSize;\n"
   "    when {FILE_DIR}:\n"
   "    dirSize +:= searchDir(filePath, fileSizeMap, dirSizeMap);\n"
   "    when {FILE_SYMLINK}:\n"
   "    noop; # Ignore symbolic link.\n"
   "  end case;\n"
   "end for;\n"))

(ert-deftest seed7-indent/case-when-indent-region-matches-fresh-line-indentation ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--case-when-searchdir-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (let ((region-result (buffer-string)))
      (erase-buffer)
      (insert seed7-test-indent-02--case-when-searchdir-correct)
      (seed7-mode)
      (goto-char (point-min))
      (while (not (eobp))
        (seed7-indent-line)
        (forward-line 1))
      (should (string= region-result (buffer-string))))))

;; ---------------------------------------------------------------------------

(defconst seed7-test-indent--elliptic-sibling-calls-correct
  (concat
   "const ellipticCurve: secp192k1 is ellipticCurve(\n"
   "                                                192, \"secp192k1\",\n"
   "                                                3_);\n"
   "\n"
   "(**\n"
   " *  The elliptical curve secp192r1.\n"
   " *)\n"
   "const ellipticCurve: secp192r1 is ellipticCurve(\n"
   "                                                192, \"secp192r1\",\n"
   "                                                4_);\n")
  "Correct layout for consecutive top-level multiline calls.")

(defconst seed7-test-indent--elliptic-sibling-calls-misaligned
  (concat
   "const ellipticCurve: secp192k1 is ellipticCurve(\n"
   "                                                192, \"secp192k1\",\n"
   "                                                3_);\n"
   "\n"
   "(**\n"
   " *  The elliptical curve secp192r1.\n"
   " *)\n"
   "                                                const ellipticCurve: secp192r1 is ellipticCurve(\n"
   "                                                192, \"secp192r1\",\n"
   "                                                4_);\n")
  "Regression input: sibling declaration inherits the prior `);' column.")

(ert-deftest seed7-indent/elliptic-sibling-call-layout-is-stable ()
  "A declaration after a completed multiline call remains top-level."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--elliptic-sibling-calls-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    ;; Line 8 is the second top-level `const ellipticCurve:' declaration.
    (should (= (seed7-test-indent--line-indentation 8) 0))
    (should (string=
             (buffer-string)
             seed7-test-indent--elliptic-sibling-calls-correct))))

(ert-deftest seed7-indent/elliptic-sibling-call-layout-is-corrected ()
  "A sibling declaration must not inherit the preceding `);' indentation."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--elliptic-sibling-calls-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    ;; Line 8 must align with Line 1, not with the closing `);' on Line 3.
    (should (= (seed7-test-indent--line-indentation 8) 0))
    (should (string=
             (buffer-string)
             seed7-test-indent--elliptic-sibling-calls-correct))))

;; ---------------------------------------------------------------------------

(defconst seed7-test-indent--string-action-siblings-correct
  (concat
   "const func string: striRange (in string: stri) [ (in integer: start) ..\n"
   "                                                    (in integer: stop) ] is action \"STR_RANGE\";\n"
   "\n"
   "(**\n"
   " *  Get a substring with a requested maximum length.\n"
   " *)\n"
   "const func string: striSubstr (in string: stri) [ (in integer: start) len\n"
   "                                                     (in integer: length) ] is action \"STR_SUBSTR\";\n"
   "\n"
   "(**\n"
   " *  Get a substring with a guaranteed length.\n"
   " *)\n"
   "const func string: striFixLen (in string: stri) [ (in integer: start) fixLen\n"
   "                                                     (in integer: length) ] is action \"STR_SUBSTR_FIXLEN\";\n"
   "\n"
   "(**\n"
   " *  Append EXTENSION to DESTINATION.\n"
   " *)\n"
   "const proc: appendTo (inout string: destination) &:= (in string: extension) is action \"STR_APPEND\";\n")
  "Correct layout for top-level action declarations following comments.")

(defconst seed7-test-indent--string-action-siblings-misaligned
  (concat
   "const func string: striRange (in string: stri) [ (in integer: start) ..\n"
   "                                                    (in integer: stop) ] is action \"STR_RANGE\";\n"
   "\n"
   "(**\n"
   " *  Get a substring with a requested maximum length.\n"
   " *)\n"
   "                                                    const func string: striSubstr (in string: stri) [ (in integer: start) len\n"
   "                                                                                                         (in integer: length) ] is action \"STR_SUBSTR\";\n"
   "\n"
   "(**\n"
   " *  Get a substring with a guaranteed length.\n"
   " *)\n"
   "                                                                                                         const func string: striFixLen (in string: stri) [ (in integer: start) fixLen\n"
   "                                                                                                                             (in integer: length) ] is action \"STR_SUBSTR_FIXLEN\";\n"
   "\n"
   "(**\n"
   " *  Append EXTENSION to DESTINATION.\n"
   " *)\n"
   "                                                                                                                             const proc: appendTo (inout string: destination) &:= (in string: extension) is action \"STR_APPEND\";\n")
  "Input for accumulated indentation after multiline action declarations.")

(ert-deftest seed7-indent/string-action-siblings-are-top-level-after-comments ()
  "Top-level action declarations do not inherit parameter continuation columns."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent--string-action-siblings-misaligned)
    (seed7-mode)

    (indent-region (point-min) (point-max))

    ;; Declaration headers, rather than parameter continuations, must be
    ;; aligned as top-level siblings.
    (should (= (seed7-test-indent--line-indentation 1) 0))
    (should (= (seed7-test-indent--line-indentation 7) 0))
    (should (= (seed7-test-indent--line-indentation 13) 0))
    (should (= (seed7-test-indent--line-indentation 19) 0))

    ;; Continuation lines remain aligned within their own declarations.
    (should (= (seed7-test-indent--line-indentation 2) 52))
    (should (= (seed7-test-indent--line-indentation 8) 53))
    (should (= (seed7-test-indent--line-indentation 14) 53))

    (should (string=
             (buffer-string)
             seed7-test-indent--string-action-siblings-correct)))


    ;; A multiline top-level native/action declaration must itself be
    ;; recognized as a completed definition, even before region indentation.
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (seed7-line-is-defun-end 0) 0)))


;; ---------------------------------------------------------------------------
(provide 'seed7-test-indent-01)

;;; seed7-test-indent-01.el ends here
