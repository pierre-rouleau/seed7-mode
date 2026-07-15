;;; seed7-test-indent-03.el --- ERT tests for array/set-block-end indentation regressions  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the SEED7-MODE package.
;; This file is not part of GNU Emacs.

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Regression tests for the case where an array/set-literal definition ends
;; on a line that contains *only* the closing delimiter (e.g. `  );' or
;; `  };'), immediately followed (after blank lines and/or a block comment)
;; by another top-level declaration.
;;
;; Before the fix to `seed7-line-at-endof-array-definition-block' and
;; `seed7-line-at-endof-set-definition-block', the strict chained inequality
;;
;;   (< block-start-pos line-start-pos enclosing-block-end-pos line-end-pos)
;;
;; failed whenever `line-start-pos' and `enclosing-block-end-pos' were equal
;; (i.e. the closing delimiter is the very first thing on the line), causing
;; the "just after end of array/set definition block" clause in
;; `seed7-calc-indent' to be skipped.  Control then fell through to the
;; generic indent-step fallback, which incorrectly re-used the raw column of
;; the previous line (the position of the closing delimiter) instead of the
;; array/set header's own column.
;;
;; Real-world case: lib/asn1.s7i, around line 71 --
;;
;; 33 const array string: classTagName is [0] (
;; ...
;; 65   "(use long-form)"
;; 66   );
;; 67
;; 68 (**
;; 69  *  Tag type used by ASN.1/BER data elements.
;; 70  *)
;; 71 const type: asn1TagType is new enum
;; ...
;;
;; Line 71 must be indented at column 0 (same as the `const array string:
;; classTagName' header on line 33), not column 2 (the column of the `)' on
;; line 66).

;; ---------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'seed7-mode)

(defconst seed7-test-indent-03--array-end-then-decl-correct
  (concat
   "const array string: classTagName is [0] (\n"
   "  \"EOC (End-of-Content)\",\n"
   "  \"BOOLEAN\",\n"
   "  \"(use long-form)\"\n"
   "  );\n"
   "\n"
   "(**\n"
   " *  Tag type used by ASN.1/BER data elements.\n"
   " *)\n"
   "const type: asn1TagType is new enum\n"
   "    tagEndOfContent,\n"
   "    tagBoolean,\n"
   "    tagUseLongForm\n"
   "  end enum;\n")
  "Correctly indented reproduction of the lib/asn1.s7i Line 71 shape.")

(defconst seed7-test-indent-03--array-end-then-decl-misaligned
  (concat
   "const array string: classTagName is [0] (\n"
   "  \"EOC (End-of-Content)\",\n"
   "  \"BOOLEAN\",\n"
   "  \"(use long-form)\"\n"
   "  );\n"
   "\n"
   "(**\n"
   " *  Tag type used by ASN.1/BER data elements.\n"
   " *)\n"
   "  const type: asn1TagType is new enum\n"          ; <- wrongly indented at col 2
   "    tagEndOfContent,\n"
   "    tagBoolean,\n"
   "    tagUseLongForm\n"
   "  end enum;\n")
  "Misindented version of `seed7-test-indent-03--array-end-then-decl-correct'.")

(defconst seed7-test-indent-03--set-end-then-decl-correct
  (concat
   "const set of string: tagNameSet is {\n"
   "  \"EOC\",\n"
   "  \"BOOLEAN\"\n"
   "  };\n"
   "\n"
   "(** doc comment *)\n"
   "const type: asn1TagType is new enum\n"
   "    tagEndOfContent\n"
   "  end enum;\n")
  "Correctly indented set-literal analogue of the array-block regression.")

(defconst seed7-test-indent-03--set-end-then-decl-misaligned
  (concat
   "const set of string: tagNameSet is {\n"
   "  \"EOC\",\n"
   "  \"BOOLEAN\"\n"
   "  };\n"
   "\n"
   "(** doc comment *)\n"
   "  const type: asn1TagType is new enum\n"           ; <- wrongly indented at col 2
   "    tagEndOfContent\n"
   "  end enum;\n")
  "Misindented version of `seed7-test-indent-03--set-end-then-decl-correct'.")

(defun seed7-test-indent-03--goto-line (line)
  "Move point to the beginning of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test-indent-03--line-indentation (line)
  "Return indentation of LINE (1-based)."
  (save-excursion
    (seed7-test-indent-03--goto-line line)
    (current-indentation)))

(ert-deftest seed7-indent/array-block-end-then-decl-keeps-correct-layout ()
  "Indenting an already-correct array-block-end-then-decl layout is stable."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--array-end-then-decl-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 1) 0))   ; const array string: ...
    (should (= (seed7-test-indent-03--line-indentation 5) 2))   ; `  );'
    (should (= (seed7-test-indent-03--line-indentation 7) 0))   ; `(**'
    (should (= (seed7-test-indent-03--line-indentation 10) 0))  ; const type: ... is new enum
    (should (string= (buffer-string)
                      seed7-test-indent-03--array-end-then-decl-correct))))

(ert-deftest seed7-indent/array-block-end-then-decl-fixes-misaligned-layout ()
  "Indenting the misaligned layout restores column 0 for the declaration
that follows the closing `);' of an array literal."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--array-end-then-decl-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 10) 0))  ; must NOT be 2
    (should (string= (buffer-string)
                      seed7-test-indent-03--array-end-then-decl-correct))))

(ert-deftest seed7-indent/set-block-end-then-decl-keeps-correct-layout ()
  "Indenting an already-correct set-block-end-then-decl layout is stable."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--set-end-then-decl-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 1) 0))
    (should (= (seed7-test-indent-03--line-indentation 4) 2))   ; `  };'
    (should (= (seed7-test-indent-03--line-indentation 7) 0))   ; const type: ...
    (should (string= (buffer-string)
                      seed7-test-indent-03--set-end-then-decl-correct))))

(ert-deftest seed7-indent/set-block-end-then-decl-fixes-misaligned-layout ()
  "Indenting the misaligned set-literal layout restores column 0."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-03--set-end-then-decl-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-03--line-indentation 7) 0))
    (should (string= (buffer-string)
                      seed7-test-indent-03--set-end-then-decl-correct))))

;; ---------------------------------------------------------------------------
(provide 'seed7-test-indent-03)

;;; seed7-test-indent-03.el ends here
