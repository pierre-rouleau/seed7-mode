;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-04-03 14:24:23 EDT, updated by Pierre Rouleau>

;; This file is not part of GNU Emacs.

;; Copyright (C) 2025  Pierre Rouleau
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
;; This is currently a *very* early/rough work-in-progress implementation for
;; Seed7 support.  It is not stable and may change.

;; Feature wish-list:
;;
;; - Syntax and semantics support for Seed7
;; - Support all comment syntax: colorization, creation, deletion
;; - keyword colorization
;; - Launch help on keywords, perhaps implement statement help
;; - Indentation help: with TAB: adjusts indentation when typed
;;                               anywhere on the line.
;; - Keyword Completion help.
;; - Navigation help
;; - Template help for code creation
;; - Commands to compile with error reporting

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;


;;; --------------------------------------------------------------------------
;;; Code:
;;
;; References:
;; - Seed7 formal specification:  "Seed7 Reference Manual".
;; -   See http://seed7.sourceforge.net/

;; Seed7 Syntax Information:
;; - https://thomasmertes.github.io/Seed7Home/faq.htm#add_syntax_highlighting

;; Seed7 keywords used in statements
;; ---------------------------------
;;
;; Instead of only using "end" in the regexp, I placed the "end KEYWORD" for
;; the various KEYWORDS that Seed7 supports.
(defconst seed7-in-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:punct:][:space:]]"
          (rx (or
               "begin"
               "block"
               "case"
               "const"
               "do"
               "downto"
               "else"
               "elsif"
               "end for"
               "end func"
               "end if"
               "end while"
               "enum"
               "exception"
               "for"
               "forward"
               "func"
               "if"
               "in"
               "include"
               "inout"
               "is"
               "local"
               "new"
               "of"
               "otherwise"
               "param"
               "range"
               "ref"
               "repeat"
               "result"
               "return"
               "step"
               "struct"
               "syntax"
               "system"
               "then"
               "to"
               "until"
               "val"
               "var"
               "when"
               "while"))
          "[[:punct:][:space:]]"))

;; Seed7 statement introducing keywords
;; ------------------------------------

(defconst seed7-statement-introducing-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:punct:][:space:]]"
          (rx (or
               "block"
               "case"
               "enum"
               "for"
               "func"
               "if"
               "include"
               "repeat"
               "struct"
               "syntax"
               "system"
               "while"))
          "[[:punct:][:space:]]"))

;; Seed7 keywords used in middle of statements
;; -------------------------------------------

(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:punct:][:space:]]"
          (rx (or
               "begin"
               "do"
               "downto"
               "else"
               "elsif"
               "end"
               "exception"
               "local"
               "new"
               "otherwise"
               "param"
               "range"
               "result"
               "step"
               "then"
               "to"
               "until"
               "when"))
          "[[:punct:][:space:]]"))


;; Seed7 declaration statement introducing keywords
;; ------------------------------------------------

(defconst seed7-declaration-into-keywords
  (format "%s\\(%s\\)%s"
          "[[:punct:][:space:]]"
          (rx (or
               "const"
               "in"
               "inout"
               "ref"
               "val"
               "var"))
          "[[:punct:][:space:]]"))

;; Seed7 Operator Symbols
;; ----------------------

(defconst seed7-operator-symbols-regexp
  (format "%s\\(%s\\)%s"
          "[(,: ]"
          (rx (or
               "and"
               "conv"
               "digits"
               "div"
               "exp"
               "in"
               "lapd0"
               "lpad"
               "mdiv"
               "mod"
               "mult"
               "not"
               "or"
               "parse"
               "rem"
               "rpad"
               "sci"
               "times"
               "varConv"))
          "[:, )]"))


;; Seed7 Predefined Types
;; ----------------------

(defconst seed7-predefined-types-regexp
  (format "%s\\(%s\\)%s"
          "[[:punct:][:space:]]"
          (rx (or
               "array"
               "bigInteger"
               "bigRational"
               "bin32"
               "bin64"
               "bitset"
               "boolean"
               "char"
               "clib_file"
               "color"
               "complex"
               "duration"
               "enum"
               "expr"
               "file"
               "float"
               "func"
               "integer"
               "object"
               "proc"
               "program"
               "rational"
               "reference"
               "ref_list"
               "set"
               "string"
               "struct"
               "text"
               "time"
               "type"
               "void"
               "PRIMITIVE_WINDOW"))
          "[[:punct:][:space:]]"))

;; See7 Predefined Constants
;; -------------------------

(defconst seed7-predefined-constants-regxp
  "[[:punct:][:space:]]\\(E\\|EOF\\|FALSE\\|PI\\|TRUE\\)[[:punct:][:space:]]")

;; Seed7 operator symbol keywords
;; ------------------------------


;; syntax match    sd7Operator "[-+*/<>&[\]:{}@]"
;; syntax keyword  sd7Operator ** ><
;; syntax match    sd7Operator "[/<>]="
;; syntax match    sd7Operator "\.\."
;; syntax match    sd7Operator "="
;;
;; S7 Assignment operators:
;;    :=
;;   -:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)-:=(in_bigInteger)
;;   +:=        https://seed7.sourceforge.net/libraries/integer.htm#(inout_integer)+:=(in_integer)
;;   *:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)*:=(in_bigInteger)
;;   /:=        https://seed7.sourceforge.net/libraries/bigrat.htm#(inout_bigRational)/:=(in_bigRational)
;;  <<:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3C%3C:=(in_integer)
;;  >>:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3E%3E:=(in_integer)
;;  ><:=        https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)%3E%3C:=(in_bin32)
;;   &:=        https://seed7.sourceforge.net/libraries/array.htm#(inout_arrayType)&:=(in_arrayType)
;;   |:=        https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)|:=(in_bin32)
;;   @:=[       https://seed7.sourceforge.net/libraries/bitset.htm#(inout_bitset)@:=_[(in_integer)](in_boolean)
;;
(defconst seed7-assignment-operator-regxp
  "\\(?:\\(?:[-\\+\\*/&|@]\\)\\|\\(?:<<\\|>>\\|><\\)\\)?:=")
;;

(defconst seed7-font-lock-keywords
  (list
   (cons seed7-in-statement-keywords-regexp          (list 1 font-lock-keyword-face))
   (cons seed7-statement-introducing-keywords-regexp (list 1 font-lock-keyword-face))
   (cons seed7-in-middle-statement-keywords-regexp   (list 1 font-lock-keyword-face))
   (cons seed7-declaration-into-keywords             (list 1 font-lock-keyword-face) )
   (cons seed7-operator-symbols-regexp               (list 1 font-lock-keyword-face))
   (cons seed7-predefined-types-regexp               (list 1 font-lock-type-face))
   (cons seed7-predefined-constants-regxp            (list 1 font-lock-constant-face))
   (cons seed7-assignment-operator-regxp             font-lock-keyword-face))
  "Alist of Seed7 base keywords with each a specific face")


;;
;; Seed7 Comments
;;
;; Region:      "(\*" "\*)"
;; To line end: "#"
(defgroup seed7 nil
  "Seed7 Programming Language support configuration."
  :group 'languages)

(defconst  seed7-block-comment-starter "(*")
(defconst  seed7-block-comment-ender   "*)")
(defconst  seed7-block-comment-prefix  "**")
(defconst  seed7-line-comment-starter  "# ")
(defcustom seed7-uses-block-comment nil
  "Seed7 comments are block comments when non-nil, line comments otherwise."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

(defconst seed7-comment-regxp "#.*$")

(defun seed7--new-state-for (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on ARG.
  ;; If ARG is nil or zero, toggle the state,
  ;; If ARG is negative, turn the state off,
  ;; If ARG is positive, turn the state on.
  (if (or (not arg)
	      (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun seed7--set-comment-style (use-block &optional verbose)
  "Set Seed7 command style to block style when USE-BLOCK is non nil.
Set it to line-style otherwise."
  (setq seed7-uses-block-comment use-block)
  (setq comment-start
	    (concat (if seed7-uses-block-comment
		            seed7-block-comment-starter
		          seed7-line-comment-starter)
		        " "))
  (setq comment-end
	    (if seed7-uses-block-comment
	        (concat " " seed7-block-comment-ender)
	      ""))
  (when verbose
    (message "Now use %s style comments" (if use-block
                                             "block"
                                           "line"))))

(defun seed7-toggle-comment-style (&optional arg)
  "Toggle the Seed7 comment style between block and line comments.
Optional numeric ARG, if supplied, switches to block comment
style when positive, to line comment style when negative, and
just toggles it when zero or left out."
  (interactive "P")
  (let ((use-block (cond
	                ((and seed7-line-comment-starter seed7-block-comment-starter)
	                 (seed7--new-state-for arg seed7-uses-block-comment))
	                (seed7-line-comment-starter nil)
	                (t t))))
    (seed7--set-comment-style use-block 'verbose))

  ;; If necessary, invert the sense of fontification of wrong style comments.
  ;; (when (and c-mark-wrong-style-of-comment
  ;;            font-lock-mode
  ;;            seed7-block-comment-starter
  ;;            seed7-block-comment-ender)
  ;;   (save-excursion
  ;;     (save-restriction
  ;;       (widen)
  ;;       (goto-char (point-min))
  ;;       (c-font-lock-flush (point-min) (point-max)))))
  ;; (c-update-modeline)
  ;; (c-keep-region-active)
  )


;;;###autoload
(define-derived-mode seed7-mode pascal-mode "seed7"
  "Major mode for editing Seed7 files.
This is a preliminary implementation, based on `pascal-mode'"
  (seed7--set-comment-style seed7-uses-block-comment)
  (setq font-lock-defaults '((seed7-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s[di]7\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;;; seed7-mode.el ends here
