;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-04-04 14:44:29 EDT, updated by Pierre Rouleau>

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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Seed7 Customization
;; ===================

(defgroup seed7 nil
  "Seed7 Programming Language support configuration."
  :group 'languages
  :link '(url-link  :tag "PEL @ GitHub"
                    "https://github.com/pierre-rouleau/seed7-mode")
  :package-version '(seed7-mode . "0.0.1"))

(defcustom seed7-uses-block-comment nil
  "Seed7 comments are block comments when non-nil, line comments otherwise."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

(defgroup seed7-faces nil
  "Fontification colors"
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'seed7)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Seed7 Keywords
;; ==============
;;
;; First define private list of keyword strings (some may be combination of
;; Seed7 keywords) inside a constant.  Then use the powerful `rx-to-string'
;; macro to create a regexp for this group.
;;
;; In the future, the private lists of keywords may be dynamically loaded for
;; specific Seed7 syntax and the constants will become variables to allow
;; the mode to dynamically adapt to the Seed7 extended systax.

;; Seed7 keywords used in statements
;; ---------------------------------
;;
;; Instead of only using "end" in the regexp, I placed the "end KEYWORD" for
;; the various KEYWORDS that Seed7 supports.

(defconst seed7--in-statement-keywords
  '("begin"
    "block"
    "case"
    "const"
    "do"
    "downto"
    "else"
    "elsif"
    "end block"            ; end is the keyword, but always used with a suffix
    "end for"              ; each combination is identified.
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

(defconst seed7-in-statement-keywords-regexp
  (format "^ *%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--in-statement-keywords)))
          "\\_>"))

;; Seed7 statement introducing keywords
;; ------------------------------------

(defconst seed7--statement-introducing-keywords
  '("block"
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

(defconst seed7-statement-introducing-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--statement-introducing-keywords)))
          "\\_>"))

;; Seed7 keywords used in middle of statements
;; -------------------------------------------

(defconst seed7--in-middle-statement-keywords
  '("begin"
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

(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:space:]]"
          (rx-to-string
           `(: (or ,@seed7--in-middle-statement-keywords)))
          "[[:punct:][:space:]]"))


;; Seed7 declaration introduction keywords
;; ---------------------------------------

(defconst seed7--declaration-intro-keywords
  '("const"
    "in"
    "inout"
    "ref"
    "val"
    "var"))

(defconst seed7-declaration-intro-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(:  (or ,@seed7--declaration-intro-keywords)))
          "\\_>"))

;; Seed7 Operator Symbols
;; ----------------------

(defconst seed7--operator-symbols
  '("and"
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

(defconst seed7-operator-symbols-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(:  (or ,@seed7--operator-symbols)))
          "\\_>"))


;; Seed7 Predefined Types
;; ----------------------

(defconst seed7--predefined-types
  '("array"
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

(defconst seed7-predefined-types-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--predefined-types)))
          "\\_>"))

;; See7 Predefined Constants
;; -------------------------

(defconst seed7--predefined-constants
  '("E"
    "EOF"
    "FALSE"
    "PI"
    "TRUE"))

(defconst seed7-predefined-constants-regxp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--predefined-constants)))
          "\\_>"))

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
;;   <&

(defconst seed7-assignment-operator-regxp
  "\\(?:\\(?:[-\\+\\*/&|@]\\)\\|\\(?:<<\\|>>\\|><\\)\\)?:=")

;; Seed Other operators
;; --------------------
;; "<&"
;; "=" "<>"
;; ">" ">="
;; "<" "<="
(defconst seed7-other-operator-regexp
  " \\(\\(<&\\)\\|\\(=\\)\\|\\(<>\\)\\|\\([<>][=]?\\)\\) ")


;; Seed7 Faces
;;
;; ===================================== ================================================
;; Variable name                         Description: Face for...
;; ===================================== ================================================
;; **Standard Emacs Faces**
;; `font-lock-builtin-face'              Builtins.
;; `font-lock-comment-delimiter-face'    Comment delimiters.
;; `font-lock-comment-face'              Comments.
;; `font-lock-constant-face'             Constant and label names.
;; `font-lock-doc-face'                  Documentation.
;; `font-lock-doc-markup-face'           Documentation mark-up.
;; `font-lock-function-name-face'        Function names.
;; `font-lock-keyword-face'              Keywords.
;; `font-lock-negation-char-face'        Easy to overlook negation.
;;                                       Something like an '!' or the 'n' in ifndef.
;; `font-lock-preprocessor-face'         Preprocessor directives.
;; `font-lock-string-face'               Strings.
;; `font-lock-type-face'                 Type and class names.
;; `font-lock-variable-name-face'        Variable names.
;; `font-lock-warning-face'              Things that should stand out.
;;
;; **Extra Faces for Seed7**
;; `seed7-in-statement-keyword-face'     Seed7 keywords used inside statements.
;; ===================================== ================================================



(defun seed7-choose-color (&rest list)
  (let (answer)
    (while list
      (or answer
          (if (or (color-defined-p (car list))
		  (null (cdr list)))
	      (setq answer (car list))))
      (setq list (cdr list)))
    answer))

(defvar seed7-dark-background
  (seed7-choose-color "navy" "os2blue" "darkgreen"))
(defvar seed7-dark-foreground
  (seed7-choose-color "orchid1" "orange"))

;; define seed7-in-statement-keyword-face
(defface seed7-in-statement-keyword
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-33" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight array names."
  :group 'seed7-faces)


(defconst seed7-font-lock-keywords
  (list
   (cons seed7-in-statement-keywords-regexp          (list 1 seed7-in-statement-keyword-face))
   (cons seed7-statement-introducing-keywords-regexp (list 1 font-lock-keyword-face))
   (cons seed7-in-middle-statement-keywords-regexp   (list 1 font-lock-keyword-face))
   (cons seed7-declaration-intro-keywords-regexp     (list 1 font-lock-keyword-face))
   (cons seed7-operator-symbols-regexp               (list 1 font-lock-keyword-face))
   (cons seed7-predefined-types-regexp               (list 1 font-lock-type-face))
   (cons seed7-predefined-constants-regxp            (list 1 font-lock-constant-face))
   (cons seed7-assignment-operator-regxp             font-lock-builtin-face)
   (cons seed7-other-operator-regexp                 (list 1 font-lock-builtin-face))
   )
  "Alist of Seed7 base keywords with each a specific face")


;; Seed7 Comments
;; --------------
;;
;; Region:      "(\*" "\*)"
;; To line end: "#"


(defconst seed7-block-comment-starter "(*")
(defconst seed7-block-comment-ender   "*)")
(defconst seed7-block-comment-prefix  "**")
(defconst seed7-line-comment-starter  "# ")
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
