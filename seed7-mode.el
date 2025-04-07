;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-04-07 18:55:02 EDT, updated by Pierre Rouleau>

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
;;
;; [:todo 2025-04-06, by Pierre Rouleau: Fix following problems:
;;  Known problems:
;;  # 01  Find why we need defvar of deface symbols. These should not be
;;        required.
;;  # 02  Back-Slash escaped double quote inside string is not recognized.
;;        - The prog-mode based partly solves this but introduces issues with
;;          escaped single quoted.  Investigate and find the proper syntax
;;          logic to use.
;;  # 03  Complete defface definitions:
;;        - Support light and dark backgrounds.
;;        - Update coloring, once testing is complete.
;;        - Maybe add ability to reduce number of faces used (or re-use the
;;          same face for various elements).  It would allow dual use: one
;;          with lots of different renderings and another with not that many,
;;          a more conservative approach.
;;  # 04  Cleanup keyword definitions.  There are probably too many
;;        defined, and these are used for preliminary testing.  Once testing
;;        of this is completed, remove the duplication and keep what is
;;        strictly necessary to eliminate un-required extra processing.
;; ]
;;
;;
;; Code Organization Layout
;;
;; -  Seed7 Customization
;; -  Seed7 Keywords
;;    - Seed7 Pragmas
;;    - Seed7 include
;;    - Seed7 keywords used in statements
;;    - Seed7 statement introducing keywords
;;    - Seed7 keywords used in middle of statements
;;    - Seed7 declaration introduction keywords
;;    - Seed7 Predefined Types
;;    - Seed7 Predefined Constants
;;    - Seed7 Predefined Variables
;;    - Seed7 Predefined errinfo value
;;    - Seed7 Operator Symbols
;;    - Seed7 Assignment Operator Symbols
;;    - Seed7 Arithmetic Operators
;;    - Seed Other operators
;; - Seed7 Faces
;; - Seed7 Font Locking Control
;; - Seed7 Comments Control
;; - Seed7 iMenu Support
;; - Seed7 Speedbar Support
;; - Seed7 Code Navigation
;; - Seed7 keymap
;; - Seed7 Major Mode

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'simple)         ; use `move-beginning-of-line'
(require 'speedbar)       ; use `speedbar-add-supported-extension'
(require 'subr-x)         ; use: `string-trim'

;;; --------------------------------------------------------------------------
;;; Code:
;;
;; References:
;; - Seed7 formal specification:  "Seed7 Reference Manual".
;; -   See http://seed7.sourceforge.net/

;; Seed7 Syntax Information:
;; - https://thomasmertes.github.io/Seed7Home/faq.htm#add_syntax_highlighting

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;* Seed7 Customization
;;  ===================

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
;;* Seed7 Keywords
;;  ==============
;;
;; First define private list of keyword strings (some may be combination of
;; Seed7 keywords) inside a constant.  Then use the powerful `rx-to-string'
;; macro to create a regexp for this group.
;;
;; In the future, the private lists of keywords may be dynamically loaded for
;; specific Seed7 syntax and the constants will become variables to allow
;; the mode to dynamically adapt to the Seed7 extended systax.

;;* Seed7 pragmas
;;  -------------
;;
;; Ref: https://thomasmertes.github.io/Seed7Home/manual/decls.htm#Pragmas

(defconst seed7--pragma-keywords
  '(
    "library"
    "message"
    "info"
    "trace"
    "decls"
    "names"))

(defconst seed7-pragma-keywords-regexp
  (format "^%s\\(\\$ +%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--pragma-keywords)))
          "\\_>"))

;;* Seed7 include
;;  -------------
;;
;; The very first include statement requires a leading '$' but not
;; the following ones."
(defconst seed7-include-regexp
  "^\\(\\$? +\\(include\\)\\) ")


;;* Seed7 keywords used in statements
;;  ---------------------------------
;;
;; All keywords a re listed here, but some are commented out because
;; they are part of another list below.  The ones left are the ones that
;; are at the beginning of a line (with or without leading white space),
;; identified in `seed7--lead-in-statement-keywords' and some that can also
;; be in the middle or end of line, which are identified by
;;`seed--in-statement-keywords'.

(defconst seed7--lead-in-statement-keywords
  '("begin"
    ;; "block"
    ;; "case"
    ;; "catch"
    "const"
    "do"
    "downto"
    ;; "else"
    ;; "elsif"
    ;; "end"
    ;; "enum"
    ;; "exception"
    ;; "for"
    "forward"
    ;; "func"
    ;; "if"
    "in"
    ;; "include"
    "inout"
    "is"
    "local"
    "new"
    "of"
    ;; "otherwise"
    "param"
    "raise"                      ; currently missing in the Seed7 keyword list
    "range"
    "ref"
    ;; "repeat"
    "result"
    "return"
    "step"
    ;; "struct"
    ;; "syntax"
    ;; "system"
    "then"
    "to"
    ;; "until"
    "val"
    "var"
    ;; "when"
    ;; "while"
    ))

(defconst seed7-lead-in-statement-keywords-regexp
  (format "^ *%s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--lead-in-statement-keywords)))
          "\\_>"))

(defconst seed7--in-statement-keywords
  '("do"
    "is"
    "then"))

(defconst seed7-in-statement-keywords-regexp
  (format ". %s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--in-statement-keywords)))
          "\\_>"))

;;* Seed7 statement introducing keywords
;;  ------------------------------------
;;

(defconst seed7--statement-introducing-keywords
  '("block"       "end block"
    "case"        "when" "otherwise" "end case"
    "exception"   "catch" ; otherwise
    "enum"        "end enum"
    "for"         "end for"
    "func"        "end func"
    "if"          "else" "elsif" "end if"
    "repeat"      "until"
    "struct"      "end struct"
    "syntax"
    "system"
    "while"       "end while"))

(defconst seed7-statement-introducing-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--statement-introducing-keywords)))
          "\\_>"))

;;* Seed7 keywords used in middle of statements
;;  -------------------------------------------

(defconst seed7--in-middle-statement-keywords
  '("begin"
    "do"
    "downto"
    ;; "else"
    ;; "elsif"
    ;; "end"
    "exception"
    "local"
    "new"
    ;; "otherwise"
    "param"
    "range"
    "result"
    "step"
    "then"
    "to"
    ;; "until"
    ;; "when"
    ))

(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:space:]]"
          (rx-to-string
           `(: (or ,@seed7--in-middle-statement-keywords)))
          "[[:punct:][:space:]]"))


;;* Seed7 declaration introduction keywords
;;  ---------------------------------------

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


;;* Seed7 Predefined Types
;;  ----------------------

(defconst seed7--predefined-types
  '("array"
    "bigInteger"
    "bigRational"
    "bin32"
    "bin64"
    "bitset"
    "boolean"
    "bstring"
    "category"
    "char"
    "clib_file"
    "color"
    "complex"
    "database"
    "duration"
    "enum"
    "expr"
    "file"
    "fileSys"
    "float"
    "func"
    "hash"
    "integer"
    "object"
    "proc"
    "process"
    "program"
    "rational"
    "reference"
    "ref_list"
    "set"
    "sqlStatement"
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

;;* Seed7 Predefined Constants
;;  -------------------------

(defconst seed7--predefined-constants
  '("E"
    "EOF"
    "FALSE"
    "NIL"
    "PI"
    "STD_NULL"
    "TRUE"))

(defconst seed7-predefined-constants-regxp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--predefined-constants)))
          "\\_>"))

;;* Seed7 Predefined Variables
;;  --------------------------

(defconst seed7--predefined-variables
  '(
    "CONSOLE_KEYBOARD"
    "GRAPH_KEYBOARD"
    "IN"
    "KEYBOARD"
    "OUT"
    "STD_CONSOLE"
    "STD_ERR"
    "STD_IN"
    "STD_NULL"
    "STD_OUT"))

(defconst seed7-predefined-variables-regxp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--predefined-variables)))
          "\\_>"))

;;* Seed7 Predefined errinfo value
;;  ------------------------------
(defconst seed7--errinfo-values
  '(
    "OKAY_NO_ERROR"
    "MEMORY_ERROR"
    "NUMERIC_ERROR"
    "OVERFLOW_ERROR"
    "RANGE_ERROR"
    "INDEX_ERROR"
    "FILE_ERROR"
    "DATABASE_ERROR"
    "GRAPHIC_ERROR"
    "ACTION_ERROR"
    "CREATE_ERROR"
    "DESTROY_ERROR"
    "COPY_ERROR"
    "IN_ERROR"))

(defconst seed7-errinfo-values-regxp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--errinfo-values)))
          "\\_>"))

;;* Seed7 Operator Symbols
;;  ----------------------

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

;;* Seed7 Assignment Operator Symbols
;;  ---------------------------------


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

;;* Seed7 Arithmetic Operators
;;  --------------------------
;;
;;  + - * / **
;;
(defconst seed7-arithmetic-operator-regexp
  "[[:alnum:]_ )]\\([/*+-]\\)[[:alnum:]_ (]"
  )


;;* Seed Other operators
;; --------------------
;; "<&"
;; "=" "<>"
;; ">" ">="
;; "<" "<="
(defconst seed7-other-operator-regexp
  " \\(\\(<&\\)\\|\\(=\\)\\|\\(<>\\)\\|\\([<>][=]?\\)\\) ")


;;* Seed7 Faces
;;  ===========
;;
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

(defface seed7-pragma-keyword-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-20" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight pragma keywords."
  :group 'seed7-faces)
(defvar seed7-pragma-keyword-face 'seed7-pragma-keyword-face)


(defface seed7-include-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-105" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight include."
  :group 'seed7-faces)
(defvar seed7-include-face 'seed7-include-face)


(defface seed7-in-statement-keyword-face
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
  "Font Lock mode face used to highlight keywords."
  :group 'seed7-faces)
(defvar seed7-in-statement-keyword-face 'seed7-in-statement-keyword-face)


(defface seed7-statement-introducing-keyword-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-44" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords that introduce a statement."
  :group 'seed7-faces)
(defvar seed7-statement-introducing-keyword-face 'seed7-statement-introducing-keyword-face)


(defface seed7-in-middle-statement-keyword-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-38" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords used in middle of statements."
  :group 'seed7-faces)
(defvar seed7-in-middle-statement-keyword-face 'seed7-in-middle-statement-keyword-face)


(defface seed7-intro-statement-keyword-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-39" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight statement intro keywords."
  :group 'seed7-faces)
(defvar seed7-intro-statement-keyword-face 'seed7-intro-statement-keyword-face)


(defface seed7-predefined-variables-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))
    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))
    (((class color)
      (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-24"))
    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))
    (t (:weight bold)))
  "Font Lock mode face used to highlight predefined variable names."
  :group 'seed7-faces)
(defvar seed7-predefined-variables-face 'seed7-predefined-variables-face)


(defface seed7-errinfo-value-face
  `(;; (((class grayscale) (background light))
    ;;  (:background "Gray90" :weight bold))

    ;; (((class grayscale) (background dark))
    ;;  (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     ;; (:foreground "Blue" :background "lightyellow2" :weight bold)
     (:foreground "color-133" :weight bold))

    ;; (((class color) (background dark))
    ;;  (:foreground "yellow" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
  :group 'seed7-faces)
(defvar seed7-errinfo-value-face 'seed7-errinfo-value-face)


;;* Seed7 Font Locking Control
;;  ==========================
;;

(defconst seed7-font-lock-keywords
  (list
   ;; pragmas
   (cons seed7-pragma-keywords-regexp                (list 1 seed7-pragma-keyword-face))
   ;; include
   (cons seed7-include-regexp                        (list 1 seed7-include-face))

   ;; in-statement keywords
   (cons seed7-lead-in-statement-keywords-regexp     (list 1 seed7-in-statement-keyword-face))
   (cons seed7-in-statement-keywords-regexp          (list 1 seed7-in-statement-keyword-face))
   ;; statement-introducing keywords (needed??probably not)
   (cons seed7-statement-introducing-keywords-regexp (list 1 seed7-statement-introducing-keyword-face))
   ;; keywords used in middle of statements
   (cons seed7-in-middle-statement-keywords-regexp   (list 1 seed7-in-middle-statement-keyword-face))
   ;; declaration introduction keywords :probably need a better name
   (cons seed7-declaration-intro-keywords-regexp     (list 1 seed7-intro-statement-keyword-face))
   ;; predefined types
   (cons seed7-predefined-types-regexp               (list 1 font-lock-type-face))
   ;; predefined constants
   (cons seed7-predefined-constants-regxp            (list 1 font-lock-constant-face))
   ;; predefined variables
   (cons seed7-predefined-variables-regxp            (list 1 seed7-predefined-variables-face))
   ;; predefined errinfo values
   (cons seed7-errinfo-values-regxp                  (list 1 seed7-errinfo-value-face))
   ;; operator symbols
   (cons seed7-operator-symbols-regexp               (list 1 font-lock-keyword-face))
   (cons seed7-assignment-operator-regxp             (list 0 font-lock-keyword-face))
   (cons seed7-other-operator-regexp                 (list 1 font-lock-keyword-face))
   (cons seed7-arithmetic-operator-regexp            (list 1 font-lock-keyword-face))
   (cons "[[:alnum:] _)]\\(/\\)[[:alnum:] _(]"       (list 1 font-lock-keyword-face)) ; /
   (cons "[[:alnum:] _)]\\(\\*\\*\\)[[:alnum:] _(]"  (list 1 font-lock-keyword-face)) ; **
   ;; logic operator
   (cons "[[:alnum:] _)\\\"]\\(&\\)[[:alnum:] _(\\\"]" (list 1 font-lock-keyword-face)) ; &
   (cons "[[:alnum:] _)\\\"]\\(|\\)[[:alnum:] _(\\\"]" (list 1 font-lock-keyword-face)) ; |
   )
  "Alist of Seed7 base keywords with each a specific face")


;;* Seed7 Comments Control
;;  ======================
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

;; ---------------------------------------------------------------------------
;;* Seed7 iMenu Support
;;  ===================
(defconst seed7-procedure-regexp
  "^[[:space:]]*const proc: \\([[:alpha:]][[:alnum:]_]+\\) .*is func")

(defconst seed7-function-regexp
  "^[[:space:]]*const func \\([[:alpha:]][[:alnum:]_]+\\) ?: *\\([[:alpha:]][[:alnum:]_]+\\) .*is\\( func\\)?")

;; The following regexp has the following groups:
;; Group 1: "proc" or "func "
;; Group 2: "proc" or "func "
;; Group 3: The func return type.  May be empty.
;; Group 4: The func or proc name.
;; Group 5: - "func" for proc or function that ends with "end func".
;;          - empty for a func that only has a return statement.
;;
(defconst seed7-procedure-or-function-regexp
  "^[[:space:]]*const \\(\\(func \\|proc\\)\\)\\([[:alpha:]][[:alnum:]_]+\\)? ?: *\\([[:alpha:]][[:alnum:]_]+\\) .*is\\( func\\)?")

(defconst seed7-enum-regexp
  "const type: \\([[:alpha:]][[:alnum:]_]+\\) is new enum")

(defconst seed7-interface-regexp
  "const type: \\([[:alpha:]][[:alnum:]_]+\\) is new interface")

(defconst seed7-struct-regexp
  "const type: \\([[:alpha:]][[:alnum:]_]+\\) is new struct")

;;* Seed7 Speedbar Support
;;  ======================

(speedbar-add-supported-extension "\\.s\\(d7\\|7i\\)\\'")

;; ---------------------------------------------------------------------------
;;* Seed7 Code Navigation
;;  =====================

(defun seed7-beg-of-defun (&optional n silent dont-push-mark)
  "Move backward to the beginning of the current function or procedure.
- With optional argument N, repeat the search that many times.
- Unless SILENT, the function prints a message showing the name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH_MARK is non-nil. Pushing the mark allows future pop to
  go back to the original position with C-u C-SPC
- Supports shift selection."
  (interactive "^p")
  (unless n (setq n 1))
  (let* ((original-pos (point))
         (final-pos    original-pos)
         (verbose nil))
    (save-excursion
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))
        (move-end-of-line 1)
        (if (re-search-backward seed7-procedure-or-function-regexp
                                nil :noerror)
            (if (eq original-pos (point))
                (progn
                  (right-char)
                  (if (re-search-backward seed7-procedure-or-function-regexp
                                          nil :noerror)
                      (progn
                        (setq final-pos (point))
                        (when verbose
                          (let ((item-name (substring-no-properties (match-string 4))))
                            (message "To beginning of: %s" item-name))))
                    (user-error "No other Seed function or procedure above.")))
              (progn
                (setq final-pos (point))
                (when verbose
                  (let ((item-name (substring-no-properties (match-string 4))))
                    (message "To beginning of: %s" item-name)))))
          (user-error "No Seed7 function or procedure found above."))
        (left-char)))
    (when (/= final-pos original-pos)
      (unless dont-push-mark
        (push-mark original-pos))
      (goto-char final-pos))))


(defun seed7-beg-of-next-defun (&optional n silent dont-push-mark)
  "Move forward to the beginning of the next function or procedure.
- With optional argument N, repeat the search that many times.
- Unless SILENT, the function prints a message showing the name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH_MARK is non-nil. Pushing the mark allows future pop to
  go back to the original position with C-u C-SPC
- Supports shift selection."
  (interactive "^")
  (unless n (setq n 1))
  (let* ((original-pos (point))
         (final-pos    original-pos)
         (verbose nil))
    (save-excursion
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))
        (right-char)
        (if (re-search-forward seed7-procedure-or-function-regexp
                               nil :noerror)
            (progn
              (move-beginning-of-line nil)
              (setq final-pos (point))
              (when verbose
                (let ((item-name (substring-no-properties (match-string 4))))
                  (message "To beginning of: %s" item-name))))
          (user-error "No Seed7 function or procedure found below!"))))
    (when (/= final-pos original-pos)
      (unless dont-push-mark
        (push-mark original-pos))
      (goto-char final-pos))))

(defun seed7--at-end-of-defun ()
  "Return t if point is at end of function or procedure, nil otherwise."
  (save-excursion
    (let ((is-at-end nil)
          (orig-end-of-line-pos (save-excursion
                                  (move-end-of-line 1)
                                  (point))))
      ;; if line is an 'end func;' we're at end of function
      (forward-line 0)
      (setq is-at-end
            (string-equal
             (string-trim (thing-at-point 'line :no-properties))
             "end func;"))
      (unless is-at-end
        ;; otherwise we may still be at end of a simple function with a return
        ;; check if we can get to the end and check if the position is the
        ;; same as what the end of line position was.
        (when (and (re-search-backward seed7-procedure-or-function-regexp
                                       nil :noerror)
                   (re-search-forward "[[:space:]]return[[:space:]]+.+;"
                                      nil :noerror))
          (when (eq (point) orig-end-of-line-pos)
            (setq is-at-end t))))
      is-at-end)))

(defun seed7-end-of-defun (&optional n silent dont-push-mark)
  "Move forward to the end of the current function or procedure.
- With optional argument N, repeat the search that many times.
- Unless SILENT, the function prints a message showing the name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH_MARK is non-nil. Pushing the mark allows future pop to
  go back to the original position with C-u C-SPC
- Supports shift selection."
  (interactive "^")
  (unless n (setq n 1))
  ;; First identify the type of declaration by searching for the beginning
  ;; of function or proc using the `seed7-procedure-or-function-regexp' regexp
  ;; which has 5 groups
  (let* ((original-pos (point))
         (final-pos    original-pos)
         (verbose nil))
    (save-excursion
      (when (seed7--at-end-of-defun)
        (seed7-beg-of-next-defun 1 :silent :dont-push-mark))
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))
        (forward-line 1)
        (if (or (re-search-backward seed7-procedure-or-function-regexp
                                    nil :noerror)
                (re-search-forward seed7-procedure-or-function-regexp
                                   nil :noerror))
            (let ((item-type (substring-no-properties (match-string 1)))
                  (item-name (substring-no-properties (match-string 4)))
                  (group5    (let ((matched (match-string 5)))
                                (when matched
                                  (substring-no-properties matched)))))
              (cond
               ;; Procedure
               ((string-equal item-type "proc")
                (if  (search-forward "end func;")
                    (progn
                      (setq final-pos (point))
                      (when verbose
                        (message "To end of: %s" item-name)))
                  (user-error "End of %s not found: is code valid?" item-name)))
               ;; Function
               ((string-equal item-type  "func ")
                (if (and group5
                         (string-equal group5 " func"))
                    ;; long func that ends with end func;
                    (if (search-forward "end func;")
                        (progn
                          (setq final-pos (point))
                          (when verbose
                            (message "To end of: %s" item-name)))
                      (user-error "End of %s not found: is code valid?"
                                  item-name))
                  ;; short func with simpler return
                  (if (re-search-forward "[[:space:]]return[[:space:]]+.+;"
                                         nil :noerror)
                      ;; [:todo 2025-04-07, by Pierre Rouleau: fix required:
                      ;; check that syntax at point is not comment or string
                      ;; at point to validate find. Do this only once the
                      ;; syntax support is complete and detects strings/comments
                      ;; properly. ]
                      (progn
                        (setq final-pos (point))
                        (when verbose
                          (message "To end of: %s" item-name)))
                    (user-error "Function not terminated properly!"))))
               ;; The next line should never occur, if it does report a bug
               ;; providing a code example to reproduce.
               (t (error "Not inside a procedure or function!"))))
          (user-error "No Seed7 end of function or procedure found below!"))))
    (when (/= final-pos original-pos)
      (unless dont-push-mark
        (push-mark original-pos))
      (goto-char final-pos))))

;; ---------------------------------------------------------------------------
;;* Seed7 Key Map
;;  =============
;;
;; [:todo 2025-04-07, by Pierre Rouleau: Find best user-option keys for Seed7 map]

(defvar seed7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-a"  'seed7-beg-of-defun)
    (define-key map "\M-\C-e"  'seed7-end-of-defun)
    map)
  "Keymap used in seed7-mode.")

;; ---------------------------------------------------------------------------

;;* Seed7 Major Mode
;;  ================

;;;###autoload
(define-derived-mode seed7-mode prog-mode "seed7"
  "Major mode for editing Seed7 files.
This is a preliminary implementation, based on `pascal-mode'"
  (seed7--set-comment-style seed7-uses-block-comment)
  (setq-local font-lock-defaults '((seed7-font-lock-keywords)))

  ;; iMenu Support / Speedbar Support
  (setq-local imenu-generic-expression
              (list
               (list "Enum"      seed7-enum-regexp 1)
               (list "Interface" seed7-interface-regexp 1)
               (list "Struct"    seed7-struct-regexp 1)
               (list "Procedure" seed7-procedure-regexp 1)
               (list "Function"  seed7-function-regexp  2)))

  ;; Code Navigation
  ;; Allow code familiar with the standard `beginning-of-defun' and
  ;; `end-of-defun' to work inside Seed7 buffers.  This includes iedit,
  ;; expand-region, etc...
  (setq-local beginning-of-defun-function 'seed7-beg-of-defun)
  (setq-local end-of-defun-function 'seed7-end-of-defun))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;;; seed7-mode.el ends here
