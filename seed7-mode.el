;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-05-26 15:01:39 EDT, updated by Pierre Rouleau>

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
;; This is currently an early/rough work-in-progress implementation for
;; Seed7 support.  It is not stable and may change.
;;
;; Code navigation:  the layout of this file supports Emacs outline mode.
;;  Use `outline-minor-mode' to hide all text except the section headings to
;;  get a quick overview of the structure of this code.

;; Feature wish-list:
;;
;; - Syntax and semantics support for Seed7 - done.
;;   - keyword colorization - done.
;; - Support comment:
;;   - comment syntax: colorization, creation, deletion - done.
;;   - hiding/showing comments: working but for 1 comment style only.
;; - Indentation control: first implementation done.
;;     - Supports automatic indentation of most code.
;;     - In some cases no rule is available but should: an error
;;       is raised and the code is not indented.
;;       These will be fixed later.
;; - Launch help on keywords, perhaps implement statement help

;; - Keyword Completion help.
;; - Navigation help - underway.
;; - Template help for code creation
;; - Commands to compile with error reporting - done.
;;
;; [:todo 2025-04-06, by Pierre Rouleau: Fix following problems:
;;  Known problems:
;;  # 01  Complete defface definitions:
;;        - Complete the dark backgrounds coloring.
;;        - Maybe add ability to reduce number of faces used (or re-use the
;;          same face for various elements).  It would allow dual use: one
;;          with lots of different renderings and another with not that many,
;;          a more conservative approach.
;;  # 02  The '#' used as base separator is not detected as a comment
;;        BUT a comment that follows a digit will not render as a comment
;;        unless a non alphanumeric character follows it.
;;        The 'seed7-mode-syntax-propertize' uses a simple regexp to prevent
;;        interpretation as comment: `seed7-base-x-number-re'.  A more complex
;;        one could be used, similar to what `seed7-base-x-big-number-re' does
;;        but by only capturing the '#'.  This however might be too processing
;;        expensive.  I will only try it once everything else is done.
;;  # 03  Escaped single and double quote in strings are now recognized.
;;        However a string continuation that ends with a backslash just before
;;        the terminating quote is not supported.
;;  # 04  seed7-predefined-constants-regxp is not perfect: it does not allow
;;        an operator just at right of it.
;;  # 05  A sequence of 3 consecutive single quote is not handled by the
;;        syntax highlighting.
;;  # 06  Several code sequences are still not handled properly by the tab
;;        indentation control.  If this bothers you, disable it by setting
;;        `seed7-auto-indent' to nil.
;; ]
;;
;;
;;* Table of Content
;;  ----------------
;;
;; Code Organization Layout (use these as markers to locate related code)
;;
;; - Seed7 Customization
;; - Seed7 Mode Syntax Control
;;   - Seed7 Mode Syntax Table
;;   - Seed7 Mode Syntax Propertize Function
;;     - `seed7-mode-syntax-propertize'
;; - Seed7 Keywords
;;    - Seed7 Tokens
;;      - Seed7 Comments Control
;;      - Seed7 Float Literals
;;      - Seed7 Numbers with Exponents
;;      - Seed7 BigInteger Literals
;;      - Seed7 Integer Literals
;;    - Seed7 Pragmas
;;    - Seed7 include
;;    - Seed7 keywords used in statements
;;    - Seed7 is-statemement keywords
;;    - Seed7 keywords used in middle of statements
;;    - Seed7 statement enclosing keywords
;;    - Seed7 declaration introduction keywords
;;    - Seed7 Predefined Types
;;    - Seed7 Predefined Constants
;;    - Seed7 Predefined Variables
;;    - Seed7 Predefined errinfo value
;;    - Seed7 Operator Symbols
;;    - Seed7 Predefined Assignment Operators
;;    - Seed7 Predefined Comparison Operators
;;    - Seed7 Other Predefined Operators
;;      - Seed7 Arithmetic Operators
;;      - Seed Other operators
;;   - Seed7 Mode Syntax Propertize Function
;; - Seed7 Faces
;;   - `seed7-choose-color'
;; - Seed7 Font Locking Control
;; - Seed7 Comments Control
;;   * `seed7-toggle-comment-style'
;;     - `seed7--new-state-for'
;;     - `seed7--set-comment-style'
;; - Seed7 iMenu Support
;; - Seed7 Speedbar Support
;; - Seed7 Code Navigation
;;   * `seed7-beg-of-defun'
;;   * `seed7-beg-of-next-defun'
;;   * `seed7-end-of-defun'
;;     - `seed7--at-end-of-defun'
;;     + `seed7--move-and-mark'
;;     + `seed7--pos-msg'
;;   - Seed7 Navigation by Block
;;   * `seed7-to-block-forward'
;;   * `seed7-to-block-backward'
;;     - `seed7--current-line-nth-word'
;; - Seed7 Code Marking
;;   * `seed7-mark-defun'
;; - Seed7 Compilation
;;   * `seed7-compile'
;; - Seed7 Key Map
;; - Seed7 Menu
;; - Seed7 Major Mode
;;   * `seed7-mode'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'simple)         ; use `move-beginning-of-line'
(require 'speedbar)       ; use `speedbar-add-supported-extension'
(require 'subr-x)         ; use: `string-trim'
(require 'easymenu)       ; use: `easy-menu-define'
;;; --------------------------------------------------------------------------
;;; Code:
;;
;; References:
;; - Seed7 formal specification:  "Seed7 Reference Manual".
;; -   See http://seed7.sourceforge.net/

;; Seed7 Syntax Information:
;; - https://thomasmertes.github.io/Seed7Home/faq.htm#add_syntax_highlighting

;; ---------------------------------------------------------------------------
;;* Seed7 Customization
;;  ===================

(defgroup seed7 nil
  "Seed7 Programming Language support configuration."
  :group 'languages
  :link '(url-link  :tag "PEL @ GitHub"
                    "https://github.com/pierre-rouleau/seed7-mode")
  :package-version '(seed7-mode . "0.0.1"))

;;** Seed7 Comments Control
(defcustom seed7-uses-block-comment nil
  "When non-nil, use Seed7 \"(*   *)\" block comments.
Use line comments otherwise."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;**Seed7 Code Navigation
(defcustom seed7-verbose-navigation t
  "Seed7 navigation command print message on success when t.
If you do not want these navigation success message printed set this to
nil.  Setting it to nil does not prevent user error messages to show up
when the navigation command fails."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Compilation
(defcustom seed7-checker "s7check"
  "Seed7 source code check command line.

The command line must identify the Seed7 static check tool, s7check,
by default.
You may:
- Use the program name without a path if it is in the PATH of your shell.
- The name with an absolute path.
-  Compiler options after the program name if necessary.

The name of the source code file is appended to the end of that line.
Note that the s7check is part of the example programs located inside
the Seed7 prg directory.  Compile the prg/s7check.sd7 with s7c to create
the executable you can use for this."
  :group 'seed7
  :type 'string)

(defcustom seed7-compiler "s7c"
  "Seed7 compiler command line.

The command line must identify the Seed7 compiler, s7c, by default.
You may:
- Use the program name without a path if it is in the PATH of your shell.
- The name with an absolute path.
-  Compiler options after the program name if necessary.

The name of the source code file is appended to the end of that line."
  :group 'seed7
  :type 'string)


;;** Seed7 Faces
(defgroup seed7-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'seed7)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;* Seed7 Mode Syntax Control
;;  =========================
;;
;;** Seed7 Mode Syntax Table
;;   -----------------------
;;
;; Ref: Comments:      https://seed7.sourceforge.net/manual/tokens.htm#Comments
;; Ref: Line comments: https://seed7.sourceforge.net/manual/tokens.htm#Line_comments
;;
;; Comments: (* This is a comment *)
;;           (* This is a
;;              multi-line comment *)
;;           # this is a line comment
;;
;;  Note that '#' is also used as a number base separator.
;;  For the moment the mode requires '##' as the line comment starter.
;;  So it's not always a comment.  For now require a space after '#' to consider it a comment.

(defvar seed7-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "."   st)
    ;;
    (modify-syntax-entry ?\( "()1n" st) ; The comment "(*" can be nested ...
    (modify-syntax-entry ?\) ")(4n" st) ; ...  and end with the matching "*)"
    (modify-syntax-entry ?* ". 23" st) ; '*' as second of "(*" and previous of; "*)"
    ;;
    ;; Seed7 Comments Control : line-end comment.
    (modify-syntax-entry ?# "<"  st)
    (modify-syntax-entry ?\n ">" st)
    ;;
    ;; string escape
    (modify-syntax-entry ?\\ "\\"  st)
    (modify-syntax-entry ?\' "\""  st)
    st)
  "Syntax table in use in seed7-mode buffers.")

;;** Seed7 Mode Syntax Propertize Function
;;   -------------------------------------

(defun seed7-mode-syntax-propertize (start end)
  "Apply syntax property between START and END to # character in number."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; Prevent the # in base numbers to be interpreted as comment.
    ;; Use "_" (word) syntax so `forward-sexp' does not stop at the '#'
    ;; in numbers with a base.
    ("[[:digit:]]\\(#\\)[[:alnum:]]" (1 (string-to-syntax "_"))))
   start end))


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

;;* Seed7 Tokens
;;  ------------
;;
;; Ref: https://seed7.sourceforge.net/manual/tokens.htm

;; [:todo 2025-04-09, by Pierre Rouleau: Complete the syntax for floating point numbers.]

;;** Seed7 Comments Control
(defconst seed7--line-comment-regexp
  "[^[:digit:]]\\(#.*\\)$"
  "Single line comment in group 1.")

(defconst seed7--whitespace-re
  "[[:space:]
]"
  "Match any horizontal whitespace character and new line.")

(defconst seed7--anychar-re
  "[^\\0]"
  "Match any character including new-line.")

(defconst seed7--bracket-re
  "[])(}{[]")

(defconst seed7-identifier-regexp
  "\\(\\_<[[:alpha:]_][[:alnum:]_]*\\_>\\)"
  "A complete, valid name identifier.")

(defconst seed7--special-char-re
  "[-!$%&*+,\\./:;<=>?@\\^`|~]"
  "Any one of the special characters.")

(defconst seed7--number-separator-re
  "[]!$%&*+,\\./:;<=>?@\\^`|~ )(}{ -[]")

(defconst seed7-integer-re (format
                            "%s\\(-?[[:digit:]]+\\)%s"
                            seed7--number-separator-re
                            seed7--number-separator-re)
  "Seed7 integer in group 1.")

;; ** Seed7 Float Literals
(defconst seed7-float-number-re
  "[0-9]+\\.[0-9]+\\(?:\\(?:[eE][+-]?\\)?[0-9]+\\)?"
  "Seed7 float number in group 0.")

;; ** Seed7 Numbers with Exponents

(defconst seed7-number-with-exponent-re
  "[0-9]+[eE][+-]?[0-9]+"
  "Literal number with exponent.  Does not reject integer with negative exponent.")


;;** Seed7 BigInteger Literals
;;
;; Ref: https://seed7.sourceforge.net/manual/tokens.htm#BigInteger_literals
;;
(defconst seed7--big-number-re-format
  "%s\\(\\(?:\\(?:\\([2-9]\\|1[0-9]\\|2[0-9]\\|3[0-6]\\)#\\)?[0-9]+_\\)\\)%s"
  ;;   1            2
  ;; Group 1: Complete Big Number with or without base. "1_" or "1234322_" or "2#0001_", etc...
  ;; Group 2: base: "2" to "36".  nil if no base.
  "Big number with/without base.  Group 1: number, group 2: base or nil.")

(defconst seed7-big-number-re (format
                               seed7--big-number-re-format
                               seed7--number-separator-re
                               "\\_>"))

;;** Seed7 Integer Literals
;;
;; Ref: https://seed7.sourceforge.net/manual/tokens.htm#Integer_literals
;;
(defconst seed7--base-x-integer-re-format
  "\\%s\
\\(?:2#[01]+\\)\\|\
\\(?:3#[0-2]+\\)\\|\
\\(?:4#[0-3]+\\)\\|\
\\(?:5#[0-4]+\\)\\|\
\\(?:6#[0-5]+\\)\\|\
\\(?:7#[0-6]+\\)\\|\
\\(?:8#[0-7]+\\)\\|\
\\(?:9#[0-8]+\\)\\|\
\\(?:10#[0-9]+\\)\\|\
\\(?:11#[0-9aA]+\\)\\|\
\\(?:12#[0-9aAbB]+\\)\\|\
\\(?:13#[0-9a-cA-C]+\\)\\|\
\\(?:14#[0-9a-dA-D]+\\)\\|\
\\(?:15#[0-9a-eA-E]+\\)\\|\
\\(?:16#[0-9a-fA-F]+\\)\\|\
\\(?:17#[0-9a-gA-G]+\\)\\|\
\\(?:18#[0-9a-hA-H]+\\)\\|\
\\(?:19#[0-9a-iA-I]+\\)\\|\
\\(?:20#[0-9a-jA-J]+\\)\\|\
\\(?:21#[0-9a-kA-K]+\\)\\|\
\\(?:22#[0-9a-lA-L]+\\)\\|\
\\(?:23#[0-9a-mA-M]+\\)\\|\
\\(?:24#[0-9a-nA-N]+\\)\\|\
\\(?:25#[0-9a-oA-O]+\\)\\|\
\\(?:26#[0-9a-pA-P]+\\)\\|\
\\(?:27#[0-9a-qA-Q]+\\)\\|\
\\(?:28#[0-9a-rA-R]+\\)\\|\
\\(?:29#[0-9a-sA-S]+\\)\\|\
\\(?:30#[0-9a-tA-T]+\\)\\|\
\\(?:31#[0-9a-uA-U]+\\)\\|\
\\(?:32#[0-9a-vA-V]+\\)\\|\
\\(?:33#[0-9a-wA-W]+\\)\\|\
\\(?:34#[0-9a-xA-X]+\\)\\|\
\\(?:35#[0-9a-yA-Y]+\\)\\|\
\\(?:36#[0-9a-zA-Z]+\\)\\)%s")

(defconst seed7-base-x-integer-re (format seed7--base-x-integer-re-format
                                          "("
                                          "[^#0-9a-zA-z]"))

(defconst seed7-base-x-big-number-re (format seed7--base-x-integer-re-format
                                             "(\\(?:"
                                             "_\\)[^#0-9a-zA-z]"))

;;* Seed7 Pragmas
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
    "names"

    ;; The next ones are not identified specifically as pragmas
    ;; but they are special and are also used with a leading '$'
    "syntax"
    "system"
    ))

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
  "^\\(\\$? *\\(?:include\\)\\) ")


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
  '(
    "raise"                      ; currently missing in the Seed7 keyword list
    "return"
    ))

(defconst seed7-lead-in-statement-keywords-regexp
  (format "^ *%s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--lead-in-statement-keywords)))
          "\\_>"))

(defconst seed7--in-statement-keywords
  '("is"
    "noop" ; not mentioned in operators but not an identifier, probably a special case
    ))

(defconst seed7-in-statement-keywords-regexp
  (format ". %s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--in-statement-keywords)))
          "\\_>"))


;;* Seed7 is-statemement keywords
;;  -----------------------------
;;
;; These keywords are exclusively used following the 'is' keyword.
;;
(defconst seed7-is-statement-keywords
  '(
    "forward"
    "new"
    ))

(defconst seed7--is-statement-keywords-regexp
  (format " is%s+\\(%s\\)\\_>"
          seed7--whitespace-re
          (rx-to-string
           `(: (or ,@seed7-is-statement-keywords)))))

;;* Seed7 keywords used in middle of statements
;;  -------------------------------------------

(defconst seed7--in-middle-statement-keywords
  '("begin"
    "do"
    "downto"
    "exception"
    "local"
    "param"
    "range"
    "result"
    "step"
    "then"
    "to"
    ))

(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:space:]]"
          (rx-to-string
           `(: (or ,@seed7--in-middle-statement-keywords)))
          "\\_>"))


;;* Seed7 statement enclosing keywords
;;  ----------------------------------
;;

(defconst seed7--block-start-keywords
  '("block"      ; "end block;"
    "case"       ; "end case;"
    "enum"
    "for"
    ;; "func"
    "if"       ; "elsif"      "end if;"
    ;; "repeat"      "until"
    "struct"
    "while"))


(defconst seed7--statement-enclosing-keywords
  '("block"       "end block"
    "case"         "when" "otherwise" "end case"
    "of"                                ; in 'case' or 'is set'
    "exception"   "catch"               ; otherwise
    "enum"        "end enum"
    "for"         "end for"
    "func"        "end func"
    "if"          "else" "elsif" "end if"
    "repeat"      "until"
    "struct"      "end struct"
    "while"       "end while"))

(defconst seed7-statement-enclosing-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\_<"
          (rx-to-string
           `(: (or ,@seed7--statement-enclosing-keywords)))
          "\\_>"))

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
    "Infinity"
    "NIL"
    "NaN"
    "PI"
    "STD_NULL"
    "TRUE"
    "empty"))

(defconst seed7-predefined-constants-regxp
  (format "%s\\(%s\\)%s"
          ""
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

;; [:todo 2025-04-10, by Pierre Rouleau: categorize 'noop' according to Seed7 spec once I find it ]
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

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;* Seed7 Predefined Assignment Operators
;;  -------------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Predefined assignment operators are: := +:= -:= *:= /:= <<:= >>:= &:= |:= ><:= @:=
;;
;;    :=
;;   +:=        https://seed7.sourceforge.net/libraries/integer.htm#(inout_integer)+:=(in_integer)
;;   -:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)-:=(in_bigInteger)
;;   *:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)*:=(in_bigInteger)
;;   /:=        https://seed7.sourceforge.net/libraries/bigrat.htm#(inout_bigRational)/:=(in_bigRational)
;;  <<:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3C%3C:=(in_integer)
;;  >>:=        https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3E%3E:=(in_integer)
;;   &:=        https://seed7.sourceforge.net/libraries/array.htm#(inout_arrayType)&:=(in_arrayType)
;;   |:=        https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)|:=(in_bin32)
;;  ><:=        https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)%3E%3C:=(in_bin32)
;;   @:=        https://seed7.sourceforge.net/libraries/bitset.htm#(inout_bitset)@:=_[(in_integer)](in_boolean)

(defconst seed7-predef-assignment-operator-regxp
  "\\(?:\\(?:[+*/&|@-]\\)\\|\\(?:<<\\|>>\\|><\\)\\)?:="
  "Symbol is in group 0.")

;;* Seed7 Predefined Comparison Operators
;;  -------------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Predefined comparison operators are: = <> < <= > >=

(defconst seed7-predef-comparison-operator-regxp
  "\\(?:[=><]\\|\\(?:<>\\|<=\\|>=\\)\\)"
  "Symbol is in group 0.")


;;* Seed7 Other Predefined Operators
;;  --------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Other predefined operators are: + - * / ** ! << >> & | >< <& ?
;;                                            -------     -------
(defconst seed7-other-predef-operator-regxp
  "[!?]\\|<<\\|>>\\|><\\|<&"
  "Symbol is in group 0.")


;;** Seed7 Arithmetic Operators
;;   --------------------------
;;
;;   + - * / **
;;
(defconst seed7-arithmetic-operator-regxp
  "[[:alnum:]_ )]\\([/*]\\)[[:alnum:]_ (]"
  "Arithmetic operator except the minus sign.")

(defconst seed7-minus-operator-regexp
  "[^+-]\\([+-]\\)[^+-]"
  "Arithmetic minus operator in group 1.")


;; ---------------------------------------------------------------------------
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
;; `seed7-in-statement-keyword-face1'    Seed7 keywords used inside statements: raise, return
;; `seed7-in-statement-keyword-face2'    Seed7 keywords used inside statements: do, is, noop, then
;; ===================================== ================================================

(defun seed7-choose-color (&rest list)
  "Use the first colour available from the specified LIST of color names.

Allows selecting similar colours for various systems."
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

;;** Seed7 Faces Customization

;; --
(defface seed7-pragma-keyword-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SlateBlue2" :background "lightYellow1" :weight bold))
    (((class color) (background dark))
     (:foreground "SlateBlue2" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights pragma keywords."
  :group 'seed7-faces)

(defface seed7-include-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SlateBlue3" :background "lightYellow1" :weight bold))
    (((class color) (background dark))
     (:foreground "SlateBlue3" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights include."
  :group 'seed7-faces)

;; --
(defface seed7-in-statement-keyword-face1
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "DodgerBlue3" :weight bold))
    (((class color) (background dark))
     (:foreground "DodgerBlue3" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights keywords."
  :group 'seed7-faces)

(defface seed7-in-statement-keyword-face2
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SteelBlue3" :weight bold))
    (((class color) (background dark))
     (:foreground "SteelBlue3" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights keywords."
  :group 'seed7-faces)

;; --
(defface seed7-statement-introducing-keyword-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "DeepSkyBlue2" :weight bold))
    (((class color) (background dark))
     (:foreground "DeepSkyBlue2" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights keywords that introduce a statement."
  :group 'seed7-faces)

;; --
(defface seed7-in-middle-statement-keyword-face1
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "RoyalBlue2" :weight bold))
    (((class color) (background dark))
     (:foreground "RoyalBlue2" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights keywords used in middle of statements."
  :group 'seed7-faces)

(defface seed7-in-middle-statement-keyword-face2
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "RoyalBlue1" :weight bold))
    (((class color) (background dark))
     (:foreground "RoyalBlue1" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights keywords used in middle of statements."
  :group 'seed7-faces)

;; --
(defface seed7-intro-statement-keyword-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "blue1" :weight bold))
    (((class color) (background dark))
     (:foreground "blue1" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights statement intro keywords."
  :group 'seed7-faces)

;; --
(defface seed7-predefined-variables-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "dark cyan"))
    (((class color) (background dark))
     (:foreground "dark cyan" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights predefined variable names."
  :group 'seed7-faces)

;; --
(defface seed7-errinfo-value-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "cadet blue" :weight bold))
    (((class color) (background dark))
     (:foreground "cadet blue" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights errinfo values."
  :group 'seed7-faces)

;; --
(defface seed7-float-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "Chocolate3" ))
    (((class color) (background dark))
     (:foreground "Chocolate3" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights errinfo values."
  :group 'seed7-faces)

(defface seed7-integer-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "peru" ))
    (((class color) (background dark))
     (:foreground "peru" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights errinfo values."
  :group 'seed7-faces)

(defface seed7-number-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "sienna" ))
    (((class color) (background dark))
     (:foreground "sienna" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights errinfo values."
  :group 'seed7-faces)

;; --
(defface seed7-identifier-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "black"  :weight bold))
    (((class color) (background dark))
     (:foreground "black" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights errinfo values."
  :group 'seed7-faces)

;;* Seed7 Font Locking Control
;;  ==========================
;;
;; Note the double single quotes in the dynamically created lists below.
;; According to Stephan Monnier, these are necessary because of a misfeature
;; of font-lock-keywords: they're data that contains quoted
;; code, so you need a first quote to quote the face, and a second quote to
;; quote the resulting code.
;;
(defconst seed7-font-lock-keywords
  (list
   ;; pragmas
   (cons seed7-pragma-keywords-regexp                (list 1 ''seed7-pragma-keyword-face))
   ;; include
   (cons seed7-include-regexp                        (list 1 ''seed7-include-face))
   ;; in-statement keywords
   (cons seed7-lead-in-statement-keywords-regexp     (list 1 ''seed7-in-statement-keyword-face1))
   (cons seed7-in-statement-keywords-regexp          (list 1 ''seed7-in-statement-keyword-face2))
   ;; statement-introducing keywords
   (cons seed7-statement-enclosing-keywords-regexp   (list 1 ''seed7-statement-introducing-keyword-face))
   ;; keywords used in middle of statements
   (cons seed7--is-statement-keywords-regexp         (list 1 ''seed7-in-middle-statement-keyword-face1))
   (cons seed7-in-middle-statement-keywords-regexp   (list 1 ''seed7-in-middle-statement-keyword-face2))
   ;; declaration introduction keywords :probably need a better name
   (cons seed7-declaration-intro-keywords-regexp     (list 1 ''seed7-intro-statement-keyword-face))
   ;; predefined types
   (cons seed7-predefined-types-regexp               (list 1 ''font-lock-type-face))
   ;; predefined constants
   (cons seed7-predefined-constants-regxp            (list 1 ''font-lock-constant-face))
   ;; predefined variables
   (cons seed7-predefined-variables-regxp            (list 1 ''seed7-predefined-variables-face))
   ;; predefined errinfo values
   (cons seed7-errinfo-values-regxp                  (list 1 ''seed7-errinfo-value-face))
   ;; operator symbols
   (cons seed7-operator-symbols-regexp               (list 1 ''font-lock-keyword-face))
   (cons seed7-predef-assignment-operator-regxp      (list 0 ''font-lock-keyword-face))
   (cons seed7-other-predef-operator-regxp           (list 0 ''font-lock-keyword-face)) ; before comparison because that has single char operators that are part of other predef
   (cons seed7-predef-comparison-operator-regxp      (list 0 ''font-lock-keyword-face))
   (cons seed7-arithmetic-operator-regxp             (list 1 ''font-lock-keyword-face))
   (cons "[[:alnum:] _)]\\(/\\)[[:alnum:] _(]"       (list 1 ''font-lock-keyword-face)) ; /
   (cons "[[:alnum:] _)]\\(\\*\\*\\)[[:alnum:] _(]"  (list 1 ''font-lock-keyword-face)) ; **
   ;; logic operator
   (cons "[[:alnum:] _)\\\"]\\(&\\)[[:alnum:] _(\\\"]" (list 1 ''font-lock-keyword-face)) ; &
   (cons "[[:alnum:] _)\\\"]\\(|\\)[[:alnum:] _(\\\"]" (list 1 ''font-lock-keyword-face)) ; |
   ;; numbers: order is significant
   (cons seed7-base-x-big-number-re                  (list 1 ''seed7-number-face))
   (cons seed7-base-x-integer-re                     (list 1 ''seed7-integer-face))
   ;; identifiers
   (cons seed7-identifier-regexp                     (list 1 ''seed7-identifier-face))
   ;; other numbers
   (cons seed7-float-number-re                       (list 0 ''seed7-float-face))
   (cons seed7-number-with-exponent-re               (list 0 ''seed7-integer-face))
   (cons seed7-big-number-re                         (list 1 ''seed7-number-face))
   (cons seed7-integer-re                            (list 1 ''seed7-integer-face))
   ;; low priority rendering of arithmetic + and -
   (cons seed7-minus-operator-regexp                 (list 1 ''font-lock-keyword-face))
   )
  "Associates regexp to a regexp group and a face to render it.")


;;* Seed7 Comments Control
;;  ======================
;;
;; - Region:      "(\*" "\*)"
;; - To line end: "#"

(defconst seed7-block-comment-starter "(*")
(defconst seed7-block-comment-ender   "*)")
(defconst seed7-block-comment-prefix  "**")
(defconst seed7-line-comment-starter  "#")

(defun seed7--new-state-for (arg prevstate)
  "Calculate the new state of PREVSTATE, t or nil, based on ARG.

- If ARG is nil or zero, toggle the state,
- If ARG is negative, turn the state off,
- If ARG is positive, turn the state on."
  (if (or (not arg)
	      (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun seed7--set-comment-style (use-block &optional verbose)
  "Set Seed7 buffer comment style to block style when USE-BLOCK is non nil.
Set it to line-style otherwise.  Only affect current buffer.
Print message when VERBOSE is non-nil.
Note: the default style for all Seed7 buffers is controlled by the
`seed7-uses-block-comment' customizable user-option."
  (setq-local seed7-uses-block-comment use-block)
  (setq-local comment-start
	          (concat (if seed7-uses-block-comment
		                  seed7-block-comment-starter
		                seed7-line-comment-starter)
		              " "))
  (setq-local comment-end
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
just toggles it when zero or left out.

Note: the default style for all Seed7 buffers is controlled by the
`seed7-uses-block-comment' customizable user-option."
  (interactive "P")
  (let ((use-block (cond
	                ((and seed7-line-comment-starter seed7-block-comment-starter)
	                 (seed7--new-state-for arg seed7-uses-block-comment))
	                (seed7-line-comment-starter nil)
	                (t t))))
    (seed7--set-comment-style use-block 'verbose)))

;; ---------------------------------------------------------------------------
;;* Seed7 iMenu Support
;;  ===================
(defconst seed7-procedure-regexp
  (format
   "^[[:space:]]*const%s+proc:%s\\([[:alpha:]][[:alnum:]_]+\\)%s*?is func"
   ;;                             G1
   seed7--whitespace-re
   seed7--whitespace-re
   seed7--anychar-re)
  "Match procedure name in group 1.")

(defconst seed7-function-regexp
  (format
   "^[[:space:]]*const%s+func \\([[:alpha:]][[:alnum:]_]+\\)%s?:%s*\\([[:alpha:]][[:alnum:]_]+\\)%s*?is%s+\\(func\\|return\\)"
  ;;                  %         G1                          %   %  G2
   seed7--whitespace-re
   seed7--whitespace-re
   seed7--whitespace-re
   seed7--anychar-re
   seed7--whitespace-re)
  "Match function name in group 2, returned type in group 1.")

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

;; [:todo 2025-04-09, by Pierre Rouleau: The following regexp does not work 100%, fix it]
;; The following regexp has the following groups:
;; Group 1: "proc" or "func "
;; Group 2: "proc" or "func "
;; Group 3: The func return type.  May be empty.
;; Group 4: The func or proc name.
;; Group 5: - "func" for proc or function that ends with "end func".
;;          - "return" for a func that only has a return statement.
;;
(defconst seed7-procedure-or-function-regexp
  (format
   "^[[:space:]]*const%s+\\(\\(func \\|proc\\)\\)\\([[:alpha:]][[:alnum:]_]+\\)?%s?:%s*\\([[:alpha:]][[:alnum:]_]+\\)%s*?is%s+\\(func\\|return\\|forward;\\)"
   ;;                 %    G1 G2                   G3                           %   %    G4                          %     %    G5
   seed7--whitespace-re
   seed7--whitespace-re
   seed7--whitespace-re
   seed7--anychar-re
   seed7--whitespace-re))

;; future?
;; "^[[:blank:]]*const \\(\\(func\\|proc\\)\\)[[:space:]]?\\(\\([[:alpha:]][[:alnum:]_]+\\)?[[:space:]]+\\([[:alpha:]][[:alnum:]_]+\\)\\) ?: *\\([[:alpha:]][[:alnum:]_]+\\).*is[[:space:]]+\\(func\\)?")
;;                      G1 G2                              G3 G4                                         G5                                    G6                                            G7

(defun seed7--move-and-mark (original-pos final-pos dont-push-mark info)
  "Move point if necessary, push mark if necessary, print info if any.

- ORIGINAL-POS and FINAL-POS are the original and final position
  of the operations.
- DONT-PUSH-MARK a flag indicating whether mark should be pushed.
- INFO a string to issue as message if non nil."
  (when (/= final-pos original-pos)
    (unless dont-push-mark
      (push-mark original-pos (not seed7-verbose-navigation))
      (when info
        (setq info (concat info ", mark set."))))
    (goto-char final-pos)
    (when (and info
               seed7-verbose-navigation)
      (message info))))

(defun seed7--pos-msg (position name)
  "Return formatted message for start/end of NAME depending on POSITION."
  (if (eq position 'at-start-of)
      (format "@ start of: '%s'" name)
    (format "@ end of  : '%s'" name)))

(defun seed7-beg-of-defun (&optional n silent dont-push-mark)
  "Move backward to the beginning of the current function or procedure.

- With optional argument N, repeat the search that many times.
- Unless SILENT, the function prints a message showing the name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
  go back to the original position with \\[universal-argument] \\[set-mark-command].
- Supports shift selection."
  (interactive "^p")
  (unless n (setq n 1))
  (let* ((original-pos (point))
         (final-pos original-pos)
         (verbose nil))           ; first used as a flag, then message content
    (save-excursion
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))
        (move-end-of-line 1)
        (if (re-search-backward seed7-procedure-or-function-regexp
                                nil :noerror)
            (if (eq original-pos (point))
                (progn
                  (forward-char)
                  (if (re-search-backward seed7-procedure-or-function-regexp
                                          nil :noerror)
                      (progn
                        (setq final-pos (point))
                        (when verbose
                          (let ((item-name (substring-no-properties (match-string 4))))
                            (setq verbose (seed7--pos-msg 'at-start-of item-name)))))
                    (user-error "No other Seed function or procedure above!")))
              (progn
                (setq final-pos (point))
                (when verbose
                  (let ((item-name (substring-no-properties (match-string 4))))
                    (setq verbose (seed7--pos-msg 'at-start-of item-name))))))
          (user-error "No Seed7 function or procedure found above!"))
        (backward-char)))
    (seed7--move-and-mark original-pos final-pos dont-push-mark verbose)))

(defun seed7--beg-of-defun-simple (&optional n)
  "Simple beginning of defun to use as `beginning-of-defun-function'.

Move once, unless N specifies a different count.
Operate silently; do not issue an error when nothing is found.
Return t if point moved to the beginning of function, nil if nothing found."
  (interactive "^p")
  (condition-case nil
      (progn
        (seed7-beg-of-defun n :silent)
        t)
    (error nil)))

(defun seed7-beg-of-next-defun (&optional n silent dont-push-mark)
  "Move forward to the beginning of the next function or procedure.

With optional argument N, repeat the search that many times.

Unless SILENT, the function prints a message showing the name of the new
found function or procedure.

When a new function or procedure is found the function pushes the mark
unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
go back to the original position with \\[universal-argument] \\[set-mark-command].

Supports shift selection."
  (interactive "^")
  (unless n (setq n 1))
  (let* ((original-pos (point))
         (final-pos    original-pos)
         (verbose nil))           ; first used as a flag, then message content
    (save-excursion
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))
        (forward-char)
        (if (re-search-forward seed7-procedure-or-function-regexp
                               nil :noerror)
            (progn
              ;; Point is now at end of function definition.
              ;; Move to the beginning of the function definition.
              (forward-char)
              (re-search-backward seed7-procedure-or-function-regexp
                                  nil :noerror)
              (setq final-pos (point))
              (when verbose
                (let ((item-name (substring-no-properties (match-string 4))))
                  (setq verbose (seed7--pos-msg 'at-start-of item-name)))))
          (user-error "No Seed7 function or procedure found below!"))))
    (seed7--move-and-mark original-pos final-pos dont-push-mark verbose)))

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

(defun seed7--defun-name (&optional pos)
  "Extract and return name of current function or procedure.
Search backward from POS or point.
Return the string or nil if cannot identify it.
Do not move point."
  (save-excursion
    (when pos
      (goto-char pos))
    (when (re-search-backward seed7-procedure-or-function-regexp
                              nil :noerror)
      (substring-no-properties (match-string 4)))))

(defun seed7-end-of-defun (&optional n silent dont-push-mark)
  "Move forward to the end of the current function or procedure.
- With optional argument N, repeat the search that many times.
- Unless SILENT, the function prints a message showing the name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
  go back to the original position with \\[universal-argument] \\[set-mark-command].
- Supports shift selection."
  (interactive "^")
  (unless n (setq n 1))
  ;; First identify the type of declaration by searching for the beginning
  ;; of function or proc using the `seed7-procedure-or-function-regexp' regexp
  ;; which has 5 groups
  (let* ((original-pos (point))
         (final-pos    original-pos)
         (verbose nil)            ; first used as a flag, then message content
         (current-pos nil)
         (is-func-pos nil)
         (is-forward-decl-pos nil)
         (end-short-func-pos nil)
         (end-long-func-pos nil))
    (save-excursion
      (when (seed7--at-end-of-defun)
        (seed7-beg-of-next-defun 1 :silent :dont-push-mark))
      (dotimes (vn n)
        (setq verbose (and (not silent)
                           (eq vn (1- n))))

        ;; Point should be at the beginning of a function or procedure here.
        ;; - Attempt to move past the 'is func' portion of the function or
        ;;   procedure header part, so we can search for the whole beginning to
        ;;   go back to the top to retrieve the function/procedure name.
        ;;   - Make sure we don't find the 'is func' in the next function
        ;;     by checking that 'is func' is *before* 'end func;'`
        ;;   - Also make sure that we're not at a short function that only
        ;;     has a return statement.
        (setq current-pos (point))
        (save-excursion
          (setq is-func-pos
                (re-search-forward "is[[:space:]]+func" nil :noerror)))
        (save-excursion
          (setq end-short-func-pos
                (re-search-forward "^[[:space:]]+return\\(.\\|[^\\0]\\)+?;"
                                   nil :noerror)))
        (save-excursion
          (setq is-forward-decl-pos
                (re-search-forward ")[[:blank:]]*?is[[:blank:]]+?forward;")))
        (save-excursion
          (setq end-long-func-pos
                (re-search-forward "end[[:space:]]+func;" nil :noerror)))
        (if (and is-forward-decl-pos
                 (< is-forward-decl-pos
                    (or end-long-func-pos 0)))
            ;; this is a forward declaration the end is is-forward-decl-pos
            (progn
              (setq final-pos is-forward-decl-pos)
              (when verbose
                (setq verbose
                      (seed7--pos-msg 'at-end-of
                                      (or (seed7--defun-name final-pos)
                                          "forward declaration of ?")))))
          (if (< (or end-short-func-pos 0)
                 (or is-func-pos 0)
                 (or end-long-func-pos 0))
              (progn
                (setq final-pos end-short-func-pos)
                (when verbose
                  (setq verbose (seed7--pos-msg 'at-end-of
                                                (or (seed7--defun-name)
                                                    "??")))))
            (if (and end-long-func-pos
                     is-func-pos)
                (if (> is-func-pos end-long-func-pos)
                    (goto-char current-pos)
                  (goto-char (+ is-func-pos 1)))
              (forward-line 1))
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
                            (setq verbose (seed7--pos-msg 'at-end-of item-name))))
                      (user-error "End of %s not found: is code valid?" item-name)))
                   ;; Function
                   ((string-equal item-type  "func ")
                    (if (and group5
                             (string-equal group5 "func"))
                        ;; long func that ends with end func;
                        (if (search-forward "end func;")
                            (progn
                              (setq final-pos (point))
                              (when verbose
                                (setq verbose (seed7--pos-msg 'at-end-of item-name))))
                          (user-error "End of %s not found: is code valid?"
                                      item-name))
                      ;; short func with simpler return
                      (if (re-search-forward "[[:space:]]return[[:space:]]+[^\\0]+?;"
                                             nil :noerror)
                          ;; [:todo 2025-04-07, by Pierre Rouleau: fix required:
                          ;; check that syntax at point is not comment or string
                          ;; at point to validate find. Do this only once the
                          ;; syntax support is complete and detects strings/comments
                          ;; properly. ]
                          (progn
                            (setq final-pos (point))
                            (when verbose
                              (setq verbose (seed7--pos-msg 'at-end-of item-name))))
                        (user-error "Function not terminated properly!"))))
                   ;; The next line should never occur, if it does report a bug
                   ;; providing a code example to reproduce.
                   (t (error "Not inside a procedure or function!"))))
              (user-error "No Seed7 end of function or procedure found below!"))))))
    (seed7--move-and-mark original-pos final-pos dont-push-mark verbose)))


(defun seed7--end-of-defun-simple (&optional n)
  "Simple end of defun to use as `end-of-defun-function'.

Move once, unless N specifies a different count.
Operate silently; do not issue an error when nothing is found.
Return t if point moved to the beginning of function, nil if nothing found."
  (interactive "^p")
  (condition-case nil
      (progn
        (seed7-end-of-defun n :silent)
        t)
    (error nil)))

;;* Seed7 Navigation by Block
;;  -------------------------
;;
;; The following commands allow moving to the end or the beginning of if/end
;; if and other types of blocks as identified by the keywords that start them
;; in `seed7--block-start-keywords'.

(defun seed7--current-line-nth-word (n)
  "Return the N-th word at beginning of current line, nil if none.
For the first word N must be 1.
Negative N starts counting from the end of the line: -1 is the last word."
  (save-excursion
    (let ((line-number-str (format-mode-line "%l")))
      (if (< n 0)
          ;; Count from end of line
          (progn
            (setq n (abs n))
            (move-end-of-line 1)
            (dotimes (_ n)
              (backward-word)))
        ;; count from beginning of line
        (forward-line 0)
        (dotimes (_ n)
          (forward-word))
        (backward-word))
      ;; Return the identified word or nil if found on a different line
      (when (string= line-number-str (format-mode-line "%l"))
        (thing-at-point 'word :no-properties)))))

(defun seed7--type-regexp (keyword)
  "Return a regexp to search for the start/end of KEYWORD type block.

The regexp has 2 capture groups:
- group1 for the starting expression,
- group2 for then end part."
  (format "^\\(?:[[:space:]]*?\\(const[[:space:]]+?type:.+?[[:space:]]%s\\)\\|[[:space:]]+?\\(end %s;\\)\\)"
          keyword keyword))

(defun seed7--end-regxp-for (word1 word2 last-word)
  "Return regexps for end and start of block for specified arguments.

- WORD1 : the first word
- WORD2 : the second word
- LAST-WORD: the last word.

Return a regexp that searches for the start or end of the block.
The start text is in group1, the end text is in group 2."
  (cond
   ;; deal with special cases first
   ;; Get the regexp for searching a block. Each regex is a string with 2
   ;; capturing sections: match 1 is the start of the block, match 2 is the end.
   ((string= word1 "repeat") "^[[:space:]]+?\\(repeat\\)\\|\\(until\\) " )
   ((string= word1 "block")  "^[[:space:]]+?\\(block\\)\\|\\(end block\\)" )
   ((string= word1 "when")   "^[[:space:]]+?\\(when\\) \\|\\(end case;\\)")
   ((string= word1 "elsif")  "^[[:space:]]+?\\(if\\) \\|\\(else\\|end if;\\)")
   ((string= word1 "else")   "^[[:space:]]+?\\(if\\) \\|\\(end if;\\)")
   ((string= word1 "const")
    (cond
     ((string= word2 "type")
      (cond
       ((member last-word '("enum"
                            "struct"))
        (seed7--type-regexp last-word))
       (t nil)))
     ((string= word2 "array") "^\\(?:[[:space:]]*?\\(const[[:space:]]+?array[[:space:]]+?.+?:\\)\\|[[:space:]]+?\\();\\)\\)")
     (t nil)))
   ;; then deal with general case: block, case, for, while.
   ((member word1 seed7--block-start-keywords)
    (format "^[[:space:]]+\\(%s \\)\\|\\(end %s;\\)" word1 word1))
   (t nil)))


(defun seed7--start-regxp-for (word1 word2)
  "Return a regexp to search the starting string block specified by the arguments.

- WORD1 : the first word of the end statement.
- WORD2 : the second word of the end statement.

Return a regexp that searches for the start or end of the block.
The start text is in group1, the end text is in group 2."
  (cond
   ;; deal with special cases first
   ;; [:todo 2025-05-12, by Pierre Rouleau: only support array: add more if needed.]
   ((not word1) "^\\(?:[[:space:]]*?\\(const[[:space:]]+?array[[:space:]]+?.+?:\\)\\|[[:space:]]+?\\();\\)\\)")
   ((string= word1 "until") "^[[:space:]]+?\\(repeat\\)\\|\\(until\\) ")
   ((string= word1 "when")  "^[[:space:]]+?\\(when\\) \\|\\(end case;\\)")
   ((string= word1 "elsif")  "^[[:space:]]+?\\(if\\) \\|\\(end if;\\)")
   ((string= word1 "else")   "^[[:space:]]+?\\(if\\|elsif\\) \\|\\(end if;\\)")
   ((string= word1 "end")
    (cond
     ((string= word2 "block")
      "^[[:space:]]+?\\(block\\(?:[[:space:]]*?#.*?\\)?$\\)\\|\\(end block;\\)")
     ((member word2 '("case"
                      "for"
                      "if"
                      "while"))
      (format"^[[:space:]]+?\\(%s \\)\\|\\(end %s;\\)" word2 word2))
     ((member word2 '("enum"
                      "struct"))
      (seed7--type-regexp word2))
     (t nil)))
   (t nil)))


(defun seed7-to-block-forward (&optional dont-push-mark)
  "Move forward from the block beginning to its end.

Handle function and forward declarations blocks.
Push mark unless DONT-PUSH-MARK is non-nil.  Supports shift-marking."
  (interactive "^")
  (let* ((first-word  (seed7--current-line-nth-word 1))
         (second-word (seed7--current-line-nth-word 2))
         (last-word   (seed7--current-line-nth-word -1))
         (regexp      (seed7--end-regxp-for first-word second-word last-word))
         (found-position nil))
    (save-excursion
      (if regexp
          (let ((nesting 0)
                (searching t))
            (while searching
              ;; skip current line: move to end of line to search forward
              (forward-line 1)
              (if (re-search-forward regexp nil :noerror)
                  (cond
                   ;; found another block start text
                   ((match-string 1)
                    (setq nesting (1+ nesting)))

                   ;; found block end text
                   ((match-string 2)
                    (if (eq nesting 0)
                        (progn
                          (setq searching nil)
                          (setq found-position (point)))
                      (setq nesting (1- nesting))))
                   ;; found nothing
                   (t (user-error "No match!")))
                (user-error "NO match!"))))
        ;; not pushing mark is also an indication to operate silently
        (seed7-end-of-defun nil dont-push-mark dont-push-mark)
        (setq found-position (point))))
    (when found-position
      (unless dont-push-mark (push-mark))
      (goto-char found-position))))


(defun seed7-to-block-backward (&optional at-beginning-of-line dont-push-mark)
  "Move backward from block end to its beginning.

Move point to the beginning of the block keyword, unless
AT-BEGINNING-OF-LINE optional argument is set; in that case move point to the
beginning of the line.

Push mark unless DONT-PUSH-MARK is non-nil.  Supports shift-marking."
  (interactive "^P")
  (let* ((first-word (seed7--current-line-nth-word 1))
         (second-word (seed7--current-line-nth-word 2))
         (regexp      (seed7--start-regxp-for first-word second-word))
         (found-position nil))
    (save-excursion
      (if regexp
          (let ((nesting 0)
                (searching t))
            (while searching
              ;; skip current line: move to beginning of current line
              (forward-line 0)
              (if (re-search-backward regexp nil :noerror)
                  (cond
                   ;; found another block start text
                   ((match-string 1)
                    (if (eq nesting 0)
                        (progn
                          (setq searching nil)
                          (setq found-position (point)))
                      (setq nesting (1- nesting))))

                   ;; found a block end text
                   ((match-string 2)
                    (setq nesting (1+ nesting)))
                   ;; found nothing
                   (t (user-error "No match!")))
                (user-error "NO match!"))))
        ;; not pushing mark is also an indication to operate silently
        (seed7-beg-of-defun nil dont-push-mark dont-push-mark)
        (setq found-position (point))))
    (when found-position
      (unless dont-push-mark (push-mark))
      (goto-char found-position)
      (unless at-beginning-of-line
        (forward-word)
        (backward-word)))))

;; ---------------------------------------------------------------------------
;;* Seed7 Code Marking
;;  ==================

(defun seed7-mark-defun ()
  "Mark the current Seed7 function or procedure.
Put the mark at the end and point at the beginning.

If point is between 2 functions or procedure, mark the next one."
  (interactive)
  (let ((original-pos (point))
        (start-pos nil)
        (end-pos nil))
    (save-excursion
      (setq start-pos (progn
                        (seed7-beg-of-defun 1 :silent)
                        (point)))
      (setq end-pos (progn (seed7-end-of-defun 1 :silent)
                           (point))))
    (when (> original-pos end-pos)
      (save-excursion
        (setq end-pos (progn (seed7-end-of-defun 1 :silent)
                             (point)))
        (setq start-pos (progn
                          (seed7-beg-of-defun 1 :silent)
                          (point)))))
    (goto-char end-pos)
    (set-mark (point))
    (goto-char start-pos)))

;; ---------------------------------------------------------------------------
;;* Seed7 Indentation
;;  -----------------

;;** Seed7 Indentation Customization
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defcustom seed7-indent-width 2
  "Number of columns used for Seed7 code indentation."
  :group 'seed7
  :type 'integer)

(defcustom seed7-auto-indent t
  "Set to t to activate automatic indentation control."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Indentation Utility Macros
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defmacro seed7--set (fct var)
  "Set VAR with result of FCT call and return it.
Use inside a `cond' clause to emphasize the check."
  `(setq ,var ,fct))

(defmacro seed7--inside-block-p (syntax)
  "Return non-nil if point is inside matching pair block according to SYNTAX."
  `(> (nth 0 ,syntax) 0))

(defmacro seed7--inside-string-p (syntax)
  "Return non-nil if point is inside string according to SYNTAX list."
  `(nth 3 ,syntax))

(defmacro seed7--inside-comment-p (syntax)
  "Return non-nil if point is inside comment according to SYNTAX."
  `(nth 4 ,syntax))

(defmacro seed7--point-in-code-p (syntax)
  "Return non-nil when point is inside code, nil otherwise.
The SYNTAX argument holds the value returned by `syntax-ppss' for point."
  `(and (not (seed7--inside-string-p ,syntax))
        (not (seed7--inside-comment-p ,syntax))))

;;** Seed7 Indentation Utility Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;*** Seed7 Indentation Base utilities

(defun seed7-blank-line-p ()
  "Return non-nil when current line is a blank line, nil otherwise."
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun seed7-indent-step-for-column (column)
  "Return indentation step for COLUMN number."
  (/ column seed7-indent-width))

(defun seed7-current-line-number ()
  "Return the current line number.  0 for the first line."
  (string-to-number (format-mode-line "%l")))

(defun seed7-to-indent ()
  "Move point to the first non-whitespace character of the line."
  (forward-line 0)
    (skip-chars-forward " \t"))

;;*** Seed7 Indentation Code Character Search Utilities

(defun seed7-above-char-pos (char)
  "Back search for CHAR in code, return its position or nil.
CHAR is a string of 1 character.
Do not move point."
  (save-excursion
    (let ((found-pos nil)
          (keep-searching t))
      (while (and keep-searching
                  (not (bobp)))
        (if (search-backward char nil :noerror)
            ;; Found something.
            (if (or  (seed7-inside-comment-p)
                     (seed7-inside-string-p))
                (backward-char)
              (setq found-pos (point))
              (setq keep-searching nil))
          ;; Found nothing - stop
          (setq keep-searching nil)))
      found-pos)))

(defun seed7-below-char-pos (char)
  "Forward search for CHAR in code, return its position or nil.
CHAR is a string of 1 character.
Do not move point."
  (save-excursion
    (let ((found-pos nil)
          (syntax nil))
      (while (and (not found-pos)
                  (not (eobp)))
        (when (search-forward char nil :noerror)
          ;; Found something
          (save-excursion
            (setq syntax (syntax-ppss (point))))
          (if (seed7--point-in-code-p syntax)
              (setq found-pos (point))
            (forward-char))))
      found-pos)))


;;*** Seed7 Indentation Base Position Detection Utilities

(defun seed7-assign-op-pos ()
  "Position of end of previous Seed7 assignment operator if found.
Return nil if not found.
Do not move point."
  (save-excursion
    (let ((found-pos nil)
          (syntax nil)
          (match-text nil))
      (while (and (not found-pos)
                  (not (bobp)))
        (when (re-search-backward seed7-predef-assignment-operator-regxp
                                  nil :noerror)
          (setq syntax (syntax-ppss (point)))
          (setq match-text (match-string 0))
          (if (seed7--point-in-code-p syntax)
              (setq found-pos (point))
            (backward-char (length match-text)))))
      (when found-pos
        (+ found-pos (length match-text))))))

(defun seed7-statement-end-pos (&optional start-pos)
  "Position of next end of Seed7 statement if found, nil otherwise.
Start searching at current point, unless START-POS is non-nil/
Do not move point."
  (save-excursion
    (when start-pos
      (goto-char start-pos))
    (seed7-below-char-pos ";")))

;;*** Seed7 Indentation Base Inside Detection Utilities

;; [:todo 2025-05-21, by Pierre Rouleau: clarify identifying comments]
(defun seed7-inside-comment-p (&optional pos)
  "Return face of comment if POS or point is inside comment, nil otherwise.
Inside a comment, the returned value is:
- font-lock-comment-face           : inside comment block or en-line comment
- font-lock-comment-delimiter-face : at the # for line-end comment."
  ;; Using the face instead of the syntax, as I found the syntax
  ;; not reliable enough when looking at some edge cases: the open block
  ;; comment characters are not recognized as comment syntax.
  (let ((pos (or pos (point))))
    ;; if there's no overlay the face show up at the top
    (or (car-safe (memq  (get-char-property pos 'face)
                         '(font-lock-comment-face
                           font-lock-comment-delimiter-face)))
        ;; where there is one or several overlay, we need to
        ;; look into the text properties to see the face.
        (let ((text-prop (text-properties-at pos)))
          (and (eq (nth 0 text-prop) 'face)
               (or (eq (nth 1 text-prop) 'font-lock-comment-face)
                   (eq (nth 1 text-prop)
                       'font-lock-comment-delimiter-face)))))))

(defun seed7-inside-string-p (&optional pos)
  "Return non-nil if POS or point is inside a string, nil otherwise.
Note that the leading quote character does not register as inside a string."
  (let* ((pos (or pos (point)))
         (syntax (syntax-ppss pos)))
    (seed7--inside-string-p syntax)))

(defun seed7-current-line-start-inside-comment-p ()
  "Return non-nil if the current line start inside a comment."
  (save-excursion
    (seed7-to-indent)
    (seed7-inside-comment-p (point))))

;;*** Seed7 Indentation Base Line Navigation

(defun seed7-to-previous-non-empty-line (&optional dont-skip-comment-start)
  "Move point to the beginning of the previous non-empty line.
Skip lines starting with new comment unless DONT-SKIP-COMMENT-START is non
nil. Always skip lines that are *inside* a comment.
Return indentation column of that line, if the line is found.
Return nil if nothing found, but do not move point."
  (let ((found-pos nil))
    (save-excursion
      (while (and (not found-pos)
                  (not (bobp)))
        ;; Note: I would have used re-search-backward but found it unreliable here.
        ;;       Checking line by line is the most reliable way and since code
        ;;       normally does not have large set of empty lines it will
        ;;       behave fast enough.
        (forward-line -1)
        (save-excursion
          (skip-chars-forward " \t")
          (unless (looking-at "\n")
            (when (and (not (seed7-inside-comment-p (- (point) 1)))
                       (or dont-skip-comment-start
                           (not (seed7-inside-comment-p (point)))))
              (setq found-pos (point)))))))
    ;; found appropriate position: move to it and return its indentation.
    (when found-pos
      (goto-char found-pos)
      (current-column))))

(defun seed7-move-to-line (n &optional dont-skip-comment-start)
  "Move point to the beginning of text of line N.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
Return nil if no appropriate line found.
Return the column number of point if appropriate line found."
  (if (eq n :previous-non-empty)
      (seed7-to-previous-non-empty-line dont-skip-comment-start)
    (when (eq (forward-line n) 0)
      (skip-chars-forward " \t")
      (current-column))))

;;*** Seed7 Indentation Base Indent-step Value Helper

(defun seed7-line-indent-step (n &optional dont-skip-comment-start)
  "Return indentation of line N in indent-steps if found, 0 otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
An indent-step corresponds to the number of columns identified by
the `seed7-indent-width' user-option."
  (save-excursion
    (if (seed7-move-to-line n dont-skip-comment-start)
        (progn
          (skip-chars-forward " \t")
          (if (looking-at "include +\"")
              0
            (seed7-indent-step-for-column (current-column))))
      ;; No appropriate line found, return 0 for indentation.
      0)))

;;** Seed7 Indentation Line Checking Base Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-to-line-starts-with (regexp)
  "Move to previous line that starts with text specified by the REGEXP.
Return position of the text found if found, nil otherwise.
When something is found, leave point at the found position, if nothing
found do not move point."
  (let ((found-pos nil)
        (keep-searching t)
        (regexp (concat "^[[:blank:]]*?" regexp)))
    (save-excursion
      (while (and keep-searching
                  (not (bobp)))
        (when (re-search-backward regexp nil :noerror)
          (skip-chars-forward " \t")
          (if (or (seed7-inside-comment-p)
                  (seed7-inside-string-p))
              (forward-line 0)
            (setq found-pos (point))
            (setq keep-searching nil)))))
    (when found-pos
      (goto-char found-pos))))

(defun seed7-line-starts-with (n regexp &optional dont-skip-comment-start)
  "Return indent column when line N non-white space begins with REGEXP.
Return nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (skip-chars-forward " \t")
      (when (looking-at regexp :inhibit-modify)
        (current-column)))))

(defun seed7-column-of-line-that-starts-with (regexp &optional n)
  "Return the column of the previous line that begins with REGEXP.
Look into the previous N lines only unless N is nil.
When N is nil it searches all lines until the beginning of the buffer.
If something found within these lines, return it's indentation column,
otherwise return nil."
  (save-excursion
    (let ((limit (or n 0))
          (found-column nil))
      (while (and (not (bobp))
                  (not found-column)
                  (or (not n)
                      (> limit 0)))
        (setq limit (1- limit))
        (forward-line -1)
        (when (seed7-line-starts-with 0 regexp)
          (skip-chars-forward " \t")
          (setq found-column (current-column))
          (setq limit 0)))
      found-column)))

(defun seed7-line-ends-with (n regexp &optional dont-skip-comment-start)
  "Return non-nil when line N non-white space ends with REGEXP.

Return nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (end-of-line nil)
      (re-search-backward regexp (save-excursion
                                   (forward-line 0)
                                   (point))
                          :noerror))))

;;** Seed7 Indentation Line Type Checking Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-line-isa-string (n)
  "Return non-nil indent column if line N is a string, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with n "\""))

(defconst seed7-block-start-regexp "\\(\
const proc: \\|\
const func \\|\
const type: \\|\
elsif \\|\
if \\|\
while \\|\
for \\|\
case \\|\
catch \\|\
local\\|\
repeat\\|\
begin\\|\
block\\|\
else\\|\
exception\\|\
result\\)"
  "Regexp for the beginning of a Seed7 block. Match in group 0.")

(defconst seed7-block-line-start-regexp (concat
                                         "^[[:blank:]]*"
                                         seed7-block-start-regexp)
  "Regexp to find location of blocks.")

(defconst seed7-block-end-regexp "\
\\(\\(?:end \\(?:enum\\|for\\|func\\|if\\|struct\\|while\\|case\\|block\\);\\)\\|until \\)"
    "regexp for generic end of block.")

(defun seed7-line-is-block-start (n &optional dont-skip-comment-start)
  "Return t if line N is a block start, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (and
     (seed7-move-to-line n dont-skip-comment-start)
     (seed7-line-starts-with 0 seed7-block-start-regexp)
     (not (seed7-line-ends-with 0 "is .+;")))))

(defun seed7-line-is-type-block-start (n)
  "Return t if line N is a block start, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (and (seed7-line-starts-with n "const type: ")
       (not (seed7-line-ends-with n "is .+;"))))

(defun seed7-line-is-section-start (n)
  "Return t if line N is a section start, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with
   n
   "\\(\
elsif \\|\
else\\|\
exception\\)"))

(defun seed7-line-is-block-end (n &optional dont-skip-comment-start)
  "Return t if line N is a block end, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with n seed7-block-end-regexp dont-skip-comment-start))


(defun seed7-line-inside-a-block (n &optional dont-skip-comment-start)
  "Check if line N is inside a Seed7 block.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If nothing found it returns nil.
If it finds something it returns a list that holds the following information:
- indent column : indentation column the line that begins the block,
- match-string  : the found string describing the type of block,
- block start position,
- block end position."
  (save-excursion
    (condition-case nil
        (when (seed7-move-to-line n dont-skip-comment-start)
          (let ((current-pos (point))
                (block-start-pos nil)
                (block-end-pos nil)
                (match-text nil)
                (syntax nil)
                (found-column nil)
                (keep-searching t))
            (while (and keep-searching
                        (not (bobp)))
              (when (re-search-backward seed7-block-line-start-regexp nil :noerror)
                (setq match-text (match-string 1))
                (setq syntax (syntax-ppss (point)))
                (if (seed7--point-in-code-p syntax)
                    (progn
                      (skip-chars-forward " \t")
                      (setq block-start-pos (point))
                      (cond
                       ((member match-text '("const proc: "
                                             "const func "
                                             "const type: "))
                        (setq block-end-pos (seed7-below-char-pos ";")))
                       ((member match-text '("local"
                                             "repeat"
                                             "begin"
                                             "block"
                                             "else"
                                             "exception"
                                             "result"
                                             "if "
                                             "elsif "
                                             "while "
                                             "for "
                                             "catch "
                                             "case "))
                        (seed7-to-block-forward :dont-push-mark)
                        (setq block-end-pos (point))))
                      (if (and block-end-pos
                               (< block-start-pos current-pos block-end-pos))
                          (progn
                            (goto-char block-start-pos)
                            (if (string= match-text "case ")
                                ;; case statements has 2 nesting levels:
                                ;; one for the 'when' (not processed here)
                                ;; and another for the statements after the when
                                ;; (which we process here).
                                (setq found-column (+ (current-column)
                                                      seed7-indent-width))
                              (setq found-column (current-column)))
                            (setq keep-searching nil))
                        ;; found something that looks like a block, but either not
                        ;; a real block or not the block that holds the line need
                        ;; to search back further for a bigger block.  If
                        ;; block-start-pos is not a column 0, then keep searching
                        ;; above for the beginning of a larger block.
                        (if (eq (current-column) 0)
                            (setq keep-searching nil)
                          (goto-char (1- block-start-pos)))))
                  (backward-char (length match-text)))))
            (when found-column
              (list found-column match-text block-start-pos block-end-pos))))
      (error nil))))

(defun seed7-line-inside-until-logic-expression (n
                                                 &optional
                                                 dont-skip-comment-start)
  "Check if line N is inside a Seed7 until logic expression.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If it finds that the line is inside the until logic expression, it
returns the indentation column, corresponding to one column right of the
end or the until word.
If it detects that it is outside, it returns nil."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (block-start-pos nil)
            (block-end-pos nil)
            (syntax nil)
            (found-column nil)
            (found nil))
        (while (and (not found)
                    (not (bobp)))
          (when (re-search-backward "^[[:blank:]]+until " nil :noerror)
            (setq syntax (syntax-ppss (point)))
            (if (seed7--point-in-code-p syntax)
                (progn
                  (setq found t)        ; also stop on complete 'until ...;'
                  (unless (seed7-line-ends-with 0 ";")
                    ;; found an incomplete until statement remember position
                    (setq block-start-pos (point))
                    ;; Now search the end, which ends on a ';'.
                    (setq block-end-pos (seed7-statement-end-pos))
                    (when (< block-start-pos current-pos block-end-pos)
                      (goto-char block-start-pos)
                      (seed7-to-indent)
                      (setq found-column (+ 6 (current-column))))))
              ;; Found comment, move to beginning of line and search again
              (forward-line 0))))
        found-column))))


(defun seed7-line-inside-func-return-statement (n
                                                &optional
                                                dont-skip-comment-start)
  "Check if line N is inside a Seed7 func return statement.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If it finds that the line is inside the func return statement, it
returns the indentation column of the return keyword.
If it detects that it is outside, it returns nil."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (start-pos nil)
            (end-pos nil)
            (found-column nil)
            (syntax nil)
            (found nil))
        (while (and (not found)
                    (not (bobp)))
          (when (re-search-backward "^[[:blank:]]+return " nil :noerror)
            (setq syntax (syntax-ppss (point)))
            (if (seed7--point-in-code-p syntax)
                (progn
                  (setq found t)
                  (seed7-to-indent)
                  (setq start-pos (point))
                  (setq found-column (+  (current-column) 5))
                  (setq end-pos (seed7-below-char-pos ";"))
                  (unless (and end-pos
                               (< start-pos current-pos end-pos))
                    (setq found-column nil)))
              ;; found inside comment move to beginning of line
              ;; and search again
              (forward-line 0))))
        found-column))))

(defun seed7-line-inside-proc-argument-list-section (n
                                                     &optional
                                                     dont-skip-comment-start)
  "Check if line N is inside a Seed7 procedure argument list section.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If it finds that the line is inside the procedure list section it
returns the indentation column of the procedure.
If it detects that it is outside, it returns nil."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (start-pos nil)
            (end-pos nil)
            (found-column nil)
            (syntax nil)
            (found nil))
        (while (and (not found)
                    (not (bobp)))
          (when (re-search-backward
                 "^[[:blank:]]*const[[:blank:]]+proc[[:blank:]]*:[[:blank:]]"
                 nil
                 :noerror)
            (setq syntax (syntax-ppss (point)))
            (if (seed7--point-in-code-p syntax)
                (progn
                  (setq found t)
                  (seed7-to-indent)
                  (setq start-pos (point))
                  (setq found-column (current-column))
                  (when (search-forward " is func" nil :noerror)
                    (progn
                      (setq end-pos (point))
                      (unless (< start-pos current-pos end-pos)
                        (setq found-column nil)))))
              ;; found inside comment. Move to beginning of line
              ;; and search again
              (forward-line 0))))
        found-column))))


;; [:todo 2025-05-21, by Pierre Rouleau: Handle find in comment]
;; [:todo 2025-05-24, by Pierre Rouleau: handle tabs in search: replace ' ' by
;;  [[:blank:]]]
(defun seed7-line-inside-array-definition-block-p (n
                                                   &optional
                                                   dont-skip-comment-start)
  "Check if line N is inside an array definition block.
Return the indentation column of the array definition block statement
if line N is inside an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-end-pos nil)
            (block-indent-column nil))
        (when (re-search-backward "^ *?\\(?:const\\|var\\) +?array +?.+?:.+?("
                                  nil :noerror)
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (when (search-forward "(" nil :noerror)
            (backward-char)
            (forward-sexp)
            (setq block-end-pos (point))
            (when (< block-start-pos original-pos block-end-pos)
              block-indent-column)))))))

;; [:todo 2025-05-21, by Pierre Rouleau: Handle find in comment]
(defun seed7-line-at-endof-array-definition-block-p (n
                                                     &optional
                                                     dont-skip-comment-start)
  "Check if line N is the end of an array definition block.
Return the indentation column of the array definition block statement
if line N is the end of an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((line-start-pos (point))
             (block-indent-column (progn
                                    (skip-chars-forward " \t")
                                    (current-column)))
             (line-end-pos (progn
                             (end-of-line)
                             (point)))
             (block-start-pos nil)
             (block-end-pos nil))
        (when (search-backward ");" nil :noerror)
          (forward-char)
          (setq block-end-pos (point))
          (backward-sexp)
          (seed7-to-indent)
          (when (looking-at "\\(?:const\\|var\\) +?array +?.+?:.+?("
                            :inhibit-modify)
            (setq block-start-pos (point))
            (when (< block-start-pos line-start-pos block-end-pos line-end-pos)
              block-indent-column)))))))

;; [:todo 2025-05-21, by Pierre Rouleau: Handle find in comment]
(defun seed7-line-inside-set-definition-block-p (n
                                                 &optional
                                                 dont-skip-comment-start)
  "Check if line N is inside a set definition block.
Return the indentation column of the set definition block statement
if line N is inside an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-end-pos nil)
            (block-indent-column nil))
        (when (re-search-backward "^ *?\\(?:const\\|var\\) +?set +?.+?:.+?{"
                                  nil
                                  :noerror)
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (when (search-forward "{" nil :noerror)
            (backward-char)
            (forward-sexp)
            (setq block-end-pos (point))
            (when (< block-start-pos original-pos block-end-pos)
              block-indent-column)))))))


(defun seed7-line-inside-logic-check-expression-p (n
                                                   &optional
                                                   dont-skip-comment-start)
  "Check if line N is inside a logic check expression.
Return the indentation column of the space following the check keyword
if line N is inside an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((current-pos (point))
             (end-pos nil)
             (keep-searching t)
             (start-pos (seed7-to-line-starts-with
                         "\\(?:while\\|if\\|when\\)[[:blank:]]"))
             (keyword  (and start-pos
                            (seed7--current-line-nth-word 1)))
             (end-str (cond
                       ((string= keyword "while") " do")
                       ((string= keyword "if")    " then")
                       ((string= keyword "when")  ":")
                       (t nil))))
        (when end-str
          (while (and keep-searching
                      (not (eobp)))
            (when (search-forward end-str nil :noerror)
              (if (or (seed7-inside-comment-p)
                      (seed7-inside-string-p))
                  ;; found end-str but no in code
                  (forward-char)
                ;; found end-str
                (setq end-pos (point))
                (setq keep-searching nil)))))
        (when (and end-pos
                   (< start-pos current-pos end-pos))
          (goto-char start-pos)
          (skip-chars-forward " \t")
          (+ (current-column) (length keyword) 1))))))

;; [:todo 2025-05-22, by Pierre Rouleau: is this handling comment?]
(defun seed7-line-inside-assign-statement-continuation-p (n
                                                          &optional
                                                          dont-skip-comment-start)
  "Check if line N is inside a Seed7 assignment statement.
Return the indentation column of the code following the statement
operator on the assignment statement if line N is inside a statement
continuation line, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((current-pos (point))
             (assignment-pos (seed7-assign-op-pos))
             (statement-end-pos (when assignment-pos
                                  (seed7-statement-end-pos assignment-pos))))
        (when (and assignment-pos
                   statement-end-pos
                   (< assignment-pos current-pos statement-end-pos))
          (goto-char assignment-pos)
          (skip-chars-forward " \t")
          (current-column))))))

;; [:todo 2025-05-21, by Pierre Rouleau: Handle find in comment]
(defun seed7-line-at-endof-set-definition-block-p (n
                                                   &optional
                                                   dont-skip-comment-start)
  "Check if line N is the end of a set definition block.
Return the indentation column of the set definition block statement
if line N is the end of an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((line-start-pos (point))
             (block-indent-column (progn
                                    (skip-chars-forward " \t")
                                    (current-column)))
             (line-end-pos (progn
                             (end-of-line)
                             (point)))
             (block-start-pos nil)
             (block-end-pos nil))
        (when (search-backward "};" nil :noerror)
          (forward-char)
          (setq block-end-pos (point))
          (backward-sexp)
          (seed7-to-indent)
          (when (looking-at "\\(?:const\\|var\\) +?set +?.+?:.+?{"
                            :inhibit-modify)
            (setq block-start-pos (point))
            (when (< block-start-pos line-start-pos block-end-pos line-end-pos)
              block-indent-column)))))))

;; [:todo 2025-05-25, by Pierre Rouleau: Improve next function by restricting
;;        search area to current scope]
(defun seed7-line-inside-parens-pair (n &optional dont-skip-comment-start)
  "Check if line N is inside a parens pair.
Return the indentation column of the character after the opening parens
if line N is between a parens pair, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((start-pos nil)
             (end-pos   nil)
             (line-start (point))
             (candidate-list nil)
             (open-paren nil)
             (keep-searching-this-paren nil))
        (dolist (op '("()" "[]" "{}" "<>"))
          (save-excursion
            (setq open-paren (substring op 0 1))
            (setq keep-searching-this-paren t)
            (while keep-searching-this-paren
              (setq start-pos (seed7-above-char-pos  open-paren))
              (if start-pos
                  (progn
                    (goto-char start-pos)
                    (forward-sexp)
                    (backward-char)
                    (setq end-pos (point))
                    (if (< start-pos end-pos line-start)
                        ;; found a pair that is closed above current position
                        ;; move to its opening paren and keep searching
                        (goto-char start-pos)
                      (if (< start-pos line-start end-pos)
                          ;; found a candidate
                          (progn
                            (setq candidate-list
                                  (push (list op start-pos end-pos) candidate-list))
                            (setq keep-searching-this-paren nil))
                        ;; nothing found. Stop searching this paren
                        (setq keep-searching-this-paren nil))))
                ;;
                (setq keep-searching-this-paren nil)))))
        (when candidate-list
          ;; sort the list by distance between open paren and start of line
          (setq candidate-list (sort candidate-list
                                     :key (lambda (e) ""
                                            (- start-pos (nth 1 e)))))
          ;; The inner block is in the first element of the candidate-list
          ;; Return the column position right after its opening paren.
          (goto-char (nth 1 (nth 0 candidate-list)))
          (1+ (current-column)))))))

(defun seed7-indentation-of-previous-non-string-line ()
  "Return indentation of previous line that is not starting with a string."
  (save-excursion
    (let ((found nil))
      (while (not found)
        (forward-line -1)
        (when (and (not (seed7-line-starts-with 0 "\""))
                   (not (seed7-inside-comment-p (point)))
                   (save-excursion
                     (skip-chars-forward " \t")
                     (not (looking-at "\n"))))
          (setq found t))))
    (skip-chars-forward " \t")
    (current-column)))


(defun seed7-below-end-of-func (n &optional dont-skip-comment-start)
  "Check if line N is below the end of a func definition block.
Return the indentation column of the func definition opening character
if line N is below the end of a func definition block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (condition-case nil
        (let ((current-pos (point))
              (start-pos nil)
              (end-pos nil)
              (column nil))
          (when (and  (seed7-move-to-line n dont-skip-comment-start)
                      (seed7-move-to-line :previous-non-empty))
            (when (seed7-line-ends-with 0 ";")
              (end-of-line)
              (let ((keep-searching t))
                (while keep-searching
                  (search-backward ";")
                  (if  (seed7-inside-comment-p)
                      (backward-char)
                    (setq keep-searching nil))))
              (forward-char)
              (setq end-pos (point))
              (seed7-to-block-backward nil :dont-push-mark)
              (setq start-pos (point))
              (setq column (current-column))
              (seed7-to-block-forward :dont-push-mark)
              (when (and (eq (point) end-pos)
                         (< start-pos end-pos current-pos))
                column))))
      (error nil))))


;;** Seed7 Indentation Comment Checking Function
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-comment-column (recurse-count)
  "Return the column number for comment start or continuation."
  (save-excursion
    ;; What type of comment is this line ?
    (seed7-to-indent)
    (cond
     ;; If at the beginning of a new comment
     ((and (or (bobp)
               (seed7-line-starts-with -1 "#")
               (not (seed7-inside-comment-p (- (point) 1))))
           (or  (looking-at "#" :inhibit-modify)
                (looking-at "(\\*" :inhibit-modify)))
      (if (seed7-above-char-pos  ";")
          (let ((spec-list nil))
            ;; if just below a closed when case, leave the comment at the
            ;; same level as the when keyword
            (if (and (seed7-line-starts-with :previous-non-empty
                                             "when[[:blank:]]")
                     (seed7--set (seed7-line-inside-a-block 0)
                                 spec-list)
                     (string= (substring-no-properties (nth 1 spec-list))
                              "case "))
                (nth 0 spec-list)
              ;; if there are (other) statements above, line up the comment
              ;; according to the nature of the previous line as if the
              ;; current line was not a comment: re-use the logic of
              ;; `seed7-calc-indent' to get the indentation.  Only 1 level of
              ;; recursion should be necessary (and allowed).
              (condition-case nil
                  (setq current-column (seed7-calc-indent
                                        :treat-comment-line-as-code
                                        (1+ recurse-count)))
                ;; If no rule was found for the code, force the indentation to 0
                ;; as if there was no statements above.
                (error 0))))
        ;; If there are no statements above indent a column 0.
        0))

     ;; Inside a block comment continuation line.
     ;; Align it only if:
     ;;     the current line starts with a '*' and
     ;;     the previous line starts with '*' or '(*'.
     ;; A comment that starts with anything else is not aligned: it
     ;; stays the way it's written.
     (t
      (let ((column nil))
        (if (seed7-line-starts-with 0 "\\*")
            (cond
             ((seed7--set (seed7-line-starts-with -1 "\\*") column)
              column)
             ((seed7--set (seed7-line-starts-with -1 "(\\*") column)
              (1+ column))
             (t (current-column)))
          (current-column)))))))


;;** Seed7 Indentation Calculator Function
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-calc-indent (&optional treat-comment-line-as-code recurse-count)
  "Calculate and return indentation (in columns) of current line of code.
When SKIP-STRING is non-nil do not base the indentation on the position
of a string."
  (let ((recurse-count (or recurse-count 0))
        (indent-step (seed7-line-indent-step :previous-non-empty))
        (first-word-on-line      (seed7--current-line-nth-word 1))
        (indent-column nil)
        (spec-list nil))
    (cond
     ((> recurse-count 1)
      (error "Recursion done more than once: implementation logic error!"))

     ((and (seed7-current-line-start-inside-comment-p)
           (not treat-comment-line-as-code))
      (setq indent-column (seed7-comment-column recurse-count)))

     ((or (seed7--set (seed7-line-inside-array-definition-block-p 0)
                      indent-column)
          (seed7--set (seed7-line-inside-set-definition-block-p 0)
                      indent-column))
      (setq indent-column (+ indent-column
                             (* 2 seed7-indent-width))))
     ((seed7-line-isa-string 0)
      (save-excursion
        ;; if previous line starts with a string, align the string to it.
        (cond
         ((seed7--set (seed7-line-inside-parens-pair 0)
                      indent-column))
         ((seed7-line-isa-string :previous-non-empty)
          (forward-line -1)
          (search-forward "\"")
          (setq indent-column (1- (current-column))))
         (t (message "String line syntax not yet supported.")))))

     ;; Special rule: if a line starts with a Seed7 assignment operator,
     ;; consider that line manually indented and keep it where it is.
     ;; This allows a long multi-line statement to be lined up on the
     ;; assignment operator placed on the line after its rvalue for the
     ;; explicit purpose of allowing that manual alignment mechanism.
     ((seed7--set (seed7-line-starts-with
                   0
                   seed7-predef-assignment-operator-regxp)
                  indent-column))
     ((seed7--set (seed7-line-inside-assign-statement-continuation-p 0)
                  indent-column))

     ((string= first-word-on-line "$")
      (setq indent-step 0))
     ((string= first-word-on-line "include")
      (cond
       ((seed7--set (seed7-line-starts-with
                     :previous-non-empty "include ")
                    indent-column))
       ((seed7-line-starts-with
         :previous-non-empty "$ include ")
        (setq indent-step 1))
       (t (setq indent-step 0))))

     ((member first-word-on-line '("local"
                                   "begin"))
      (cond
       ((seed7-line-is-block-start :previous-non-empty)
        (setq indent-step (1+ indent-step)))
       ((seed7-line-starts-with :previous-non-empty "end for is func")
        (let ((column-of-proc (seed7-column-of-line-that-starts-with
                               "const proc: for " 10)))
          (if column-of-proc
              (setq indent-step (1+ (seed7-indent-step-for-column
                                     column-of-proc)))
            (error "Can't identify start of for at line %s"
                   (format-mode-line "%l")))))
       ((seed7--set (seed7-line-inside-proc-argument-list-section
                     :previous-non-empty)
                    indent-column)
        (setq indent-column (+ indent-column seed7-indent-width)))
       (t (setq indent-step (1- indent-step)))))

     ((seed7--set (seed7-line-inside-parens-pair 0)
                  indent-column))

     ((seed7-line-is-type-block-start :previous-non-empty)
      (setq indent-step (+ indent-step 2)))

     ((seed7--set (seed7-line-inside-logic-check-expression-p 0)
                  indent-column))

     ((seed7-line-is-block-start :previous-non-empty)
      (setq indent-step (1+ indent-step)))

     ((seed7-line-is-block-end 0)
      (save-excursion
        (seed7-to-block-backward nil :dont-push-mark)
        (setq indent-column (current-column)))
      (when (or
             (seed7-line-starts-with 0 "end func;")
             (seed7-line-starts-with 0 "end struct;")
             (seed7-line-starts-with 0 "end enum;"))
        (setq indent-column (+ indent-column seed7-indent-width))))

     ((seed7--set (seed7-line-inside-proc-argument-list-section
                   :previous-non-empty)
                  indent-column)
      (setq indent-column (+ indent-column seed7-indent-width)))

     ((seed7-line-is-section-start 0)
      (setq indent-step (1- indent-step)))

     ((seed7-line-isa-string :previous-non-empty)
      (setq indent-column (seed7-indentation-of-previous-non-string-line)))

     ((string= first-word-on-line "when")
      (setq indent-column (+ (seed7-column-of-line-that-starts-with "case ")
                             seed7-indent-width)))

     ;; when-end-case otherwise clause:
     ((and
       (string= first-word-on-line "otherwise")
       (seed7--set (seed7-line-inside-a-block 0)
                   spec-list)
       (string= (substring-no-properties (nth 1 spec-list)) "case "))
      (setq indent-column (nth 0 spec-list)))

     ((seed7--set (seed7-line-starts-with :previous-non-empty "when")
                  indent-column)
      (setq indent-column (+ indent-column seed7-indent-width)))

     ((seed7-line-is-block-end :previous-non-empty)
      ;; keep the same indentation as the previous line unless:
      ;; - the block end was a "end func;",
      ;; - the block end was a "end struct;",
      ;; - the current line is exception or until
      ;; in which case decrement indentation
      (cond
       ((or (member first-word-on-line '("exception"
                                         "until"))
            (seed7-line-starts-with :previous-non-empty "end func;")
            (seed7-line-starts-with :previous-non-empty "end struct;")
            (seed7-line-starts-with :previous-non-empty "end enum;"))
        (setq indent-step (1- indent-step)))
       ;; if the line is inside a 'until .... ;' area
       ((seed7--set (seed7-line-inside-until-logic-expression 0)
                    indent-column))
       ;; if line follows an until end
       ((save-excursion
          (forward-line -1)
          (when (string= (seed7--current-line-nth-word 1) "until")
            (skip-chars-forward " \t")
            ;; check if the until statement is complete or not
            (if (seed7-line-ends-with 0 ";")
                (setq indent-column (current-column))
              ;; line up after the 'until ' to continue.
              (setq indent-column (+ 6 (current-column)))))))))

     ;; Check if the line is inside a 'until .... ;' area
     ((seed7--set (seed7-line-inside-until-logic-expression 0)
                  indent-column))

     ((seed7--set (seed7-line-inside-func-return-statement 0)
                  indent-column)
      (setq indent-column (+ indent-column seed7-indent-width)))

     ((seed7--set (seed7-line-inside-a-block 0)
                  spec-list)
      (setq indent-column (nth 0 spec-list))
      (if (string= (nth 1 spec-list)
                   "const type: ")
          (setq indent-column (+ indent-column (* 2 seed7-indent-width)))
        (setq indent-column (+ indent-column seed7-indent-width))))

     ((or (seed7--set (seed7-line-at-endof-array-definition-block-p
                       :previous-non-empty)
                      indent-column)
          (seed7--set (seed7-line-at-endof-set-definition-block-p
                       :previous-non-empty)
                      indent-column))
      (setq indent-column (- indent-column
                             (* 2 seed7-indent-width))))
     ;; When inside a paren block, adjust indent to the column
     ;; following the open paren; any of: ( { [ <
     ((seed7--set (seed7-line-inside-parens-pair 0)
                  indent-column))

     ((and (string= first-word-on-line "end")
           (string= (seed7--current-line-nth-word 2) "block"))
      (setq indent-step (- indent-step 2)))
     ((string= first-word-on-line "until")
      (setq indent-step (1- indent-step)))
     ((and (string= first-word-on-line "var")
           (seed7-line-starts-with :previous-non-empty "include "))
      (setq indent-step 0))

     ((seed7--set (seed7-below-end-of-func 0)
                  indent-column))

     ((seed7-line-starts-with 0 "(")
      ;; for block comment comments or code withing parentheses,
      ;; if did not find a rule report it.  For comment it will be
      ;; caught by `seed7-comment-column' and that will force indent to 0,
      ;; inside code it will leave the line unchanged and will print the
      ;; error.
      (error "No rule yet to indent line %d" (seed7-current-line-number)))

     ;; don't indent blank lines
     ((and (not first-word-on-line)
           (seed7-blank-line-p))))
    (if indent-column
        indent-column
      (* indent-step seed7-indent-width))))

(defun seed7-indent-line ()
  "Indent the current Seed7 line of code."
  (interactive "*")
  (save-excursion
    (seed7-to-indent)
    (let ((current-indent (current-column))
          (indent (seed7-calc-indent)))
      (when (not (= indent current-indent))
        (progn
          (forward-line 0)
          (delete-horizontal-space)
          (unless (eq indent 0)
            (indent-to indent)))))))

;; ---------------------------------------------------------------------------
;;* Seed7 Compilation
;;  =================
;;


;; (defun seed7--filter-compiler-output ()
;;   "Filter function to format s7c compiler output."
;;   (save-excursion
;;     ))
;; compilation-filter-hook
;; (add-hook 'compilation-filter-hook #'grep--heading-filter 80 t)
;; [:todo 2025-04-07, by Pierre Rouleau: Filter s7c output to comply with
;;   required compile-mode buffer format.  Remove the "*** " prefix of error
;;   lines ]
;;

(defun seed7-compile (&optional compile)
  "Static check current Seed7 file, show errors in `compilation-mode' buffer.

If optional COMPILE argument set, compile the file to executable instead."
  (interactive "P")
  (compile (format "%s %s"
                   (if compile seed7-compiler seed7-checker)
                   (shell-quote-argument (buffer-file-name)))))

;; ---------------------------------------------------------------------------
;;* Seed7 Key Map
;;  =============
;;

(defvar seed7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-a"  'seed7-beg-of-defun)
    (define-key map "\M-\C-e"  'seed7-end-of-defun)
    (define-key map "\M-\C-h"  'seed7-mark-defun)
    (define-key map (kbd "C-c ;")  'seed7-toggle-comment-style)
    map)
  "Keymap used in seed7-mode.")

;; ---------------------------------------------------------------------------
;; Seed7 Menu
;; ==========

(easy-menu-define seed7-mode-menu seed7-mode-map
  "Menu for Seed7 Mode."
  '("Seed7"
    ["Toggle outline-minor-mode" outline-minor-mode
      :help "Control hiding/showing content of blocks"]
    ("Navigation"
     ["Forward func/proc" seed7-end-of-defun
      :help "Go forward to end of function or procedure"]
     ["Backward to func/proc" seed7-beg-of-defun
      :help "Go backward to the beginning of function or procedure"]
     ["Forward to next func/proc" seed7-beg-of-next-defun
      :help "Go forward to the beginning of next function or procedure"]
     ["Block end" seed7-to-block-forward
      :help "Go forward to end of block"]
     ["Block start" seed7-to-block-backward
      :help "Go backward to start of block"])
    "---"
    ["Static check"  seed7-compile t]
    ["Compile"       (seed7-compile t) t]
    "---"
    ["Customize Mode" (customize-group 'seed7) t]))

;; ---------------------------------------------------------------------------

;;* Seed7 Major Mode
;;  ================

;; [:todo 2025-04-09, by Pierre Rouleau: stop deriving from pascal-mode once
;;  the syntax control for Seed7 is written.  Using its own syntax will solve
;;  the comment rendering and the multi-line string rendering.  But for now
;;  derive from pascal so we can at least render some of the comments
;;  properly.]

;;;###autoload
(define-derived-mode seed7-mode prog-mode "seed7"
  "Major mode for editing Seed7 files."

  ;; Seed7 Font Locking Control
  (setq-local font-lock-defaults '((seed7-font-lock-keywords)))

  ;; Seed7 Mode Syntax Propertize Function
  (setq-local syntax-propertize-function #'seed7-mode-syntax-propertize)

  ;; iMenu Support / Speedbar Support
  (setq-local imenu-generic-expression
              (list
               (list "Enum"      seed7-enum-regexp 1)
               (list "Interface" seed7-interface-regexp 1)
               (list "Struct"    seed7-struct-regexp 1)
               (list "Procedure" seed7-procedure-regexp 1)
               (list "Function"  seed7-function-regexp  2)))

  ;; Seed7 Comments Control
  (seed7--set-comment-style seed7-uses-block-comment)

  ;; Seed7 Code Navigation
  ;; Allow code familiar with the standard `beginning-of-defun' and
  ;; `end-of-defun' to work inside Seed7 buffers.  This includes iedit,
  ;; expand-region, etc...
  (setq-local beginning-of-defun-function
              (function seed7--beg-of-defun-simple))
  (setq-local end-of-defun-function
              (function seed7--end-of-defun-simple))

  ;; Seed7 outline minor-mode support
  (setq-local outline-regexp
              "const \\(type: \\|proc: \\|func \\)")
  (setq-local outline-heading-end-regexp
              "\\( is\\(?:\\ new struct| func\\)?\\)")

  ;; Seed7 Indentation
  (when seed7-auto-indent
    (setq-local indent-line-function (function seed7-indent-line))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;;; seed7-mode.el ends here
