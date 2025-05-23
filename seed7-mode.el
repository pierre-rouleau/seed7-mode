;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-05-16 14:23:25 EDT, updated by Pierre Rouleau>

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
;; - Syntax and semantics support for Seed7 - done.
;;   - keyword colorization - done.
;; - Support comment:
;;   - comment syntax: colorization, creation, deletion - done.
;;   - hiding/showing comments: working but for 1 comment style only.
;; - Launch help on keywords, perhaps implement statement help
;; - Indentation help: with TAB: adjusts indentation when typed
;;                               anywhere on the line.
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
;; ]
;;
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
;;     - `seed7--is-first'
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

;;** Seed7 Customization

;; --
(defface seed7-pragma-keyword-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SlateBlue2" :background "lightYellow1" :weight bold))
    (((class color) (background dark))
     (:foreground "SlateBlue2" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight pragma keywords."
  :group 'seed7-faces)

(defface seed7-include-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SlateBlue3" :background "lightYellow1" :weight bold))
    (((class color) (background dark))
     (:foreground "SlateBlue3" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight include."
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
     (:foreground "DodgerBlue3" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords."
  :group 'seed7-faces)

(defface seed7-in-statement-keyword-face2
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "SteelBlue3" :weight bold))
    (((class color) (background dark))
     (:foreground "SteelBlue3" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords."
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
     (:foreground "DeepSkyBlue2" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords that introduce a statement."
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
     (:foreground "RoyalBlue2" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords used in middle of statements."
  :group 'seed7-faces)

(defface seed7-in-middle-statement-keyword-face2
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "RoyalBlue1" :weight bold))
    (((class color) (background dark))
     (:foreground "RoyalBlue1" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight keywords used in middle of statements."
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
     (:foreground "blue1" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight statement intro keywords."
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
     (:foreground "dark cyan" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight predefined variable names."
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
     (:foreground "cadet blue" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
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
     (:foreground "Chocolate3" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
  :group 'seed7-faces)

(defface seed7-integer-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "peru" ))
    (((class color) (background dark))
     (:foreground "peru" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
  :group 'seed7-faces)

(defface seed7-number-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold))

    (((class color) (background light))
     (:foreground "sienna" ))
    (((class color) (background dark))
     (:foreground "sienna" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
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
     (:foreground "black" :background ,seed7-dark-background :weight bold))

    (t (:weight bold)))
  "Font Lock mode face used to highlight errinfo values."
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
   "^[[:space:]]*const%s+\\(\\(func \\|proc\\)\\)\\([[:alpha:]][[:alnum:]_]+\\)?%s?:%s*\\([[:alpha:]][[:alnum:]_]+\\)%s*?is%s+\\(func\\|return\\)"
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
                  (right-char)
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
        (left-char)))
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
        (right-char)
        (if (re-search-forward seed7-procedure-or-function-regexp
                               nil :noerror)
            (progn
              ;; Point is now at end of function definition.
              ;; Move to the beginning of the function definition.
              (right-char)
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

(defun seed7--is-first (a b c)
  "Return t is A is smaller than B and C.
If A is nil return nil.
If B or C are nil consider A smaller."
  (unless (not a)
    (and
     (or (not b) (< a b))
     (or (not c) (< a c)))))

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
          (setq end-long-func-pos
                (re-search-forward "end[[:space:]]+func;" nil :noerror)))
        (if (seed7--is-first end-short-func-pos
                             is-func-pos
                             end-long-func-pos)
            (progn
              (setq final-pos end-short-func-pos)
              (save-excursion
                ;; extract current function name
                (let ((item-name nil))
                  (when (re-search-backward seed7-procedure-or-function-regexp
                                            nil :noerror)
                    (setq item-name (substring-no-properties (match-string 4))))
                  (setq verbose (seed7--pos-msg 'at-end-of (or item-name  "??"))))))
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
            (user-error "No Seed7 end of function or procedure found below!")))))
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
   ((string= word1 "repeat") "^[[:space:]]+?\\(repeat\\)\\|\\(until\\) " )
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


(defun seed7-to-block-forward ()
  "Move forward from the block beginning to its end.

Push mark.  Supports shift-marking."
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
        (seed7-end-of-defun)
        (setq found-position (point))))
    (when found-position
      (push-mark)
      (goto-char found-position))))


(defun seed7-to-block-backward (&optional at-beginning-of-line)
  "Move backward from block end to its beginning.

Move point to the beginning of the block keyword, unless
AT-BEGINNING-OF-LINE optional argument is set; in that case move point to the
beginning of the line.

Push mark.  Supports shift-marking."
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
        (seed7-beg-of-defun)
        (setq found-position (point))))
    (when found-position
      (push-mark)
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
              "\\( is\\(?:\\ new struct| func\\)?\\)"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;;; seed7-mode.el ends here
