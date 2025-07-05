;;; seed7-mode.el --- Support for the Seed7 Programming Language.  -*- lexical-binding: t; -*-

;; Created   : Wednesday, March 26 2025.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2025-07-05 07:45:22 EDT, updated by Pierre Rouleau>

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

;; Feature wish-list and status.
;;
;; =========================================== ===============================
;; Feature                                     Status
;; =========================================== ===============================
;; FONTIFICATION
;;
;; Syntax and semantics support for Seed7      Mostly done.
;; with fontification of Seed7 keywords and    Remaining problems listed below.
;; syntactic elements.
;;
;; COMMENTS
;;
;; Comment support: rendering, commenting      Done.
;; un-commenting code with `comment-dwim'.
;;
;; Selection of block or line-end comments:    Done.
;; by customization and by dynamic command.    `seed7-toggle-comment-style'
;;                                             toggles between the 2 types of
;;                                             comments.

;;
;; Selection of comment types: box, aligned,   Done.
;; multi-line, etc... as controlled by the
;; `comment-style' customizable user-option
;; variable.
;;
;; Hide/show comment toggle with the           Done.
;; hide-cmnt.el package.
;;
;; NAVIGATION
;; - Move point to the end of next function    Done: `seed7-end-of-defun'
;;   or procedure.
;; - Move point to the beginning of previous   Done: `seed7-beg-of-defun'
;;   function or procedure.
;; - Move point to the beginning of next       Done: `seed7-beg-of-next-defun'
;;   function or procedure.
;; - Move point to end of previous function    Not yet implemented here.
;;   or procedure.                             Available in PEL.
;; - Support of `beginning-of-defun' and       Done.  Silent and non-marking
;;   `end-of-defun' conventional protocol      version of the functions are
;;   to support other Emacs operations based   used.
;;   on this ability.
;; - Move to end/beginning of block.           Done.  For most code blocks
;;                                             inside functions and
;;                                             procedures.  Can also handle
;;                                             struct and enums.  Will also
;;                                             move to the multiple clauses of
;;                                             if/elsif/else,
;;                                             case/when/otherwise blocks.
;;                                             Also moves across comments:
;;                                             from the beginning to end of
;;                                             comment and vice-versa.  Treats
;;                                             consecutive lines of comments
;;                                             as a single comment.
;;
;; MARKING
;; - C-M-h function/procedure mark.            Done.  Use `seed7-mark-defun'
;; - iedit-mode support, allowing              Done.  Very useful for code
;;   selection of names in all buffer          refactoring.
;;   or restricted to a single function        Install iedit-mode package
;;   or procedure and modifications of all     for that.
;;   of them.
;;
;; AUTO-INDENT
;; - Automatic indentation of code and         Done.
;;   block comments with leading start         Auto indentation of function and
;;   characters with auto-fill mode support.   /procedure code and argument
;;                                             blocks, logic blocks and code
;;                                             inside parens of the 4
;;                                             shapes (), [], {} and <> is
;;                                             supported and currently
;;                                             imposed.
;;                                             A potential improvement would
;;                                             be to add customization for
;;                                             this.
;;                                             The `seed7-indent-width'
;;                                             user-option, which defaults to
;;                                             2, controls the indentation
;;                                             width.
;;
;; Static checking/compilation                 Done.  `seed7-compile' performs
;;                                             the operation identified in the
;;                                             `seed7-checker' and
;;                                             `seed7-compiler' customizable
;;                                             user options.
;;
;; CODE INSERTION HELP
;; - Keyword template expansion                Done.  Code expands specific
;;                                             keywords alone on line or
;;                                             before a closing parenthesis
;;                                             with boiler plate code with
;;                                             tempo markers at locations that
;;                                             must be filled.  The expansion
;;                                             is done with the <tab> key and
;;                                             the <backtab> moves to the next
;;                                             tempo marker.
;;
;; - Seed7-specific abbreviations              Done.  Support short
;;                                             Seed7-specific abbreviations
;;                                             that are expanded automatically
;;                                             when a word-separator key (such
;;                                             as <space>) is typed.  All
;;                                             abbreviations are customizable
;;                                             and start with a semi-colon so
;;                                             they do not clash with possible
;;                                             code symbols of
;;                                             identifiers.  This uses the
;;                                             abbrev-mode.  It is also allowed
;;                                             by customization.

;;
;; seed7-mode key map.                         Done.
;; Top Menu.                                   Done.
;; imenu support                               Done.
;; Speedbar support.                           Done.
;;
;; =========================================== ===============================

;; Please any problem you may notice by creating a bug report in the Github
;; project: https://github.com/pierre-rouleau/seed7-mode


;; Future:
;; - Launch help on keywords, perhaps implement statement help
;; - Keyword Completion help.

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
;;  # 04  The `seed7-to-block-backward' and `seed7-to-block-forward' can
;;        detect the other end when point is on the beginning or end line,
;;        but they fail when point is inside the block.
;;        I will fix that once I get the auto indentation working properly
;;        for all code.  I will then have to decide if that's considered a
;;        defun to allow marking the block just like procedure and functions.
;; ]
;;
;;
;;* Table of Content
;;  ----------------
;;
;; Code Organization Layout (use these as markers to locate related code; both
;; in the titles and inside other code locations).
;; With `lispy-mode' active, you can also use the `outline-minor-mode'
;; commands to navigate across section titles as well as hide/show the content
;; of sections.
;;
;; - Version Info
;; - Seed7 Customization
;; - Seed7 Keyword Regexp
;;    - Seed7 Tokens
;;      - Seed7 Comments Control
;;      - Seed7 Basic Element Regexp
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
;;    - Seed7 Arithmetic Operators
;;    - Seed7 Block Processing Regular Expressions
;; - Seed7 Mode Syntax Control
;;   - Seed7 Mode Syntax Table
;;   - Seed7 Mode Syntax Propertize Function
;;     . `seed7-mode-syntax-propertize'
;; - Seed7 Faces
;;   - `seed7-choose-color'
;; - Seed7 Font Locking Control
;; - Seed7 Comments Control
;;   * `seed7-toggle-comment-style'
;;     . `seed7--new-state-for'
;;     . `seed7--set-comment-style'
;; - Seed7 iMenu Support Regexp
;; - Seed7 Speedbar Support
;; - Seed7 Low-level Macros
;;   . `seed7--set'
;; - Seed7 Code Navigation
;;  - Seed7 Comment and String Identification Macros and Functions
;;    . `seed7--point-in-code-p'
;;      . `seed7--inside-string-p'
;;      . `seed7--inside-comment-p'
;;    . `seed7-inside-comment-p'
;;    . `seed7-inside-string-p'
;;      . `seed7--inside-string-p'
;;  - Seed7 Code Search Functions
;;    . `seed7-re-search-forward'
;;    . `seed7-re-search-backward'
;;  - Seed7 Procedure/Function Regular Expressions
;;  - Seed7 Skipping Comments
;;    . `seed7-skip-comment-backward'
;;    . `seed7-skip-comment-forward'
;;      . `seed7---skip-block-comment-forward'
;;      . `seed7---skip-line-end-comment'
;;  - Seed7 Navigation by Block/Procedure/Function
;;    - Navigation to Outer Block
;;      . `seed7-top-block-name'
;;        . `seed7--to-top'
;;        . `seed7--block-name'
;;      . `seed7-to-top-of-block'
;;        . `seed7--to-top'
;;    - Seed7 Procedure/Function Search Utility functions
;;      . `seed7--move-and-mark'
;;      . `seed7--pos-msg'
;;      . `seed7--show-info'
;;      . `seed7--no-defun-found-msg-for'
;;    - Seed7 Procedure/Function Navigation Commands
;;      * `seed7-beg-of-defun'
;;      * `seed7-beg-of-next-defun'
;;      * `seed7-end-of-defun'
;;        o `seed7--move-and-mark'
;;        o `seed7--pos-msg'
;;     - Seed7 Procedure/Function Navigation Mode Functions
;;       > `seed7--beg-of-defun-silently'
;;         o `seed7-beg-of-defun'
;;       > `seed7--end-of-defun-silently'
;;         o `seed7-end-of-defun'
;;   - Seed7 Navigation by Block
;;     * `seed7-to-block-forward'
;;       . `seed7--end-regxp-for'
;;         . `seed7--type-regexp'
;;     * `seed7-to-block-backward'
;;       . `seed7--current-line-nth-word'
;;       . `seed7--start-regxp-for'
;;         . `seed7--type-regexp'
;; - Seed7 iMenu Support
;; - Seed7 Code Marking
;;   * `seed7-mark-defun'
;; - Seed7 Indentation
;;   - Seed7 Indentation Customization
;;   - Seed7 Indentation Utility Macros
;;     . `seed7--inside-block-p'
;;   - Seed7 Indentation Utility Functions
;;     - Seed7 Indentation Base utilities
;;       . `seed7-blank-line-p'
;;       . `seed7-indent-step-for-column'
;;       . `seed7-current-line-start-inside-comment-p'
;;         o `seed7-inside-comment-p'
;;         . `seed7-to-indent'
;;       . `seed7-to-line-last-non-whitespace'
;;       . `seed7-at-end-of-line-p'
;;       . `seed7-inside-line-end-comment-p'
;;       . `seed7-inside-line-indent-p'
;;       . `seed7-inside-line-trailing-whitespace-before-line-end-comment-p'
;;         . `seed7-inside-line-indent-before-comment-p'
;;     - Seed7 Indentation Code Character Search Utilities
;;       . `seed7-backward-char-pos'
;;       . `seed7-forward-char-pos'
;;     - Seed7 Indentation Base Position Detection Utilities
;;       . `seed7-assign-op-pos'
;;       . `seed7-statement-end-pos'
;;     - Seed7 Indentation Base Line Navigation
;;       . `seed7-move-to-line'
;;         . `seed7-to-previous-non-empty-line'
;;     - Seed7 Indentation Base Indent-step Value Helper
;;       . `seed7-line-indent-step'
;;   - Seed7 Indentation Line Checking Base Functions
;;     . `seed7-to-previous-line-starts-with'
;;     . `seed7-to-next-line-starts-with'
;;     . `seed7-line-starts-with-any'
;;       . `seed7-line-starts-with'
;;     . `seed7-column-of-line-that-starts-with'
;;       o `seed7-line-starts-with'
;;     . `seed7-line-code-ends-with'
;;   - Seed7 Indentation Line Type Checking Functions
;;     . `seed7-line-isa-string'
;;     . `seed7-line-is-block-end'
;;     . `seed7-line-inside-a-block'
;;       . `seed7--block-end-pos-for'
;;       . `seed7--indent-offset-for'
;;         . `seed7--at-pos-looking-at-p'
;;       . `seed7--on-lineof'
;;     . `seed7-line-inside-until-logic-expression'
;;     . `seed7-line-inside-func-return-statement'
;;     . `seed7-line-inside-proc-argument-list-section'
;;     . `seed7-line-inside-array-definition-block'
;;     . `seed7-line-at-endof-array-definition-block'
;;     . `seed7-line-inside-set-definition-block'
;;     . `seed7-line-inside-logic-check-expression'
;;     . `seed7-line-inside-assign-statement-continuation'
;;     . `seed7-line-at-endof-set-definition-block'
;;     . `seed7-line-inside-parens-pair'
;;     . `seed7-line-inside-parens-pair-column'
;;     . `seed7-line-inside-nested-parens-pairs'
;;     . `seed7-line-inside-nested-parens-pairs-column'
;;     . `seed7-indentation-of-previous-non-string-line'
;;     . `seed7-line-is-defun-end'
;;       o `seed7-line-starts-with-any'
;;         o `seed7-line-starts-with'
;;   - Seed7 Indentation Comment Checking Function
;;     . `seed7-comment-column'
;;     . `';;   - Seed7 Indentation Calculator Function
;;     o `seed7-complete-statement-or-indent'
;;       * `seed7-indent-line'
;;         . `seed7-calc-indent'
;;           . `seed7--indent-one-line'
;; - Seed7 Code Template Expansion
;;   * `seed7-complete-statement-or-indent'
;;     . `seed7--delete-backward'
;;     o `seed7-indent-line'
;;     . `seed7-insert-if-statement'
;;     . `seed7-insert-if-else-statement'
;;     . `seed7-insert-if-elsif-statement'
;;     . `seed7-insert-if-elsif-else-statement'
;;     . `seed7-insert-case-statement'
;;     . `seed7-insert-for'
;;     . `seed7-insert-for-until'
;;     . `seed7-insert-for-step'
;;     . `seed7-insert-for-each'
;;     . `seed7-insert-for-each-until'
;;     . `seed7-insert-for-each-key'
;;     . `seed7-insert-for-each-key-until'
;;     . `seed7-insert-for-key'
;;     . `seed7-insert-for-key-until'
;;     . `seed7-insert-repeat'
;;     . `seed7-insert-while'
;;     . `seed7-insert-block'
;;     . `seed7-insert-global'
;;     . `seed7-insert-procedure-declaration'
;;     . `seed7-insert-func-declaration'
;;     . `seed7-insert-short-function-declaration'
;;     . `seed7-insert-var-declaration'
;;     . `seed7-insert-const-declaration'
;;     . `seed7-insert-in-parameter'
;;     . `seed7-insert-invar-parameter'
;;     . `seed7-insert-inout-parameter'
;;     . `seed7-insert-reference-parameter'
;;     . `seed7-insert-value-parameter'
;;     . `seed7-insert-call-by-name-parameter'
;;     . `seed7-insert-include'
;;     . `seed7-insert-enumeration-type-declaration'
;;     . `seed7-insert-struct-type-declaration'
;;       . `seed7--delete-char-and-mark-at'
;;         . `seed7--delete-char-and-mark-at-column'
;;           . `seed7--delete-char-and-mark'
;;       . `seed7--indent-lines'
;; - Seed7 Compilation
;;   * `seed7-compile'
;; - Seed7 Cross Reference
;;    > `seed7--xref-backend'
;;    + `xref-backend-identifier-at-point'
;;      . `seed7-symbol-at-point'
;;        . `seed7-operator-at-point'
;;    + `xref-backend-definitions'
;;      . `seed7--find-symbol'
;;        . `seed7--xref-get'
;;          . `seed7--find-info-about'
;;          . `seed7--xref-get-from-s7xref'
;;            . `seed7--build-xref'
;;            . `seed7--xref-in-list'
;;            . `seed7--signature-from'
;;              . `seed7--signature-at'
;;        . `seed7--make-xref-from-file-loc'
;; - Seed7 Completion Support
;; - Seed7 Abbreviation Support
;; - Seed7 Key Map
;; - Seed7 Menu
;; - Seed7 Major Mode
;;   * `seed7-mode'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'simple)         ; use: `move-beginning-of-line'
(require 'speedbar)       ; use: `speedbar-add-supported-extension'
(require 'subr-x)         ; use: `string-trim'
(require 'easymenu)       ; use: `easy-menu-define'
(require 'tempo)          ; use: `tempo-forward-mark', `tempo-backward-mark'
(require 'imenu)          ; use: `imenu--menubar-select', `imenu--rescan-item'
;;                        ;      `imenu-update-menubar',
;;                        ;      `imenu-generic-expression'
(require 'xref)           ; use: `xref-make', 'xref-make-file-location'

;;; --------------------------------------------------------------------------
;;; Code:
;;
;; References:
;; - Seed7 formal specification:  "Seed7 Reference Manual".
;; -   See http://seed7.sourceforge.net/

;; Seed7 Syntax Information:
;; - https://thomasmertes.github.io/Seed7Home/faq.htm#add_syntax_highlighting

;; ---------------------------------------------------------------------------
;;* Version Info
;;  ============

(defconst seed7-mode-version-timestamp "2025-07-05T11:45:22+0000 W27-6"
  "Version UTC timestamp of the seed7-mode file.
Automatically updated when saved during development.
Please do not modify.")

(defun seed7-mode-version ()
  "Print `seed7-mode' version UTC time stamp."
  (interactive)
  (message "seed7-mode version UT timestamp: %s" seed7-mode-version-timestamp))

;; ---------------------------------------------------------------------------
;;* Seed7 Customization
;;  ===================

(defun seed7-mode-customize ()
  "Open the `seed7-mode' customization buffer."
  (interactive)
  (customize-group "seed7"))

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

(defcustom seed7-menu-list-functions-and-procedures-together  t
  "When on, list function and procedures together, otherwise separately.

This affects the way the callable are displayed in imenu commands,
in the top menu and inside the Speedbar."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

(defcustom seed7-menu-list-functions-sorted t
  "When on, list menu entries in sorted order, otherwise in code order.

This affects the way the callable are displayed in imenu commands,
in the top menu and inside the Speedbar."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Code Template Expansion

(defcustom seed7-template-expansion-disables-overwrite-mode t
  "When on, `overwrite-mode' is forced off when code template is expanded.

When `seed7-complete-statement-or-indent' performs code expansion and
`seed7-template-expansion-disables-overwrite-mode' is on, it forces
`overwrite-mode' off in the current buffer to prevent writing over the
expanded code.

To disable this behaviour turn this user-option off."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Code Navigation

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

;;** Seed7 Cross Reference

(defcustom seed7-xref "s7xref"
  "Seed7 cross reference builder command line.

The command line must identify the Seed7 cross reference builder,
s7xref, by default.

You may type:
- the cross reference builder executable program to use, or
- the s7 interpreter, followed by the Seed7 source file to use.

The name of the cross reference executable or the s7 Seed interpreter program
must include their absolute path unless these programs can be found through
the PATH environment variable accessible to Emacs.

The ~ character is expanded.

The seed7 mode repository includes the s7xref.sd7 file inside the tools
sub-directory. You can either create an executable for it or use the s7
interpreter to run it without having to compile it."
  :group 'seed7
  :type 'string)


;;** Seed7 Faces
(defgroup seed7-faces nil
  "Fontification colors."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'seed7)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;* Seed7 Keyword Regexp
;;  ====================
;;
;; First define private list of keyword strings (some may be combination of
;; Seed7 keywords) inside a constant.  Then use the powerful `rx-to-string'
;; macro to create a regexp for this group.
;;
;; In the future, the private lists of keywords may be dynamically loaded for
;; specific Seed7 syntax and the constants will become variables to allow
;; the mode to dynamically adapt to the Seed7 extended systax.
;;
;; See: https://seed7.net/faq.htm#add_syntax_highlighting

;;** Seed7 Tokens
;;   ------------
;;
;; Ref: https://seed7.sourceforge.net/manual/tokens.htm

;;*** Seed7 Comments Control

(defconst seed7--line-comment-re
  "[^[:digit:]]\\(#.*\\)$"
  "Single line comment in group 1.")

;;*** Invalid String Literals

(defconst seed7--invalid-char-literal-re
  "[^'\\]\\(\\(?:''\\)\\|\\(?:'[^\\].+?'\\)\\|\\('\\\\[[:digit:]]+'\\)\\)[^']"
  "Match invalid single quote char literal.  In group 1." )

;;*** Seed7 Basic Element Regexp

(defconst seed7--blank-re
  "[[:blank:]]"
  "Match any horizontal white space character")

(defconst seed7--whitespace-re
  "[[:blank:]
]"
  "Match any horizontal whitespace character and new line.")

(defconst seed7--anychar-re
  "[^\\0]"
  "Match any character including new-line.")

(defconst seed7--any-wp-text-re
  (format "\\(?:%s+?.+?\\)+?"
          seed7--whitespace-re)
  "Any sequence of whitespace followed by non-whitespace.
Inside a non-capturing group.")

(defconst seed7--bracket-re
  "[])(}{[]")

(defconst seed7--non-capturing-name-identifier-re
  "\\(?:[[:alpha:]_][[:alnum:]_]*\\)"
  "A complete, valid name identifier.  No capturing group.")

(defconst seed7-name-identifier-re
  (format "\\(%s\\)" seed7--non-capturing-name-identifier-re)
  "A complete, valid name identifier.  One capturing group.")

(defconst seed7--non-capturing-type-identifier-re
  (format "\\(?:%s\\(?:[[:blank:]]+?%s\\)??\\)"
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-name-identifier-re)
  "A complete, valid type identifier name with one or 2 identifiers.
Has no capturing group.")


;; (defconst seed7--syntax-spec-identifier-re
;;   "\\(\\.\\(?:[[:alpha:]][[:alnum:]]*\\)\\|\\(?:[[:alpha:]][[:alnum:]]*\\)\\.\\)"
;;   "Seed7 syntax spec keyword (surrounded by period). In group 1")

(defconst seed7-type-identifier-re
  (format "\\(%s\\(?:[[:blank:]]+?%s\\)??\\)"
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-name-identifier-re)
  "A complete, valid type identifier name with one or 2 identifiers.
Has only one capturing group.")

(defconst seed7--special-char-re
  "[-!$%&*+,\\./:;<=>?@\\^`|~]"
  "Any one of the special characters.")

(defconst seed7--non-capturing-special-identifier-re
  (format "\\(?:%s+\\)" seed7--special-char-re)
  "A complete, valid Seed7 special identifier.  Non capturing.")

(defconst seed7-special-identifier-re
  (format "\\(%s\\)" seed7--non-capturing-special-identifier-re)
  "A complete, valid Seed7 special identifier.  One capturing group.")

(defconst seed7--non-capturing-any-identifier-re
  (format "\\(?:\\(%s\\|%s\\)\\)"
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-special-identifier-re)
  "A name or special identifier. 1 capturing group")

(defconst seed7--any-identifier-re
  (format "\\(\\(%s\\|%s\\)\\)"
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-special-identifier-re)
  "A name or special identifier. 1 capturing group")

;; --

(defconst seed7-integer-invalid-0x-re "\\(0x[[:digit:]]+\\)"
  "Seed7 integer in group 1.")

(defconst seed7-integer-re "\\([[:digit:]]+\\)"
  "Seed7 integer in group 1.")

(defconst seed7--opt-square-brace-start-re
  (format "\\(?:%s+?\\[\\)"
          seed7--whitespace-re))

(defconst seed7--opt-square-brace-end-re
  (format "\\(?:%s+?]%s+?\\)"
          seed7--whitespace-re
          seed7--whitespace-re))

;;*** Seed7 Float Literals

(defconst seed7-float-number-invalid1-re
  "[^[:alnum:]]\\(\\.[[:digit:]]+\\)"
  "Invalid Seed7 float number in group 1.")
(defconst seed7-float-number-invalid2-re
  "[^[:alnum:]]\\([[:digit:]]+\\.\\)[^[:alnum:]]"
  "Invalid Seed7 float number in group 1.")

(defconst seed7-float-number-re
  "[0-9]+\\.[0-9]+\\(?:\\(?:[eE][+-]?\\)?[0-9]+\\)?"
  "Seed7 float number in group 0.")


;;*** Seed7 Numbers with Exponents

(defconst seed7-number-with-negative-exponent-re
  "[0-9]+[eE]-[0-9]+"
  "Literal number with negative exponent.  Invalid in Seed7.")

(defconst seed7-number-with-exponent-re
  "[0-9]+[eE]\\+?[0-9]+"
  "Literal number with exponent.")

;;*** Seed7 BigInteger Literals
;;
;; Ref: https://seed7.sourceforge.net/manual/tokens.htm#BigInteger_literals
;;
(defconst seed7-big-number-re
  "\\(\\(?:\\(?:\\([2-9]\\|1[0-9]\\|2[0-9]\\|3[0-6]\\)#\\)?[0-9]+_\\)\\)"
  ;; 1            2
  ;; Group 1: Complete Big Number with or without base. "1_" or "1234322_" or "2#0001_", etc...
  ;; Group 2: base: "2" to "36".  nil if no base.
  "Big number with/without base.  Group 1: number, group 2: base or nil.")


;;*** Seed7 Integer Literals
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

(defconst seed7-any-valid-char-integer-semicolon-re
  (format "'\\\\%s;'\\)\\|\\(?:'\\\\[[:digit:]]+;'"
          (format seed7--base-x-integer-re-format
                  "(?:"
                  "")
          ))

(defconst seed7-base-x-big-number-re (format seed7--base-x-integer-re-format
                                             "(\\(?:"
                                             "_\\)[^#0-9a-zA-z]"))

;;** Seed7 Pragmas
;;   -------------
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

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-pragma-keywords-regexp
  (format "^%s\\(\\$ +%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--pragma-keywords)))
          "\\>"))

;;** Seed7 include
;;   -------------
;;
;; The very first include statement requires a leading '$' but not
;; the following ones."
(defconst seed7-include-regexp
  "^\\(\\$? *\\(?:include\\)\\) ")


;;** Seed7 keywords used in statements
;;   ---------------------------------
;;
;; All keywords a re listed here, but some are commented out because
;; they are part of another list below.  The ones left are the ones that
;; are at the beginning of a line (with or without leading white space),
;; identified in `seed7--lead-in-statement-keywords' and some that can also
;; be in the middle or end of line, which are identified by
;; `seed7--in-statement-keywords'.

(defconst seed7--lead-in-statement-keywords
  '(
    "raise"                      ; currently missing in the Seed7 keyword list
    "return"
    ))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-lead-in-statement-keywords-regexp
  (format "^ *%s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--lead-in-statement-keywords)))
          "\\>"))

(defconst seed7--in-statement-keywords
  '("is"
    "noop" ; not mentioned in operators but not an identifier, probably a special case
    ))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-in-statement-keywords-regexp
  (format ". %s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--in-statement-keywords)))
          "\\>"))


;;** Seed7 is-statemement keywords
;;   -----------------------------
;;
;; These keywords are exclusively used following the 'is' keyword.
;;
(defconst seed7-is-statement-keywords
  '(
    "forward"
    "DYNAMIC"
    "new"
    "sub"))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7--is-statement-keywords-regexp
  (format " is%s+\\<\\(%s\\)\\>"
          seed7--whitespace-re
          (rx-to-string
           `(: (or ,@seed7-is-statement-keywords)))))

;;** Seed7 keywords used in middle of statements
;;   -------------------------------------------
;;
;; The name identifiers inside constructs are:
;;  - begin
;;  - default
;;  - do
;;  - downto
;;  - exception
;;  - fixLen
;;  - key
;;  - len
;;  - of
;;  - otherwise
;;  - range
;;  - step
;;  - then
;;  - to
;;  - until

(defconst seed7--in-middle-statement-keywords
  '("begin"
    "default"
    "do"
    "downto"
    "exception"
    "fixLen"
    "key"
    "len"
    "local"                    ; not inside constructs but section start
    "of"                       ; also in `seed7--statement-enclosing-keywords'
    "otherwise"                ; also in `seed7--statement-enclosing-keywords'
    "param"                    ; used as nameless parameter.  Will be removed.
    "range"
    "result"                   ; not inside construct but section start
    "step"
    "then"
    "to"
    "until"                    ; also in `seed7--statement-enclosing-keywords'
    ))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:space:]]\\<"
          (rx-to-string
           `(: (or ,@seed7--in-middle-statement-keywords)))
          "\\>"))


;;** Seed7 statement enclosing keywords
;;   ----------------------------------
;;

(defconst seed7--block-start-keywords
  '("block"                             ; "end block"
    "case"                              ; "end case"
    "enum"
    "for"
    "global"
    ;; "func"
    "if"                                ; "elsif"      "end if"
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
    "global"      "end global"
    "func"  "varfunc" "proc" "end func"
    "if"          "else" "elsif" "end if"
    "repeat"      "until"
    "struct"      "end struct"
    "while"       "end while"))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-statement-enclosing-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--statement-enclosing-keywords)))
          "\\>"))

;;** Seed7 declaration introduction keywords
;;   ---------------------------------------
;;
;; Ref: Parameters: https://seed7.net/manual/params.htm

(defconst seed7--declaration-intro-keywords
  '("attr"
    "const"
    "in"
    "inout"
    "ref"
    "val"
    "var"))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-declaration-intro-keywords-regexp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(:  (or ,@seed7--declaration-intro-keywords)))
          "\\>")
  "Argument declaration intro keyword: group 1")

(defconst seed7-non-capturing-declaration-intro-keywords-regexp
  (format "%s\\(?:%s\\)%s"
          "\\<"
          (rx-to-string
           `(:  (or ,@seed7--declaration-intro-keywords)))
          "\\>")
  "Argument declaration intro keyword: no capturing group.")


;;** Seed7 Predefined Types
;;   ----------------------
;;
;; Ref: Abstract data type: https://seed7.net/manual/decls.htm#Abstract_data_types

(defconst seed7--predefined-types
  '("array"                             ; abstract data type
    "subtype"                           ; abstract data type
    "subrange"                          ; abstract data type
    "interface"                         ; abstract data type
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
    "enum"                              ; abstract data type
    "expr"
    "file"
    "fileSys"
    "float"
    "func"
    "hash"                              ; abstract data type
    "integer"
    "object"
    "proc"
    "process"
    "program"
    "rational"
    "reference"
    "ref_list"
    "set"                               ; abstract data type
    "sqlStatement"
    "string"
    "struct"                            ; abstract data type
    "text"
    "time"
    "type"
    "void"
    "PRIMITIVE_WINDOW"))

;; Note: the < > are important to prevent detection of words inside other
;; words.  The predefined types are not set to have symbol syntax, therefore
;; they must be treated as words and use word boundary.
(defconst seed7-predefined-types-regexp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--predefined-types)))
          "\\>"))

;;** Seed7 Predefined Constants
;;   --------------------------

(defconst seed7--predefined-constants
  '("E"
    "EOF"
    "FALSE"
    "Infinity"
    "NIL"
    "NaN"
    "PI"
    "TRUE"
    "empty"))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-predefined-constants-regxp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--predefined-constants)))
          "\\>"))

;;** Seed7 Predefined Variables
;;   --------------------------

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

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-predefined-variables-regxp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--predefined-variables)))
          "\\>"))

;;** Seed7 Predefined errinfo value
;;   ------------------------------
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

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-errinfo-values-regxp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--errinfo-values)))
          "\\>"))

;;** Seed7 Operator Symbols
;;   ----------------------

;; [:todo 2025-04-10, by Pierre Rouleau: categorize 'noop' according to Seed7 spec once I find it ]
(defconst seed7--operator-symbols
  '("and"
    "conv"
    "digits"
    "div"
    "exp"
    "in"
    "lpad"
    "lpad0"
    "mdiv"
    "mod"
    "mult"
    "not"
    "or"
    "parse"
    "radix"
    "RADIX"
    "rem"
    "rpad"
    "sci"
    "times"
    "varConv"))

;; Note: the < > are important to prevent detection of words inside other words.
(defconst seed7-operator-symbols-regexp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(:  (or ,@seed7--operator-symbols)))
          "\\>")
  "Seed7 word operator symbols.  Captured in group 1.")

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;** Seed7 Predefined Assignment Operators
;;   -------------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Predefined assignment operators are: := +:= -:= *:= /:= <<:= >>:= &:= |:= ><:= @:=
;;
;;    :=
;;   +:=   https://seed7.sourceforge.net/libraries/integer.htm#(inout_integer)+:=(in_integer)
;;   -:=   https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)-:=(in_bigInteger)
;;   *:=   https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)*:=(in_bigInteger)
;;   /:=   https://seed7.sourceforge.net/libraries/bigrat.htm#(inout_bigRational)/:=(in_bigRational)
;;  <<:=   https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3C%3C:=(in_integer)
;;  >>:=   https://seed7.sourceforge.net/libraries/bigint.htm#(inout_bigInteger)%3E%3E:=(in_integer)
;;   &:=   https://seed7.sourceforge.net/libraries/array.htm#(inout_arrayType)&:=(in_arrayType)
;;   |:=   https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)|:=(in_bin32)
;;  ><:=   https://seed7.sourceforge.net/libraries/bin32.htm#(inout_bin32)%3E%3C:=(in_bin32)
;;   @:=   https://seed7.sourceforge.net/libraries/bitset.htm#(inout_bitset)@:=_[(in_integer)](in_boolean)

(defconst seed7--assignment-operator-symbols
  '(
    ":="
    "::="      ; see https://github.com/ThomasMertes/seed7/issues/64
    "+:="
    "-:="
    "*:="
    "/:="
    "<<:="
    ">>:="
    "&:="
    "|:="
    "><:="
    "@:="))

(defconst seed7-predef-assignment-operator-regxp
  (rx-to-string
   `(:  (or ,@seed7--assignment-operator-symbols)))
  "Seed7 assignment operator symbols.  Captured in Group 0.")


;;** Seed7 Predefined Comparison Operators
;;   -------------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Predefined comparison operators are: = <> < <= > >=

(defconst seed7-predef-comparison-operator-regxp
  "\\(?:[=><]\\|\\(?:<>\\|<=\\|>=\\)\\)"
  "Symbol is in group 0.")


;;** Seed7 Other Predefined Operators
;;   --------------------------------
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


;;** Seed7 Block Processing Regular Expressions
;;   ------------------------------------------

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
\\<local\\>\\|\
\\<repeat\\>\\|\
\\<global\\>\\|\
\\<begin\\>\\|\
\\<block\\>\\|\
\\<else\\>\\|\
\\<exception\\>\\|\
\\<result\\>\\)"
  "Regexp for the beginning of a Seed7 block.  One capture group.")

(defconst seed7-block-line-start-regexp (concat
                                         "^[[:blank:]]*?"
                                         seed7-block-start-regexp)
  "Regexp to find location of blocks.")

(defconst seed7-block-end-regexp "\
\\(?:end \
\\(?:\\(?:\\(?:enum\\|for\\|func\\|if\\|struct\\|while\\|case\\);\\)\
\\|block\\)\\)\
\\|\\(?:until \\)"
  "Regexp for generic end of block.")

;; [:todo 2025-07-01, by Pierre Rouleau: optimize these 2 regexps]
(defconst seed7-block-top-start-regexp "\\(\
const proc: \\|\
const func \\|\
const type: \\|\
const array \\|\
var array \\|\
const set \\|\
var set \\|\
elsif \\|\
if \\|\
while \\|\
for \\|\
case \\|\
catch \\|\
\\<local\\>\\|\
\\<repeat\\>\\|\
\\<global\\>\\|\
\\<begin\\>\\|\
\\<block\\>\\|\
\\<else\\>\\|\
\\<exception\\>\\|\
\\<result\\>\\)"
  "Regexp for the top of a Seed7 block.  One capture group.")

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
    (modify-syntax-entry ?_ "w"   st)   ; underscores are allowed in
                                        ; identifiers, which have word syntax.
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
    ;; single quote: Seed7 supports ''' as well as '\''.  Deal with it in seed7-syntax-propertize.
    (modify-syntax-entry ?\' "." st) ; attribute; see seed7-syntax-propertize for character literal
    st)
  "Syntax table in use in `seed7-mode' buffers.")

;;** Seed7 Mode Syntax Propertize Function
;;   -------------------------------------

(defconst seed7-char-literal-re
  (format
   "\\(?:\\(?:[[:digit:]]\\(#\\)[[:alnum:]]\\)\\|\\(\\(?:'\\\\''\\)\\|\\(?:'.'\\)\\|\\(?:'\\\\[abefnrtv\"A-Z\\\\]'\\)\\|\\(?:%s\\)\\)\\)"
   ;; (----(----------------------------------)----(--------------------------------------------------------------------------------)--)
   ;;                      (---)                   (-----------------------------------------------------------------------------------)
   ;;                                                 (-----------)     (-------)     (-----------------------------)     (------)
   ;;                      G1                      G2                                                                        %1
   seed7-any-valid-char-integer-semicolon-re))

(defun seed7-mode-syntax-propertize (start end)
  "Apply syntax property between START and END to # character in number."
  ;; (info "(elisp)Syntax Properties")
  ;;
  ;; called from `syntax-propertize', inside save-excursion with-silent-modifications
  (goto-char start)
  (while (re-search-forward seed7-char-literal-re end t)
    (cond
     ;; deal with '#'
     ((match-beginning 1)
      (let
          ((mb (match-beginning 1)) (me (match-end 1))
           (syntax (string-to-syntax "_")))
        (if syntax (put-text-property mb me 'syntax-table syntax))))

     ;; Deal with single quoted character expression
     ((match-beginning 2)
      (put-text-property  (match-beginning 2) (1+ (match-beginning 2)) 'syntax-table '(7 . ?'))
      (put-text-property  (1- (match-end 2))  (match-end 2)            'syntax-table '(7 . ?'))))))


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
(defface seed7-name-identifier-face
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

;; (defface seed7-spec-name-identifier-face
;;   `((((class grayscale) (background light))
;;      (:background "Gray90" :weight bold))
;;     (((class grayscale) (background dark))
;;      (:foreground "Gray80" :weight bold))
;;
;;     (((class color) (background light))
;;      (:foreground "blue3"  :weight bold))
;;     (((class color) (background dark))
;;      (:foreground "blue3" :background ,seed7-dark-background
;;                   :weight bold))
;;
;;     (t (:weight bold)))
;;   "Font Lock mode face that highlights errinfo values."
;;   :group 'seed7-faces)

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
   ;; (cons seed7--syntax-spec-identifier-re            (list 1 ''seed7-spec-name-identifier-face))
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
   ;; Numbers: order is significant
   ;; float and integers with exponents numbers (have a 'E' or 'e' embedded.
   ;; They must be rendered before predefined-constants.
   (cons seed7-float-number-invalid1-re              (list 1 ''font-lock-warning-face))
   (cons seed7-float-number-invalid2-re              (list 1 ''font-lock-warning-face))
   (cons seed7-float-number-re                       (list 0 ''seed7-float-face))
   ;; numbers: order is significant : base-x numbers use '#' and can have a 'e'
   ;; therefore , those must be checked before numbers with exponent (that can also use a 'e')
   (cons seed7-base-x-big-number-re                  (list 1 ''seed7-number-face))
   (cons seed7-base-x-integer-re                     (list 1 ''seed7-integer-face))
   (cons seed7-number-with-negative-exponent-re      (list 0 ''font-lock-warning-face))
   (cons seed7-number-with-exponent-re               (list 0 ''seed7-integer-face))
   (cons seed7-integer-invalid-0x-re                 (list 1 ''font-lock-warning-face))
   ;; predefined constants (includes 'E'). Must be rendered after float numbers.
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

   ;; invalid single quote char literals
   (cons seed7--invalid-char-literal-re              (list 1 ''font-lock-warning-face))

   ;; big numbers: have an underscore : must be done before seed7-name-identifier-re
   (cons seed7-big-number-re                         (list 1 ''seed7-number-face))

   ;; identifiers: include the underscore
   (cons seed7-name-identifier-re                    (list 1 ''seed7-name-identifier-face))
   ;; other numbers
   (cons seed7-integer-re                            (list 1 ''seed7-integer-face))
   ;; low priority rendering of arithmetic + and -
   (cons seed7-minus-operator-regexp                 (list 1 ''font-lock-keyword-face))


   )
  "Associates regexp to a regexp group and a face to render it.")


;; ---------------------------------------------------------------------------
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
;;* Seed7 iMenu Support Regexp
;;  ==========================

(defconst seed7--procedure-forward-or-action-re
  (format "forward;\\|DYNAMIC;\\|action%s+?\\\".+?\\\";"
          seed7--whitespace-re)
  "Regexp matching forward or action declaration. No capture group.")

;; functions declarations support all procedure have plus ability to complete
;;  with a 'is value;' syntax like 'is 16;'
(defconst seed7--function-value-forward-or-action-declaration-re
  (format "[^;]+?;\\|%s"
          seed7--procedure-forward-or-action-re)
  "Regexp matching value, forward or action declaration. No capture group.")

;; --
;; Regexp for procedure and function declarations. No matching group.

(defconst seed7-forward-or-action-procedure-declaration-re
  (format
   "const proc\\(?:%s\\)+?is%s+?\\(?:%s\\)"
   "[^;]"
   seed7--whitespace-re
   seed7--procedure-forward-or-action-re)
  "Regexp matching forward or action procedure declaration. No capture group.")

(defconst seed7-forward-or-action-function-declaration-re
  ;;                             (--------------)     (-----------)
  ;;                        (----------------------------------------)
  ;;(--------------------------------------------------------------------)
  (format "\\(const \\(?:var\\)?func%s+?%s:%s+?%s+?is%s+?\\(?:%s\\)\\)"
          ;;                        %   %  %  %     %        %
          ;;                        1   2  3  4     5        6
          seed7--whitespace-re                    ; 1
          seed7--non-capturing-name-identifier-re ; 2
          "[^;]"                                  ; 3  any char but semi-colon
          seed7--whitespace-re                    ; 4
          seed7--whitespace-re                    ; 5
          seed7--function-value-forward-or-action-declaration-re) ; 6
  "Regexp matching value, forward or action function declaration.
No matching group.")


;; --
;; Regexp for procedure and function declarations or beginning of block.

(defconst seed7-procedure-regexp
  (format
   "^[[:blank:]]*const%s+proc:%s\\(%s\\)%s*?is%s+?\\(func\\|%s\\)"
   ;;                             G1
   ;;                 %       %   %     %     %             %
   ;;                 1       2   3     4     5             6
   seed7--whitespace-re                    ; 1
   seed7--whitespace-re                    ; 2
   seed7--non-capturing-name-identifier-re ; 3
   seed7--anychar-re                       ; 4
   seed7--whitespace-re                    ; 5
   seed7--procedure-forward-or-action-re)  ; 6
  "Match procedure name in group 1.")

(defconst seed7-function-regexp
  (format
   ;;             const   func T       :                                                                     is      func
   ;;                              w    w[      w                                                      .   w]w w
   "^[[:blank:]]*?const%s+\\(?:var\\)?func %s??%s??:%s?\\(?:%s+?\\(?:(%s+?)\\)\\)?%s*\\(%s\\|%s\\)\\(?:%s(%s+?)\\)?%s*?%s?is%s+\\(func\\|return\\|%s\\)"
   ;;                  %                   G2  %    %        %         %           %   G3%    %         %  %       %   %    %    G4               %
   ;;                  1                   %2  3    4        5         6           7     8    9         10 11      12  13   14                    15
   ;;
   seed7--whitespace-re                                   ; 1
   seed7-type-identifier-re                               ; 2
   seed7--whitespace-re                                   ; 3
   seed7--opt-square-brace-start-re                       ; 4 w[
   seed7--whitespace-re                                   ; 5
   seed7--anychar-re                                      ; 6
   seed7--whitespace-re                                   ; 7
   seed7--non-capturing-name-identifier-re                ; 8
   seed7--non-capturing-special-identifier-re             ; 9
   seed7--whitespace-re                                   ; 10
   seed7--anychar-re                                      ; 11
   seed7--anychar-re                                      ; 12
   seed7--opt-square-brace-end-re                         ; 13 w]w
   seed7--whitespace-re                                   ; 14
   seed7--function-value-forward-or-action-declaration-re) ; 15
  "Regexp identifying beginning of procedures and functions.
Group 1: The function return type.
Group 2: The function name.
Group 3: - \"func\" for proc or function that ends with \"end func\".
         - \"return\" for a func that only has a return statement.
         - \"forward\" for a forward declaration.
         - \"action ACTION\" for an action function." )

;; --

(defconst seed7-enum-regexp
  "const type: \\([[:alpha:]][[:alnum:]_]+\\) is new enum")

(defconst seed7-interface-regexp
  (format "const%stype:%s\\(%s\\)%sis%s\\(?:new\\|sub%s%s\\)%sinterface;"
          ;;    %      %    %    %   %             % %    %
          ;;    1      2    3    4   5             6 7    8
          seed7--whitespace-re                    ; 1
          seed7--whitespace-re                    ; 2
          seed7--non-capturing-name-identifier-re ; 3
          seed7--whitespace-re                    ; 4
          seed7--whitespace-re                    ; 5
          seed7--whitespace-re                    ; 6
          seed7--non-capturing-name-identifier-re ; 7
          seed7--whitespace-re)                   ; 8
  "Regexp to extract interface type declaration. Group 1: name of type.")

(defconst seed7-struct-regexp
  (format
   "const type: %s%s+?is%s+?\\(?:sub\\|new\\)%s+?\\(?:%s%s+?\\)??struct"
   ;;           G1
   ;;           % %     %                    %        % %
   ;;           1 2     3                    4        5 6
   seed7-name-identifier-re    ; 1
   seed7--whitespace-re        ; 2
   seed7--whitespace-re        ; 3
   seed7--whitespace-re        ; 4
   seed7-name-identifier-re    ; 5
   seed7--whitespace-re))      ; 6


;; ---------------------------------------------------------------------------
;;* Seed7 Speedbar Support
;;  ======================
;;
;; Seed7 files: programs:          `.sd7`
;;              library/interface: `.s7i`

(speedbar-add-supported-extension "\\.s\\(d7\\|7i\\)\\'")

;; ---------------------------------------------------------------------------
;;* Seed7 Low-level Macros
;;  ======================

(defmacro seed7--set (fct var)
  "Set VAR with result of FCT call and return it.
Use inside a `cond' clause to emphasize the check FCT."
  `(setq ,var ,fct))

;; ---------------------------------------------------------------------------
;;* Seed7 Code Navigation
;;  =====================

;;** Seed7 Comment and String Identification Macros and Functions
;;   ------------------------------------------------------------

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

;; [:todo 2025-05-21, by Pierre Rouleau: clarify identifying comments]
(defun seed7-inside-comment-p (&optional pos)
  "Return face of comment if POS or point is inside comment, nil otherwise.
Inside a comment, the returned value is:
- `font-lock-comment-face'           : inside comment block or en-line comment
- `font-lock-comment-delimiter-face' : at the # for line-end comment."
  ;; Using the face instead of the syntax, as I found the syntax
  ;; not reliable enough when looking at some edge cases: the open block
  ;; comment characters are not recognized as comment syntax.
  (let ((pos (or pos (point))))
    ;; deal with comment-dwim that passes 0 to pos when trying to write a line
    ;; comment when issued at the beginning of an empty line.
    (unless (eq pos 0)
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
                         'font-lock-comment-delimiter-face))))))))

(defun seed7-inside-string-p (&optional pos)
  "Return non-nil if POS or point is inside a string, nil otherwise.
Note that the leading quote character does not register as inside a string."
  (save-match-data
    (let* ((pos (or pos (point)))
           (syntax (syntax-ppss pos)))
      (seed7--inside-string-p syntax))))


;;** Seed7 Code Search Functions
;;   ---------------------------

(defun seed7-re-search-forward (regexp &optional bound)
  "Search for REGEXP inside code.  Skip comment and strings.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
Return found position or nil if nothing found.
Move point."
  (let ((found-pos nil)
        (keep-searching t)
        ;; prevent case fold searching: Seed7 is case sensitive.
        (case-fold-search nil))
    (while (and keep-searching
                (not (eobp)))
      (if (re-search-forward regexp bound :noerror)
          (if (or (seed7-inside-comment-p)
                  (seed7-inside-string-p))
              ;; found in comment or string.  Skip and keep searching
              (forward-char (length
                             (substring-no-properties (match-string 0))))
            ;; Found in code!
            (setq found-pos (point)
                  keep-searching nil))
        ;; Not found. stop.
        (setq keep-searching nil)))
    found-pos))

(defun seed7-re-search-forward-closest (regexps)
  "Search for all specified regexp in REGEXPS and stop at the closest found.
Return position of the closest found, nil if nothing found."
  (let* ((positions nil)
         (closest-position
          (dolist (regexp regexps (car-safe
                                   (sort
                                    (seq-filter #'identity positions))))
            (save-excursion
              (push (seed7-re-search-forward regexp) positions)))))
    (when closest-position
      (goto-char closest-position))))

(defun seed7-re-search-backward (regexp &optional bound)
  "Search for REGEXP inside code.  Skip comment and strings.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
Return found position or nil if nothing found.
Move point."
  (let ((found-pos nil)
        (keep-searching t)
        ;; prevent case fold searching: Seed7 is case sensitive.
        (case-fold-search nil))
    (while (and keep-searching
                (not (bobp)))
      (if (re-search-backward regexp bound :noerror)
          (if (save-excursion
                (forward-char) ; Protection. Prevent code following line-end comment as comment.
                ;;             ; This should not be necessary because the first char
                ;;             ; of a line will not be a comment or string, but it seems
                ;;             ; that the check invalidly reports code as comment when
                ;;             ; code is executed inside a keyboard macro that first inserts
                ;;             ; a space before a indented keyword that follows a line-end comment.
                ;;             ; The problem does not seem to happen under normal typing.
                ;;             ; This is very strange.  Perhaps I don't understand what is
                ;;             ; really happening here but tracing through this indicated that the
                ;;             ; forward-char solves the issue when it occurs.
                ;; [:todo 2025-06-04, by Pierre Rouleau: investigate: a bug in Emacs? Or in my code? ]
                (or (seed7-inside-comment-p)
                    (seed7-inside-string-p)))
              ;; found in comment or string.  Skip and keep searching
              (backward-char (length
                              (substring-no-properties (match-string 0))))
            ;; Found in code!
            (setq found-pos (point))
            (setq keep-searching nil))
        ;; Not found. stop.
        (setq keep-searching nil)))
    found-pos))

(defun seed7-re-search-backward-closest (regexps)
  "Search for all specified regexp in REGEXPS and stop at the closest found.
Return position of the closest found, nil if nothing found."
  (let* ((positions nil)
         (closest-position
          (dolist (regexp regexps (car-safe
                                   (nreverse
                                    (sort
                                     (seq-filter #'identity positions)))))
            (save-excursion
              (push (seed7-re-search-backward regexp) positions)))))
    (when closest-position
      (goto-char closest-position))))

;;** Seed7 Procedure/Function Regular Expressions
;;   --------------------------------------------

(defconst seed7-one-arg-re
  ;;         (-------------------------------------------------------------------)
  ;;                      in       type1      type   :   id             attr attribute
  ;;              (    (          (----------)          )         )  |  (--------------)
  (format "\\(?:\\(?:\\(?:%s%s+?\\(?:%s%s+?\\)?%s%s*?:\\)?%s+?%s\\)\\|\\(?:attr%s+?%s\\)\\)"
          ;;              % %        % %       % %        %   %                %   %
          ;;              1 2        3 4       5 6        7   8                9   10
          seed7-non-capturing-declaration-intro-keywords-regexp ; 1 in/out/inout...
          seed7--whitespace-re                                  ; 2
          seed7--non-capturing-name-identifier-re               ; 3 type (opt)
          seed7--whitespace-re                                  ; 4
          seed7--non-capturing-name-identifier-re               ; 5 type
          seed7--whitespace-re                                  ; 6
          seed7--whitespace-re                                  ; 7
          seed7--non-capturing-name-identifier-re               ; 8 identifier
          seed7--whitespace-re                                  ; 9
          seed7--non-capturing-name-identifier-re               ; 10 attribute
          )
  "Regexp for one argument declaration.  No capture group.
Matches something like:
  - \"in integer: index\" or,
  - \"attr arrayType\".")

(defconst seed7-args-in-parens-re
  (format "(\\(?:%s*?%s\\(?:%s*?,%s*?%s%s*?\\)*?\\)?)"
          ;;     %   %    %    %   % %
          ;;     1   2    3    4   5 6
          seed7--whitespace-re     ; 1
          seed7-one-arg-re         ; 2
          seed7--whitespace-re     ; 3
          seed7--whitespace-re     ; 4
          seed7-one-arg-re         ; 5
          seed7--whitespace-re)    ; 6
  "Regexp for 0 to many arguments inside parenthesis pair.")

;; --

(defconst seed7-name-argparens-re               ; 1
  (format "\\(%s\\)%s+?%s"
          seed7--non-capturing-name-identifier-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for fctname ( args... ).  Group1 : fctname.")

(defconst seed7-argparens-name-argparens-re     ; 2
  (format "%s%s+?\\(%s\\|%s\\)%s+?%s"
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-special-identifier-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for (args...) fctname (args...). Group1: fctname.")


(defconst seed7-arrparens-name-arrparens-re     ; 3
  (format "\\[%s*?%s%s+?\\(%s\\|%s\\)%s+?%s%s*?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-special-identifier-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for [ (args...) fctname (args...) ]. Group1: fctname.")

(defconst seed7-arg-arrparens-name-arrparens-re ; 4
  (format "%s%s+?\\[%s*?%s%s+?\\(%s\\|%s\\)%s+?%s%s*?]"
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--non-capturing-name-identifier-re
          seed7--non-capturing-special-identifier-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for (args...)  [ (args...) fctname (args...) ]. Group1: fctname.")

(defconst seed7-emptyarr-argparens-re           ; 5
  (format "\\[]%s+?%s"
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for [] (args...)")

(defconst seed7-arrparens-argparens-re          ; 6
  (format "\\[%s+?%s%s+?]%s+?%s"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for [(args...)] (args...)")

(defconst seed7-argparens-arrparens-re          ; 7
  (format "%s+?%s%s+?\\[%s+?%s%s+?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for (args...) [(args...)]")

(defconst seed7-argparens-arrparens-op-re       ; 8
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s%s+?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--non-capturing-special-identifier-re
          seed7--whitespace-re)
  "Regexp for (args...) [(args...) op ]")

(defconst seed7-argparens-op-arrparens-re       ; 9
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s%s+?]"
          ;;%  % %      %   % %   % %
          ;;1  2 3      4   5 6   7 8
          seed7--whitespace-re                       ; 1
          seed7-args-in-parens-re                    ; 2
          seed7--whitespace-re                       ; 3
          seed7--whitespace-re                       ; 4
          seed7--non-capturing-special-identifier-re ; 5
          seed7--whitespace-re                       ; 6
          seed7-args-in-parens-re                    ; 7
          seed7--whitespace-re)                      ; 8
  "Regexp for (args...) [ op (args...) ]")

(defconst seed7-argparens-arrparens-op-arrparens-re ; 10
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s+?%s%s+?%s%s+?]"
          ;;%  % %      %   % %   %   % %   % %
          ;;1  2 3      4   5 6   7   8 9   10 11
          seed7--whitespace-re                       ; 1
          seed7-args-in-parens-re                    ; 2
          seed7--whitespace-re                       ; 3
          seed7--whitespace-re                       ; 4
          seed7-args-in-parens-re                    ; 5
          seed7--whitespace-re                       ; 6
          seed7--whitespace-re                       ; 7
          seed7--non-capturing-any-identifier-re     ; 8 : op
          seed7--whitespace-re                       ; 9
          seed7-args-in-parens-re                    ; 10
          seed7--whitespace-re)                      ; 11
  "Regexp for (args...) [ (args..) op (args...) ]")


;; --

(defconst seed7-any-arg-pattern-re
  (format "\\(?:\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\|\\(?:%s\\)\
\\)"
          seed7-argparens-arrparens-op-arrparens-re ; 10
          seed7-argparens-op-arrparens-re           ; 9
          seed7-argparens-arrparens-op-re           ; 8
          seed7-argparens-arrparens-re              ; 7
          seed7-arrparens-argparens-re              ; 6
          seed7-emptyarr-argparens-re               ; 5
          seed7-arg-arrparens-name-arrparens-re     ; 4
          seed7-arrparens-name-arrparens-re         ; 3
          seed7-argparens-name-argparens-re         ; 2
          seed7-name-argparens-re)                  ; 1
  "Regexp for all possible argument patterns.")


;; --

;; (defconst seed7-arg-name-parens-re
;;   (format "\\(?:%s??%s+?\\)??\\(%s\\|%s\\)%s+?%s"
;;           seed7-args-in-parens-re
;;           seed7--whitespace-re
;;           seed7--non-capturing-name-identifier-re
;;           seed7--non-capturing-special-identifier-re
;;           seed7--whitespace-re
;;           seed7-args-in-parens-re)
;;   "Regexp for name followed by args within parens pair. Group1: function name.")

(defconst seed7-callable-args-regexp
  ""
  "All potential argument forms.")



(defconst seed7-procfunc-regexp
  (format
   ;;    const      varfunc| func | proc       RT?     :                                    fct name                           is     func
   ;;              (--------------------------)                          (----)            (---------)        (----)                  (---------------------------------------------)
   ;;              G1                          G2                     (----------)         G3            (------------)               G4
   ;;                                                        (----------------------)
   ;;                                              w    w[      w                                                      .   w]w w
   "^%s*?const%s+\\(\\(?:var\\)?func \\|proc\\)%s??%s??:%s?\\(?:%s+?\\(?:(%s+?)\\)\\)?%s*\\(%s\\|%s\\)\\(?:%s(%s+?)\\)?%s*?%s?is%s+\\(func\\|return\\|forward;\\|DYNAMIC;\\|action%s\".+?\";\\)"
   ;;%        %    G1                          G2  %    %        %         %           %   G3%    %         %  %       %   %    %    G4                                            %
   ;;1        2                                %3  4    5        6         7           8     9    10        11 12      13  14   15                                                 16
   ;;
   seed7--blank-re                            ; 1
   seed7--whitespace-re                       ; 2
   seed7-type-identifier-re                   ; 3 : RT? : return type (optional)
   seed7--whitespace-re                       ; 4
   seed7--opt-square-brace-start-re           ; 5 w[
   seed7--whitespace-re                       ; 6
   seed7--anychar-re                          ; 7
   seed7--whitespace-re                       ; 8
   seed7--non-capturing-name-identifier-re    ; 9
   seed7--non-capturing-special-identifier-re ; 10
   seed7--whitespace-re                       ; 11
   seed7--anychar-re                          ; 12
   seed7--anychar-re                          ; 13
   seed7--opt-square-brace-end-re             ; 14 w]w
   seed7--whitespace-re                       ; 15
   seed7--whitespace-re)                      ; 16
  "Regexp identifying beginning of procedures and functions.
Group 1: \"proc\", \"varfunc\" or \"func \"
Group 2: The func return type.  May be empty.
Group 3: The func or proc name.
Group 4: - \"func\" for proc or function that ends with \"end func\".
         - \"return\" for a func that only has a return statement.
         - \"forward\" for a forward declaration.
         - \"action ACTION\" for an action function." )

(defconst seed7-procfunc-regexp-item-type-group 1)
(defconst seed7-procfunc-regexp-item-name-group 3)
(defconst seed7-procfunc-regexp-tail-type-group 4)

(defconst seed7-procfunc-end-regexp
  "end[[:blank:]]+func;"
  "Regexp to detect end of procedure or long function.  No group.")

(defconst seed7-short-func-end-regexp
  (format "^[[:blank:]]+?return\\(?:%s+?.+?\\)+?;"
          ;;                        %
          seed7--whitespace-re)
  "Regexp to detect end of short function.  No group.")

(defconst seed7-forward-declaration-end-regexp
  "[[:blank:]]*?is[[:blank:]]+?forward;"
  "Regexp to detect end of forward declaration.  No group.")

(defconst seed7-function-implementation-declaration-end-regexp
  (format "%s+?is%s+?\\(?:\\(?:action%s+?\".+?\"\\)\\|\\(?:DYNAMIC\\)\\);"
          seed7--whitespace-re
          seed7--whitespace-re
          seed7--whitespace-re)
  "Regexp to detect end of function implementation declaration.  No group.")

;;** Seed7 Skipping Comments
;;   -----------------------

(defun seed7-skip-comment-backward (&optional dont-push-mark)
  "Move point backward before comments and consecutive comment blocks.
Push mark before moving unless DONT-PUSH-MARK is non-nil."
  (let ((keep-searching t)
        (original-pos (point))
        (end-pos (point))
        (pos nil))
    (save-excursion
      (while (and keep-searching
                  (not (bobp)))
        (cond
         ;; if at the end of a comment block or line comment,
         ;; or inside a block comment:
         ((seed7-inside-comment-p)
          (cond
           ;; if at end of block comment
           ((save-excursion
              (backward-char)
              (looking-at-p "\\*)"))
            (search-backward "(*" nil :noerror)
            (backward-char))
           ;; if inside a line-end comment
           ((seed7--set (seed7-inside-line-end-comment-p) pos)
            (goto-char (1- pos)))
           ;; if inside a block comment
           (t (search-backward "(*" nil :noerror)
              (backward-char))))
         ;;
         ;; if at indent before the start of a comment
         ((seed7-inside-line-indent-before-comment-p)
          (forward-line 0)
          (backward-char))
         ;;
         ;; If right after the block comment end
         ((save-excursion
            (backward-char 2)
            (looking-at-p "\\*)"))
          (search-backward "(*" nil :noerror)
          (backward-char))
         ;;
         ;; If inside the trailing whitespace of a line
         ;; following the end of a comment block
         ((save-excursion
            (seed7-to-line-last-non-whitespace)
            (seed7-inside-comment-p))
          (search-backward "(*" nil :noerror)
          (backward-char)))
        ;;
        ;; Stop iterating if now outside of comment
        ;; and not at indentation before a comment or,
        ;;     at end of line inside a comment or after a comment
        (cond
         ;; in comment - keep searching
         ((seed7-inside-comment-p))
         ;; at line indent before comment - keep searching from end of
         ;; previous line
         ((save-excursion
            (seed7-inside-line-indent-before-comment-p))
          (forward-line 0)
          (backward-char))
         ;; at line end after block comment
         ((save-excursion
            (and (seed7-at-end-of-line-p)
                 (progn
                   (skip-chars-backward " \t")
                   (backward-char (min (current-column) 2))
                   (looking-at-p "\\*)"))))
          (seed7-to-line-last-non-whitespace))
         ;; otherwise stop
         (t (setq end-pos (point)
                  keep-searching nil)))))
    ;; Push mark if required and return position.
    (unless (or dont-push-mark
                (eq original-pos end-pos))
      (push-mark original-pos))
    (goto-char end-pos)))


(defun seed7---skip-block-comment-forward ()
  "Skip comment block utility.
Only used by `seed7-skip-comment-forward'."
  (search-forward "*)" nil :noerror)
  (when (seed7-at-end-of-line-p)
    (forward-line 1)
    (seed7-to-indent)))

(defun seed7---skip-line-end-comment ()
  "Skip line end comment utility.
Only used by `seed7-skip-comment-forward'."
  (forward-line 1)
  (seed7-to-indent))

(defun seed7-skip-comment-forward (&optional dont-push-mark)
  "Move point forward after comments and consecutive comment blocks.
Push mark before moving unless DONT-PUSH-MARK is non-nil."
  (let ((keep-searching t)
        (original-pos (point))
        (end-pos (point)))
    (save-excursion
      (while (and keep-searching
                  (not (eobp)))
        (cond
         ;; if inside line-end comment,
         ;; or beginning or block comment,
         ;; or inside a block comment:
         ((seed7-inside-comment-p)
          (cond
           ;; if inside line-end comment
           ((seed7-inside-line-end-comment-p)
            (seed7---skip-line-end-comment))
           ;; if just before beginning of a block comment
           ((save-excursion
              (forward-char)
              (looking-at-p "(\\*"))
            (seed7---skip-block-comment-forward))
           ;; at the last character of block comment
           ((save-excursion
              (backward-char)
              (looking-at-p "\\*)"))
            (forward-char))
           ;; if inside a block comment
           (t (seed7---skip-block-comment-forward))))
         ;;
         ;; if in trailing whitespace before line-end comment
         ((seed7-inside-line-trailing-whitespace-before-line-end-comment-p)
          (seed7---skip-line-end-comment))
         ;;
         ;; if in indent before a line or block comment
         ((seed7-inside-line-indent-before-comment-p)
          (seed7-to-indent)
          (cond
           ((looking-at-p "#")
            (seed7---skip-line-end-comment))
           ((looking-at-p "(\\*")
            (seed7---skip-block-comment-forward))))
         ;;
         ;; if before beginning of line end comment
         ((looking-at-p "#")
          (seed7---skip-line-end-comment))
         ;;
         ;; if before beginning of block comment
         ((looking-at-p "(\\*")
          (seed7---skip-block-comment-forward)))
        ;;
        ;; Stop iterating if now outside of comment
        ;; and not before the beginning of another comment block
        (cond
         ((seed7-inside-comment-p))
         ((and (seed7-at-end-of-line-p)
               (save-excursion
                 (forward-line 1)
                 (seed7-to-indent)
                 (seed7-inside-comment-p)))
          (forward-char))
         ((save-excursion
            (unless (seed7-at-end-of-line-p)
              (seed7-to-indent)
              (seed7-inside-comment-p))))
         (t (setq end-pos (point)
                  keep-searching nil)))))
    ;; Push mark if required and return position.
    (unless (or dont-push-mark
                (eq original-pos end-pos))
      (push-mark original-pos))
    (goto-char end-pos)))

;;** Seed7 Navigation by Block/Procedure/Function
;;   --------------------------------------------


;;*** Navigation to Outer Block
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7--to-top (&optional pos)
  "Move point to beginning of outer block surrounding code at POS or point."
  (when pos (goto-char pos))
  (let ((keep-searching t))
    (while (and keep-searching
                (not (bobp))
                (seed7-re-search-backward seed7-block-top-start-regexp))
      (seed7-to-indent)
      (when (= (current-column) 0)
        (setq keep-searching nil)))))

(defun seed7-to-top-of-block ()
  "Move point to the top of the current block."
  (interactive)
  (seed7--to-top))

(defun seed7--block-name (&optional pos)
  "Return the name of the block declared at POS or point.
Return nil if name is not found."
  (save-excursion
    (when pos (goto-char pos))
    (when (seed7-re-search-forward seed7-procfunc-regexp)
      (substring-no-properties (match-string
                                seed7-procfunc-regexp-item-name-group)))))

(defun seed7-top-block-name (&optional pos)
  "Return the name of the top block item surrounding code at POS or point."
  (save-excursion
    (seed7--to-top pos)
    (or (seed7--block-name) "?")))

;;*** Seed7 Procedure/Function Search Utility functions
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(defun seed7--pos-msg (position qualifier name)
  "Return formatted message for start/end of NAME depending on POSITION.
The QUALIFIER is a string that identifies if it is a function or procedure."
  (if (eq position 'at-start-of)
      (format "@ start of %s: '%s'" qualifier name)
    (format "@ end of %s : '%s'" qualifier name)))

(defun seed7--show-info (position name type tail-type)
  "Return formatted message for start/end of function/procedure at POSITION.
- NAME: name of function/procedure.  Extracted from group2 of
  `seed7-procfunc-regexp'.
- TYPE: \"func \" or \"proc\". Extracted from group 2 of seed7-procfunc-regexp.
- TAIL-TYPE: string describing the type of function. Extracted from group 6 of
   seed7-procfunc-regexp."
  (seed7--pos-msg
   position
   type
   (cond
    ((or (string= tail-type "func")
         (string= tail-type "return"))
     name)
    ((string= tail-type "forward;")
     (format "forward declaration of %s" name))
    (t (format "%s, %s" name tail-type)))))

(defun seed7--no-defun-found-msg-for (n direction)
  "Return formatted error message for failure to search N defun in DIRECTION.

- DIRECTION: symbol: either forward to backward."
  (format "There's no %sSeed7 function%s or procedure%s found %s!"
          (if (= n 1) "" (format "%d " n))
          (if (= n 1) "" "s")
          (if (= n 1) "" "s")
          (if (eq direction 'forward) "below" "above")))


;;*** Seed7 Procedure/Function Navigation Commands
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-beg-of-defun (&optional n silent dont-push-mark)
  "Move backward to the beginning of the current function or procedure.

- With optional argument N, repeat the search that many times and succeed
  only when that many function or procedures are found.
  A value of zero means no action.  A negative value is not allowed and raises
  a user error.
- Unless SILENT, the function prints a message showing the name of the
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
  go back to the original position with \\[universal-argument] \\[set-mark-command].
- Supports shift selection."
  (interactive "^P")
  (let* ((n (prefix-numeric-value n))
         (original-pos (point))
         (found-pos nil)
         (item-type nil)
         (item-name nil)
         (tail-type nil))
    (when (< n 0)
      (user-error "Negative N (%d) not allowed!" n))
    (unless (eq n 0)
      (save-excursion
        (dotimes (_ n)
          (setq found-pos nil)
          (when (seed7-re-search-backward seed7-procfunc-regexp)
            (setq found-pos (point)
                  item-type (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                  item-name (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                  tail-type (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group))))))
      (if found-pos
          (let ((top-block-name (seed7-top-block-name)))
            (when (and top-block-name
                       (not (string= top-block-name item-name)))
              (setq item-name (format "%s %s" top-block-name item-name)))
            (seed7--move-and-mark
             original-pos
             found-pos
             dont-push-mark
             (unless silent
               (seed7--show-info 'at-start-of item-name item-type tail-type))))
        (user-error (seed7--no-defun-found-msg-for n 'backward))))))

(defun seed7-beg-of-next-defun (&optional n silent dont-push-mark)
  "Move forward to the beginning of the next function or procedure.

- With optional argument N, repeat the search that many times and succeed
  only when that many function or procedures are found.
  A value of zero means no action.  A negative value is not allowed and raises
  a user error.
- Unless SILENT, the function prints a message showing the name of the
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
  go back to the original position with \\[universal-argument] \\[set-mark-command].
- Supports shift selection."
  (interactive "^P")
  (let* ((n (prefix-numeric-value n))
         (original-pos (point))
         (found-pos nil)
         (item-type nil)
         (item-name nil)
         (tail-type nil))
    (when (< n 0)
      (user-error "Negative N (%d) not allowed!" n))
    (unless (eq n 0)
      (save-excursion
        (dotimes (_ n)
          (setq found-pos nil)          ; only last one is important
          (forward-char)
          (if (seed7-re-search-forward seed7-procfunc-regexp)
              (setq found-pos (point)
                    item-type (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                    item-name (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                    tail-type (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
            (user-error (seed7--no-defun-found-msg-for n 'forward))))
        (when found-pos
          ;; When found, point is somewhere past the very beginning of the
          ;; function.  Move back to its real beginning.
          ;; That search should always succeed.
          (if (seed7-re-search-backward seed7-procfunc-regexp)
              (setq found-pos (point))
            (error "Logic error in:seed7-beg-of-next-defun at %d.  Check regexp used: %s"
                   (point)
                   (symbol-name 'seed7-procfunc-regexp)))))
      ;; Position is found, otherwise an error would have occurred.
      (let ((top-block-name (seed7-top-block-name)))
        (when (and top-block-name
                   (not (string= top-block-name item-name)))
          (setq item-name (format "%s %s" top-block-name item-name)))
        (seed7--move-and-mark
         original-pos
         found-pos
         dont-push-mark
         (unless silent
           (seed7--show-info 'at-start-of item-name item-type tail-type)))))))





(defun seed7-end-of-defun (&optional n silent dont-push-mark)
  "Move forward to the end of the current or next function or procedure.
Move inside the current if inside one, to the next if outside one.
- With optional argument N, repeat the search that many times and succeed
  only when that many function or procedures are found.
  A value of zero means no action.  A negative value is not allowed and raises
  a user error.
- Unless SILENT, the function prints a message showing the item-name of the new
  found function or procedure.
- When a new function or procedure is found the function pushes the mark
  unless DONT-PUSH-MARK is non-nil.  Pushing the mark allows future pop to
  go back to the original position with \\[universal-argument] \\[set-mark-command].
- Supports shift selection."
  (interactive "^P")
  ;; First identify the item-type of declaration by searching for the beginning
  ;; of function or proc using the `seed7-procfunc-regexp' regexp
  ;; which has 6 groups
  (let* ((n (prefix-numeric-value n))
         (original-pos (point))  (found-candidate nil)
         (final-pos nil)         (found-pos nil)
         (item-name nil)         (item-name2 nil)
         (item-type nil)         (item-type2 nil)
         (tail-type nil)         (tail-type2 nil)
         (top-block-name nil)    (top-block-name2 nil))
    (when (< n 0)
      (user-error "Negative N (%d) not allowed!" n))
    (unless (eq n 0)
      (save-excursion
        (dotimes (_ n)
          (setq found-candidate nil
                final-pos nil
                found-pos nil)
          ;; Search for all possible function/procedure end.
          ;; - Retain the one that is closest to point.
          ;; Search for next procedure or long function
          (save-excursion
            (when
                (and
                 (setq final-pos (seed7-re-search-forward seed7-procfunc-end-regexp)
                       top-block-name2 (seed7-top-block-name))
                 (when (seed7-re-search-backward seed7-procfunc-regexp)
                   ;; [:todo 2025-06-12, by Pierre Rouleau: when at end of
                   ;; func that has nested func/proc, the spec extracted below
                   ;; are the spec of the last nested func/proc NOT the spec
                   ;; of the top one. Need a way to distinguish the 2...]
                   (setq item-type (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                         item-name (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                         tail-type (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group))
                         top-block-name top-block-name2)
                   t)
                 (seed7-re-search-forward  seed7-procfunc-end-regexp)
                 (eq (point) final-pos))
              (setq found-candidate t)))
          ;; Search for next short function
          (save-excursion
            (when
                (and
                 (setq found-pos (seed7-re-search-forward seed7-short-func-end-regexp)
                       top-block-name2 (seed7-top-block-name))
                 (when (seed7-re-search-backward seed7-procfunc-regexp)
                   (setq item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                         item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                         tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                   t)
                 (seed7-re-search-forward seed7-short-func-end-regexp)
                 (eq (point) found-pos)
                 (or (not final-pos)
                     (< found-pos final-pos)))
              (setq final-pos found-pos
                    found-candidate t
                    item-name item-name2
                    item-type item-type2
                    tail-type tail-type2
                    top-block-name top-block-name2)))
          ;; Search for next forward declaration
          (save-excursion
            (when
                (and
                 (setq found-pos
                       (seed7-re-search-forward seed7-forward-declaration-end-regexp)
                       top-block-name2 (seed7-top-block-name))
                 (when (seed7-re-search-backward seed7-procfunc-regexp)
                   (setq item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                         item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                         tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                   t)
                 (seed7-re-search-forward seed7-forward-declaration-end-regexp)
                 (eq (point) found-pos)
                 (or (not final-pos)
                     (< found-pos final-pos)))
              (setq final-pos found-pos
                    found-candidate t
                    item-name item-name2
                    item-type item-type2
                    tail-type tail-type2
                    top-block-name top-block-name2)))
          ;; Search for next function implementation declaration
          (save-excursion
            (when
                (and
                 (setq found-pos
                       (seed7-re-search-forward seed7-function-implementation-declaration-end-regexp)
                       top-block-name2 (seed7-top-block-name))
                 (when (seed7-re-search-backward seed7-procfunc-regexp)
                   (setq item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                         item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                         tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                   t)
                 (seed7-re-search-forward seed7-function-implementation-declaration-end-regexp)
                 (eq (point) found-pos)
                 (or (not final-pos)
                     (< found-pos final-pos)))
              (setq final-pos found-pos
                    found-candidate t
                    item-name item-name2
                    item-type item-type2
                    tail-type tail-type2
                    top-block-name top-block-name2)))

          (save-excursion
            (when
                (and
                 (setq found-pos
                       (seed7-re-search-forward seed7-function-implementation-declaration-end-regexp)
                       top-block-name2 (seed7-top-block-name))
                 (when (seed7-re-search-backward-closest (list seed7-forward-or-action-procedure-declaration-re
                                                               seed7-forward-or-action-function-declaration-re))

                   (setq item-type2 (if (seed7-line-starts-with 0 "const proc") "proc" "func")
                         item-name2 "?"
                         tail-type2 "??")
                   t)
                 (seed7-re-search-forward seed7-function-implementation-declaration-end-regexp)
                 (eq (point) found-pos)
                 (or (not final-pos)
                     (< found-pos final-pos)))
              (setq final-pos found-pos
                    found-candidate t
                    item-name item-name2
                    item-type item-type2
                    tail-type tail-type2
                    top-block-name top-block-name2)))

          (if found-candidate
              ;; move to the end of first function to allow next search in loop
              (goto-char final-pos)
            ;; Nothing found in this loop.  Quit searching right away
            (user-error (seed7--no-defun-found-msg-for n 'forward)))))
      (when (and top-block-name
                 (not (string= top-block-name item-name)))
        (setq item-name (format "%s %s" top-block-name item-name)))
      (seed7--move-and-mark
       original-pos
       final-pos
       dont-push-mark
       (unless silent
         (seed7--show-info 'at-end-of item-name item-type tail-type))))))

;;*** Seed7 Procedure/Function Navigation Mode Functions
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; The following functions are used as mode callbacks to perform various
;; operations such as `mark-defun' for marking a complete function. These
;; functions must therefore be silent and not push mark.  They use their
;; interactive counterparts specifying the extra parameters and silencing
;; errors.

(defun seed7--beg-of-defun-silently (&optional n)
  "Simple beginning of defun to use as `beginning-of-defun-function'.

Move once, unless N specifies a different count.
Operate silently; do not issue an error when nothing is found.
Return t if point moved to the beginning of function, nil if nothing found."
  (condition-case nil
      (progn
        (seed7-beg-of-defun n :silent)
        t)
    (error nil)))

(defun seed7--end-of-defun-silently (&optional n)
  "Simple end of defun to use as `end-of-defun-function'.

Move once, unless N specifies a different count.
Operate silently; do not issue an error when nothing is found.
Return t if point moved to the beginning of function, nil if nothing found."
  (condition-case nil
      (progn
        (seed7-end-of-defun n :silent)
        t)
    (error nil)))

;;** Seed7 Navigation by Block
;;   -------------------------
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

;; [:todo 2025-06-30, by Pierre Rouleau: Add support for multiple lines]
(defconst seed7---inner-callables-1
   ;;         (----------------)                (----------)
   ;;                                      (--------------------)
   ;;(-------------------------------------------------------------)
  "\\(const \\(?:func\\|proc\\)[^\\0]+?is\\(?:\\(?: +func\\)?$\\)\\)")

;; [:todo 2025-06-30, by Pierre Rouleau: Add support for multiple lines]
(defconst seed7---inner-callables-2
  ;;              (---------------)
  ;;     (----------------------------)     (--------------)
  ;;  (--------------------------------------------------------)
  (format
   "\\(\\(?:end \\(?:func\\|proc\\);\\)\\|\\(?:return%s+?;\\)\\)"
   seed7--any-wp-text-re))


(defconst seed7---inner-callables-4
  "const proc: .+? is forward;")


(defconst seed7--callable-return-re
  (format "return%s+?;"
          seed7--any-wp-text-re))

(defconst seed7--inner-callables-triplets-re
  (format
   "^[[:blank:]]*?\\(?:%s\\|%s\\|%s\\)"
   seed7---inner-callables-1
   seed7---inner-callables-2
   seed7-forward-or-action-function-declaration-re)
  "A regexp with 3 groups:
- group 1: function/procedure start,
- group 2: function/procedure end,
- group 3: action or forward function declaration.")

(defun seed7--type-regexp (keyword)
  "Return a regexp to search for the start/end of KEYWORD type block.

The regexp has 2 capture groups:
- group1 for the starting expression,
- group2 for then end part."
  (format "^\\(?:[[:space:]]*?\\(const[[:space:]]+?type:.+?[[:space:]]%s\\)\\|[[:space:]]*?\\(end %s;\\)\\)"
          keyword keyword))

;; [:todo 2025-05-31, by Pierre Rouleau: Add support for hard tab after keyword
(defun seed7--end-regxp-for (word1 word2 last-word)
  "Return regexps for end and start of block for specified arguments.
Used when searching forward.
Where WORD1 is the first word, WORD2 the second word and LAST-WORD the last.
The regexp has 2 or 3 groups:
- group 1: block start text,
- group 2: the block end text,
- group 3: optional, a peer level clause."
  (let ((regexp nil)
        (start-pos 'end-of-line))
    (setq regexp
          (cond
           ;; deal with special cases first
           ;; Get the regexp for searching a block. Each regex is a string with 2
           ;; capturing sections: match 1 is the start of the block, match 2 is the
           ;; end.  The following regexp allow no white-space before the keywords even
           ;; though the space is always required in Seed7 code.  This is done this
           ;; way to allow a match when trying to verify a matching block for the
           ;; purpose of calculating the indentation required.
           ((string= word1 "repeat")    "^[[:blank:]]*?\\(?:\\(repeat\\>\\)\\|\\(until\\>\\) \\)" )
           ((string= word1 "block")     "^[[:blank:]]*?\\(?:\\(block\\>\\)\\|\\(end block\\)\\)" )
           ((string= word1 "global")    "^[[:blank:]]*?\\(?:\\(global\\>\\)\\|\\(end global;\\)\\)" )
           ((string= word1 "when")      "^[[:blank:]]*?\\(?:\\(case \\)\\|\\(end case;?\\)\\|\\(when \\|otherwise\\)\\)")
           ((string= word1 "otherwise") "^[[:blank:]]*?\\(?:\\(case \\)\\|\\(end case;?\\)\\)")
           ((string= word1 "elsif")     "^[[:blank:]]*?\\(?:\\(if \\)\\|\\(end if;?\\)\\|\\((elsif \\|else\\)\\)")
           ((string= word1 "else")      "^[[:blank:]]*?\\(?:\\(if \\)\\|\\(end if;?\\)\\)")
           ((member word1 '("local" "begin")) seed7--inner-callables-triplets-re)
           ((string= word1 "const")
            (cond
             ((member word2 '("varfunc" "func" "proc"))
              nil)   ; use `seed7-end-of-defun' for function and procedures.
             ((string= word2 "type")
              (cond
               ((member last-word '("enum" "struct"))
                (seed7--type-regexp last-word))
               (t
                (setq start-pos 'dont-move)
                ;; return a regexp where group 1 never matches
                "\\(^\\(?:;INVALID-MAKE-IT-NEVER-MATCH;\\)\\)\\|\\([[:blank:]]is[[:blank:]]+?.+?;\\)")))
             (t nil)))

           ;; then deal with general case: block, case, for, while.
           ((member word1 seed7--block-start-keywords)
            (format "^[[:blank:]]*?\\(?:\\(%s \\)\\|\\(end %s;?\\)\\)" word1 word1))
           (t nil)))
    (cons regexp start-pos)))

(defun seed7-to-block-forward (&optional dont-push-mark)
  "Move forward from the block beginning to its end.

Handle function and forward declarations blocks.
Push mark unless DONT-PUSH-MARK is non-nil.  Supports shift-marking.
Return found position or nil if nothing found."
  (interactive "^")
  (let ((original-pos (point))
        (found-position nil))
    (save-excursion
      (if (or
           (seed7-inside-comment-p)
           (seed7-inside-line-indent-before-comment-p)
           (seed7-inside-line-trailing-whitespace-before-line-end-comment-p))
          ;; inside comment, skip comment
          (setq found-position
                (seed7-skip-comment-forward dont-push-mark))
        ;; outside comment handle blocks, functions and procedures
        (let ((word1  (seed7--current-line-nth-word 1))
              (word2 (seed7--current-line-nth-word 2)))
          (cond
           ;; handle array
           ((and (string= word2 "array")
                 (member word1 '("const" "var")))
            (seed7-re-search-forward "(")
            (backward-char)
            (forward-sexp)
            (seed7-re-search-forward ";")
            (setq found-position (point)))
           ;; handle set
           ((and (string= word2 "set")
                 (member word1 '("const" "var")))
            (seed7-re-search-forward "{")
            (backward-char)
            (forward-sexp)
            (seed7-re-search-forward ";")
            (setq found-position (point)))
           ;; handle type
           ((and (string= word2 "type")
                 (member word1 '("const" "var"))
                 (not (member (seed7--current-line-nth-word -1) '("struct" "enum"))))
            (seed7-re-search-forward ";")
            (setq found-position (point)))
           ;; handle everything else
           (t
            (let* ((regexp.start-pos (seed7--end-regxp-for
                                      word1 word2
                                      (seed7--current-line-nth-word -1)))
                   (regexp (car regexp.start-pos))
                   (start-pos (cdr regexp.start-pos)))
              (if regexp
                  ;; Inside a block: search with nesting handling.
                  (let ((nesting 0)
                        (searching t))
                    ;; For the very first search, skip the current keyword by
                    ;; moving point to end of line.
                    ;; That skipping won't be necessary in the loop because
                    ;; each search moves the point after the match.
                    (cond
                     ((eq start-pos 'beginning-of-line)   (forward-line 0))
                     ((eq start-pos 'end-of-line)         (end-of-line)))
                    ;; The perform search, handing potential nesting.
                    (while (and searching
                                (not (eobp)))
                      (if (stringp regexp)
                          (if (seed7-re-search-forward regexp)
                              (cond
                               ;; found another block start: nesting deeper.
                               ((match-string 1)
                                (setq nesting (1+ nesting)))
                               ;; found block end: exiting one nesting level.
                               ((match-string 2)
                                (if (eq nesting 0)
                                    (progn
                                      (setq searching nil)
                                      (setq found-position (point)))
                                  (setq nesting (1- nesting))))
                               ;; Found a peer level clause: stop if at
                               ;; nesting level 0
                               ((match-string 3)
                                (when (and (not (string= word2 "func"))
                                           (eq nesting 0))
                                  (setq searching nil)
                                  (setq found-position (point))))
                               ;; found nothing
                               (t (user-error
                                   "seed7-to-block-forward: \
No match.  From %d, at point %d, nesting=%d, line %d for: %S"
                                   original-pos
                                   (point)
                                   nesting
                                   (seed7-current-line-number)
                                   regexp)))
                            (user-error "seed7-to-block-forward: \
NO match.  From %d, at point  %d, nesting=%d, line %d for: %S"
                                        original-pos
                                        (point)
                                        nesting
                                        (seed7-current-line-number)
                                        regexp))
                        ;; 3 regexps are used, not only 1, search for each and move
                        ;; to the one that match closest.
                        (when (seed7-re-search-forward-closest regexp)
                          (setq searching nil
                                found-position (point))))))
                ;; Not inside a block: search for end of function or procedure.
                ;; - Not pushing mark is also an indication to operate silently.
                (seed7-end-of-defun nil dont-push-mark dont-push-mark)
                (setq found-position (point)))))))))
    (when found-position
      (unless (eq original-pos found-position)
        (unless dont-push-mark (push-mark)))
      (goto-char found-position))))


;; [:todo 2025-05-31, by Pierre Rouleau: Add & test support for hard tab after keyword
(defun seed7--start-regxp-for (word1 word2)
  "Return a regexp to search the starting string block specified by the arguments.
Used when searching backward.

- WORD1 : the first word of the end statement.
- WORD2 : the second word of the end statement.

Return a regexp that searches for the start or end of the block.
The regexp has 2 or 3 groups:
- group 1: block start text,
- group 2: the block end text,
- group 3: optional, a peer level clause."
  (let ((regexp nil)
        (same-line nil))
    (setq regexp
          (cond
           ;; Deal with special cases first
           ;;  The following regexp allow no white-space before the keywords even
           ;;  though the space is always required in Seed7 code.  This is done this
           ;;  way to allow a match when trying to verify a matching block for the
           ;;  purpose of calculating the indentation required.
           ((not word1) "^\\(?:[[:space:]]*?\\(const[[:space:]]+?array[[:space:]]+?.+?:\\)\\|[[:space:]]+?\\();\\)\\)")
           ((string= word1 "until")               "^[[:blank:]]*?\\(?:\\(repeat\\>\\)\\|\\(until\\>\\) \\)")
           ((member  word1 '("when" "otherwise")) "^[[:blank:]]*?\\(?:\\(case \\)\\|\\(end case;?\\)\\|\\(when \\)\\)")
           ((member  word1 '("elsif" "else"))     "^[[:blank:]]*?\\(?:\\(if \\)\\|\\(end if;?\\)\\|\\(elsif \\)\\)")
           ((string= word1 "end")
            (cond
             ((string= word2 "func") seed7--inner-callables-triplets-re)
             ((string= word2 "global")            "^[[:blank:]]*?\\(?:\\(global\\>\\)\\|\\(end global;\\)\\)")
             ((string= word2 "block")             "^[[:space:]]*?\\(block\\>\\(?:[[:space:]]*?#.*?\\)?$\\)\\|\\(end block;\\)")
             ((member word2 '("case"
                              "for"
                              "if"
                              "while"))
              (format"^[[:space:]]*?\\(?:\\(%s \\)\\|\\(end %s;?\\)\\)" word2 word2))
             ((member word2 '("enum"
                              "struct"))
              (seed7--type-regexp word2))
             (t nil)))
           ((and (string= word1 "const")
                 (string= word2 "type"))
            (setq same-line t)
            ;; return a regexp where the group 2 never matches
            "\\(\\(?:^[[:space:]]*?const[[:space:]]+?type:.+?[[:space:]]\\)\\)\\|\\(^;INVALID-MAKE-IT-NEVER-MATCH;\\)")
           (t nil)))
    (cons regexp same-line)))

(defun seed7-to-block-backward (&optional at-beginning-of-line dont-push-mark)
  "Move backward from block end to its beginning.

Move point to the beginning of the block keyword or comment.
 If point moves to the indenting area as a result, and AT-BEGINNING-OF-LINE
 optional argument is set, move point to the beginning of the line.
Push mark unless DONT-PUSH-MARK is non-nil.  Supports shift-marking.
Return found position if found, nil if nothing found."
  (interactive "^P")
  (let ((original-pos (point))
        (found-position nil))
    (if (or (seed7-inside-comment-p)
            (seed7-inside-line-indent-before-comment-p))
        (setq found-position
              (seed7-skip-comment-backward dont-push-mark))
      (save-excursion
        (let* ((first-word (seed7--current-line-nth-word 1))
               (second-word (seed7--current-line-nth-word 2))
               (regexp.same-line (seed7--start-regxp-for first-word
                                                         second-word))
               (regexp (car regexp.same-line))
               (same-line (cdr regexp.same-line)))
          (if (and regexp
                   first-word)
              ;; Inside a block: search with nesting handling.
              (let ((nesting 0)
                    (searching t))
                ;; For the very first search, skip the current keyword by
                ;; moving point to beginning of line.
                ;; That skipping won't be necessary in the loop because each
                ;; search moves the point after the match.
                (unless same-line
                  (forward-line 0))
                (while searching
                  (if (seed7-re-search-backward regexp)
                      (cond
                       ;; Found another block start text: exiting a nested
                       ;; level.
                       ((match-string 1)
                        (if (eq nesting 0)
                            (progn
                              (setq searching nil)
                              (setq found-position (point)))
                          (setq nesting (1- nesting))))
                       ;; Found a block end text: entering a deeper nesting
                       ;; level.
                       ((match-string 2)
                        (setq nesting (1+ nesting)))
                       ;; Found peer level clause: stop if at nesting level 0
                       ((match-string 3)
                        (when (and (not (string= second-word "func"))
                                   (eq nesting 0))
                          (setq searching nil)
                          (setq found-position (point))))
                       ;; found nothing
                       (t (user-error
                           "seed7-to-block-backward: \
No match.  From %d, at point %d, nesting=%d, line %d for: %S"
                           original-pos
                           (point)
                           nesting
                           (seed7-current-line-number)
                           regexp)))
                    (user-error
                     "seed7-to-block-backward: \
NO match.  From %d, at point %d, nesting=%d, line %d  for: %S"
                     original-pos
                     (point)
                     nesting
                     (seed7-current-line-number)
                     regexp))))
            ;; Not inside a block.
            ;; Check for end of parens pair (used for array and set)
            (let ((pos nil))
              (cond
               ((seed7--set (seed7-line-code-ends-with 0 ");") pos)
                (goto-char (1+ pos))
                (backward-sexp)
                (seed7-to-indent)
                (setq found-position (point)))
               ((seed7--set (seed7-line-code-ends-with 0 "};") pos)
                (goto-char (1+ pos))
                (backward-sexp)
                (seed7-to-indent)
                (setq found-position (point)))
               (t
                ;; search for beginning of function or procedure.
                ;; - Not pushing mark is also an indication to operate silently.
                (seed7-beg-of-defun nil dont-push-mark dont-push-mark)
                (setq found-position (point)))))))))
    (when found-position
      (unless (eq original-pos found-position)
        (unless dont-push-mark (push-mark)))
      (goto-char found-position)
      (unless at-beginning-of-line
        (when (seed7-inside-line-indent-before-comment-p)
          (seed7-to-indent)))
      (point))))

;; ---------------------------------------------------------------------------
;;* Seed7 iMenu Support
;;  ===================

(defvar-local seed7--menu-list-functions-and-procedures-together
    seed7-menu-list-functions-and-procedures-together
  "When on, list function and procedures together, otherwise separately.

Dynamic value that affects the way the callable are displayed in imenu
commands, in the top menu and inside the Speedbar.
Initialized to `seed7-menu-list-functions-and-procedures-together' user-option
value which can then be dynamically modified by the
`seed7-toggle-menu-callable-list' command.")

(defun seed7--setup-imenu ()
  "Configure the way imenu lists its items."
  (setq-local
   imenu-generic-expression
   (if seed7--menu-list-functions-and-procedures-together
       (list
        (list "Enum"      seed7-enum-regexp 1)
        (list "Interface" seed7-interface-regexp 1)
        (list "Struct"    seed7-struct-regexp 1)
        (list "Callable"  seed7-procfunc-regexp
              seed7-procfunc-regexp-item-name-group))
     (list
      (list "Enum"      seed7-enum-regexp 1)
      (list "Interface" seed7-interface-regexp 1)
      (list "Struct"    seed7-struct-regexp 1)
      (list "Procedure" seed7-procedure-regexp 1)
      (list "Function"  seed7-function-regexp  2)))))


(defun seed7--refresh-imenu ()
  "Force re-display of the imenu."
  (imenu--menubar-select imenu--rescan-item)
  (imenu-update-menubar))

(defun seed7-toggle-menu-callable-list ()
  "Change the way callables are listed inside the current buffer menu.
Toggles listing them together or separately.
  When listed separately the function and procedures are listed inside
  their own group, otherwise they are listed together."
  (interactive)
  (if seed7--menu-list-functions-and-procedures-together
      (setq seed7--menu-list-functions-and-procedures-together nil)
    (setq seed7--menu-list-functions-and-procedures-together t))
  (message "Now listing function & procedure %s in Seed7 buffers."
           (if seed7--menu-list-functions-and-procedures-together
               "together"
             "separately"))
  (seed7--setup-imenu)
  (seed7--refresh-imenu))

(defvar-local seed7--menu-list-functions-sorted seed7-menu-list-functions-sorted
  "Set to non-nil to list menu entries in sorted order")

(defun seed7-toggle-menu-sorting ()
  "Toggle displaying menu entries in code order or sorted order."
  (interactive)
  (if seed7--menu-list-functions-sorted
      (setq seed7--menu-list-functions-sorted nil
            imenu-sort-function nil)
    (setq seed7--menu-list-functions-sorted t
          imenu-sort-function 'imenu--sort-by-name))
  (message "Now listing menu entries in %s order."
           (if seed7--menu-list-functions-sorted
               "sorted"
             "code"))
  (seed7--refresh-imenu))

;; ---------------------------------------------------------------------------
;;* Seed7 Code Marking
;;  ==================

;; [:todo 2025-06-21, by Pierre Rouleau: Perhaps this can use
;; `seed7-to-top-of-block' to handle more blocks?]
(defun seed7-mark-defun ()
  "Mark the current Seed7 function or procedure.
Put the mark at the end and point at the beginning.
If point is before or between 2 functions or procedure, mark the next one."
  (interactive)
  (let ((original-pos (point))
        (start-pos nil)
        (end-pos nil))
    (save-excursion
      (setq start-pos
            (condition-case nil
                (progn
                  (seed7-beg-of-defun 1 :silent)
                  (point))
              (error
               ;; Handle the case where a short function is at the top
               ;; of the buffer: the above call will fail.
               (condition-case nil
                   (progn
                     (forward-line -1)
                     (seed7-beg-of-next-defun 1 :silent)
                     (point))
                 (error nil)))))
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

(defun seed7-lines-in-defun ()
  "Return the number of lines of a Seed7 function or procedure."
  (let ((line-count 0))
    (save-excursion
      (seed7-mark-defun)
      (setq line-count (count-lines (region-beginning) (region-end)))
      (deactivate-mark))
    line-count))

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

(defmacro seed7--inside-block-p (syntax)
  "Return non-nil if point is inside matching pair block according to SYNTAX."
  `(> (nth 0 ,syntax) 0))

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
  "Return the current line number.  1 for the first line."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun seed7-to-indent ()
  "Move point to the first non-whitespace character of the line."
  (forward-line 0)
  (skip-chars-forward " \t"))

(defun seed7-current-line-start-inside-comment-p ()
  "Return non-nil if the current line start inside a comment."
  (save-excursion
    (seed7-to-indent)
    (seed7-inside-comment-p (point))))

(defun seed7-to-line-last-non-whitespace ()
  "Move point to the last non-whitespace character of the line.
Return it's position if found, nil if the line is empty."
  (end-of-line)
  ;; move back to the last non-white, but stay on the same line.
  (re-search-backward "[^ \t\r\n]" (save-excursion
                                     (forward-line 0)
                                     (point))
                      :noerror))

(defun seed7-at-end-of-line-p ()
  "Return non-nil if point is at end of line or whitespace before end of line.
Return nil otherwise."
  (save-excursion
    (skip-chars-forward " \t")
    (looking-at-p "\n")))

;; --

(defun seed7-inside-line-end-comment-p ()
  "Return non-nil if point is inside a line-end comment, nil otherwise.
Specifically, it returns the position of the # character that starts the line
comment if point is inside a block comment or nil if point is inside a block
comment or not inside comment."
  (when (seed7-inside-comment-p)
    (save-excursion
      (let ((end-pos (1+ (point)))
            (keep-searching t)
            (line-comment-start-pos nil))
        (forward-line 0)
        (while keep-searching
          (if (search-forward "#" end-pos :noerror)
              (when (and (seed7-inside-comment-p)
                         (or (eq (current-column) 1)
                             (save-excursion
                               (backward-char 2)
                               (not (seed7-inside-comment-p)))))
                (setq line-comment-start-pos (1- (point))
                      keep-searching nil))
            (setq keep-searching nil)))
        line-comment-start-pos))))

(defun seed7-inside-line-indent-p ()
  "Return non-nil if point is inside line indentation or first char of text."
  (save-excursion
    (let ((current-pos (point))
          (line-start-pos (save-excursion (forward-line 0)
                                          (point))))
      (seed7-to-indent)
      (or (eq line-start-pos current-pos)
          (< line-start-pos current-pos (point))
          (eq current-pos (point))))))

(defun seed7-inside-line-indent-before-comment-p ()
  "Return non-nil if point is between beginning of line and a comment."
  (save-excursion
    (let ((current-pos (point))
          (line-start-pos (save-excursion (forward-line 0)
                                          (point))))
      (seed7-to-indent)
      (when (or (eq line-start-pos current-pos)
                (< line-start-pos current-pos (point)))
        (seed7-inside-comment-p)))))

(defun seed7-inside-line-trailing-whitespace-before-line-end-comment-p ()
  "Return non-nil if point is in whitespace before line-end comment.
Return nil otherwise."
  (save-excursion
    (unless (or (seed7-inside-comment-p)
                (seed7-inside-line-indent-before-comment-p))
      (skip-chars-forward " \t")
      (and (looking-at-p "#")
           (seed7-inside-comment-p)))))


;;*** Seed7 Indentation Code Character Search Utilities

(defun seed7-backward-char-pos (char &optional bound)
  "Back search for CHAR in code, return its position or nil.
CHAR is a string of 1 character.
If BOUND is specified it bounds the search; it is a buffer position:
the match found must not begin before that position.
Do not move point."
  (save-excursion
    (seed7-re-search-backward (regexp-quote char) bound)))

(defun seed7-forward-char-pos (char &optional bound)
  "Forward search for CHAR in code, return its position or nil.
CHAR is a string of 1 character.
If BOUND is specified it bounds the search; it is a buffer position:
the match found must not begin after that position.
Do not move point."
  (save-excursion
    (seed7-re-search-forward (regexp-quote char) bound)))


;;*** Seed7 Indentation Base Position Detection Utilities

;; [:todo 2025-05-30, by Pierre Rouleau: Add bound search limit]
(defun seed7-assign-op-pos ()
  "Position of end of previous Seed7 assignment operator if found.
Return nil if not found.
Do not move point."
  (save-excursion
    (when (seed7-re-search-backward seed7-predef-assignment-operator-regxp)
      (+ (point) (length (match-string 0))))))

;; [:todo 2025-05-30, by Pierre Rouleau: Add bound search limit]
(defun seed7-statement-end-pos (&optional start-pos)
  "Position of next end of Seed7 statement if found, nil otherwise.
Start searching at current point, unless START-POS is non-nil/
Do not move point."
  (save-excursion
    (when start-pos
      (goto-char start-pos))
    (seed7-forward-char-pos ";")))


;;*** Seed7 Indentation Base Line Navigation

(defun seed7-to-previous-non-empty-line (&optional dont-skip-comment-start)
  "Move point to the beginning of the previous non-empty line.
Skip lines starting with new comment unless DONT-SKIP-COMMENT-START is
non-nil.  Always skip lines that are *inside* a comment.
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
          (unless (looking-at-p "\n")
            (when (or (not (seed7-inside-comment-p (point)))
                      dont-skip-comment-start)
              (setq found-pos (point)))))))
    ;; found appropriate position: move to it and return its indentation.
    (when found-pos
      (goto-char found-pos)
      (current-column))))

(defun seed7-move-to-line (n &optional dont-skip-comment-start)
  "Move point to the beginning of text of line N.
N is: - :dont-move : do not move point
      - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
Return nil if no appropriate line found.
Return the column number of point if appropriate line found."
  (cond
   ((eq n :previous-non-empty)
    (seed7-to-previous-non-empty-line dont-skip-comment-start))
   ;;
   ((integerp n)
    (when (eq (forward-line n) 0)
      (skip-chars-forward " \t")
      (current-column)))
   ;;
   ((eq n :dont-move)
    (current-column))
   ;;
   (t (error "Invalid N: %S" n))))

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
          (if (looking-at-p "include +\"")
              0
            (seed7-indent-step-for-column (current-column))))
      ;; No appropriate line found, return 0 for indentation.
      0)))

;;** Seed7 Indentation Line Checking Base Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-to-previous-line-starts-with (regexp &optional bound)
  "Move to previous code line starting with text specified by the REGEXP.
Return position of the text found if found, nil otherwise.
When something is found, leave point at the found position, if nothing
  found do not move point.
The optional second argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer."
  (let ((regexp (concat "^[[:blank:]]*?" regexp))
        (found-pos nil))
    (save-excursion
      (when (seed7-re-search-backward regexp bound)
        (seed7-to-indent)
        (setq found-pos (point))))
    (when found-pos
      (goto-char found-pos))))

(defun seed7-to-next-line-starts-with (regexp)
  "Move to next line starting with text specified by the REGEXP.
Return position of the (indented) text found if found, nil otherwise.
When something is found, leave point at the found position, if nothing
  found do not move point."
  (let ((regexp (concat "^[[:blank:]]*?" regexp))
        (found-pos nil))
    (save-excursion
      (when (seed7-re-search-forward regexp)
        (skip-chars-forward " \t")
        (setq found-pos (point))))
    (when found-pos
      (goto-char found-pos))))

(defun seed7-line-starts-with (n regexp
                                 &optional dont-skip-comment-start end-pos)
  "Return indent column when line N non-white space begins with REGEXP.
Return nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If END-POS is non-nil, it identifies the limit for the string."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (skip-chars-forward " \t")
      (when (and (looking-at-p regexp)
                 (or (not end-pos)
                     (save-excursion
                       (re-search-forward regexp end-pos :noerror))))
        (current-column)))))

(defun seed7-line-starts-with-any (n regexps
                                     &optional dont-skip-comment-start end-pos)
  "Return indent column when line N non-white space begins with any of REGEXPS.
Return nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If END-POS is non-nil, it identifies the limit for the string."
  (let ((regexp nil)
        (found-column nil))
    (while (and regexps (not found-column))
      (setq regexp  (car-safe regexps))
      (setq regexps (cdr-safe regexps))
      (when regexp
        (setq found-column (seed7-line-starts-with n regexp
                                                   dont-skip-comment-start
                                                   end-pos))))
    found-column))

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

(defun seed7-line-code-ends-with (n regexp &optional dont-skip-comment-start)
  "Return non-nil when line N non-white space code ends with REGEXP.
When found it returns the position of the string found.
Return nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((line-start-pos (save-excursion
                              (forward-line 0)
                              (point)))
            (line-code-end-pos nil)
            (found-pos nil))
        ;; First move to the end of the line code, skipping comments.
        (end-of-line nil)
        (when (seed7-re-search-backward "[^ \t]" line-start-pos)
          (forward-char)
          (setq line-code-end-pos (point))
          (when (seed7--set (seed7-re-search-backward regexp line-start-pos)
                          found-pos)
          (when (eq (+ found-pos
                       (length (substring-no-properties (match-string 0))))
                    line-code-end-pos)
            found-pos)))))))

;;** Seed7 Indentation Line Type Checking Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-line-isa-string (n)
  "Return non-nil indent column if line N is a string, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with n "\""))

(defun seed7-line-is-block-end (n &optional dont-skip-comment-start)
  "Return t if line N is a block end, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with n seed7-block-end-regexp dont-skip-comment-start))

(defun seed7--block-end-pos-for (header)
  "Return position of end of block starting with HEADER.
Move point."
  (cond
   ((member header '("const proc: "
                     "const func "
                     "const type: "
                     "local"
                     "repeat"
                     "begin"
                     "block"
                     "global"
                     "else"
                     "result"
                     "if "
                     "elsif "
                     "while "
                     "for "
                     "case "))
    (seed7-to-block-forward :dont-push-mark)
    (point))
   ;;
   ((string= header "exception")
    (seed7-to-next-line-starts-with "end block")
    (point))
   ;;
   ((string= header "catch ")
    (seed7-to-next-line-starts-with "end block")
    (point))
   ;;
   (t (error "Unsupported block header: %s" header))))

(defun seed7--at-pos-looking-at-p (pos regexp)
  "Return non-nil if text at POS matches REGEXP, nil otherwise.
Does not move point, does not modify search match data."
  (save-excursion
    (goto-char pos)
    (looking-at-p regexp)))

;; [:todo 2025-06-04, by Pierre Rouleau: optimize #1? Add first-word argument
;;                    and passed from seed7-line-inside-a-block call.
;;                    Would eliminate the need to search
;;                    (or add redundancy check) ]
(defun seed7--indent-offset-for (header line-n-indent-pos)
  "Return indentation offset (in columns) for the inside of a block.

- HEADER: string: identifies the block start header.
- LINE-N-INDENT-POS: position: the indented position of the currently
  inspected line."
  (cond

   ;;-- local - begin - end func;
   ((string= header "local")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "begin")
        ;; The begin keyword lines up with the local keyword
        0
      ;; Other lines are indented
      seed7-indent-width))
   ((string= header "begin")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end func;")
        ;; The end func; lines up with begin
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- if - elsif - else - end if
   ((member header '("if "
                     "elsif "
                     "else"))
    (if (or
         (seed7--at-pos-looking-at-p line-n-indent-pos "elsif")
         (seed7--at-pos-looking-at-p line-n-indent-pos "else")
         (seed7--at-pos-looking-at-p line-n-indent-pos "end if;?"))
        ;; The elsif/else/end if keywords line up with the if keyword
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- block - exception - catch - end block;
   ((string= header "block")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "exception")
        ;; exception lines up with block
        0
      ;; Other lines are indented
      seed7-indent-width))
   ((string= header "exception")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "catch ")
        ;; catch is indented one level from exception
        seed7-indent-width
      ;; Other lines are indented by 2 levels
      (* 2 seed7-indent-width)))
   ((string= header "catch ")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end block")
        ;; en block lines up with block, 1 indentation level less than catch
        (-  seed7-indent-width)
      ;; Other lines are indented by 1 level relative to catch
      seed7-indent-width))

   ;;-- repeat - until
   ((string= header "repeat")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "until ")
        ;; until lines up with repeat
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- global - end global
   ((string= header "global")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end global;")
        ;; until lines up with global
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- for - end for
   ((string= header "for ")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end for;?")
        ;; until lines up with for
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- while - end while - elsif - end while
   ((string= header "while ")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end while;?")
        ;; until lines up with for
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- case - end case;
   ((string= header "case ")
    (if (seed7--at-pos-looking-at-p line-n-indent-pos "end case;?")
        ;; until lines up with for
        0
      ;; Other lines are indented
      (if (or (seed7--at-pos-looking-at-p line-n-indent-pos "when ")
              (seed7--at-pos-looking-at-p line-n-indent-pos "otherwise"))
          ;; the when and otherwise keywords are indented once
          seed7-indent-width
        ;; the statements inside when or otherwise blocks are indented twice
        (* 2 seed7-indent-width))))

   ;;-- const func - return/result-begin
   ((string= header "const func ")
    (if (or (seed7--at-pos-looking-at-p line-n-indent-pos "return ")
            (seed7--at-pos-looking-at-p line-n-indent-pos "result"))
        seed7-indent-width
      ;; otherwise indent below 'return '. BUT other code should handle this.
      (+ seed7-indent-width 7)))
   ((string= header "result")
    (if (or (seed7--at-pos-looking-at-p line-n-indent-pos "begin")
            (seed7--at-pos-looking-at-p line-n-indent-pos "local"))
        ;; begin/local lines up with result
        0
      ;; other lines are indented
      seed7-indent-width))

   ;;-- const proc
   ((string= header "const proc: ")
    seed7-indent-width)

   ((string= header "const type: ")
    (if (or (seed7--at-pos-looking-at-p line-n-indent-pos "end struct;")
            (seed7--at-pos-looking-at-p line-n-indent-pos "end enum;"))
        ;; end struct/enum is indented once relative to the const type
        seed7-indent-width
      ;; the definitions are indented twice.
      (* 2 seed7-indent-width)))

   ;; (error "Unsupported header %s" header)
   (t 0)))


(defun seed7--on-lineof  (start-pos &optional pos)
  "Return t if point or POS is on first line of bock starting at START-POS."
  (<= (save-excursion (goto-char start-pos)
                      (forward-line 0)
                      (point))
      (or pos (point))
      (save-excursion (goto-char start-pos)
                      (end-of-line)
                      (point))))

;; [:todo 2025-06-04, by Pierre Rouleau: optimize #1? Add first-word argument
;;                    and pass it to seed7--indent-offset-for ]
(defun seed7-line-inside-a-block (n &optional dont-skip-comment-start)
  "Check if line N is inside a Seed7 block.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If nothing found it returns nil.
If it finds something it returns a list that holds the following information:
- 0: indent column : indentation column the line N should use,
- 1: match string  : the found string describing the type of block,
- 2: block start position, (the beginning of the start keyword line),
- 3: enclosing block end position.
- 4: indent column of the block start."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (block-start-pos nil)
            (enclosing-block-start-pos nil)
            (enclosing-block-end-pos nil)
            (match-text nil)
            (block-start-indent-column nil)
            ;; (line-n-indent-column nil)
            (keep-searching t)
            (result nil))
        ;; all block line start regexp fit 1 line: search from end of line
        ;; to detect if the point is on the block start we find.
        (end-of-line)
        (while (and keep-searching
                    (not (bobp)))
          (when (seed7-re-search-backward seed7-block-line-start-regexp)
            (setq match-text (substring-no-properties (match-string 1)))
            (setq block-start-pos (point)) ; start point: line start.
            (skip-chars-forward " \t")
            (setq block-start-indent-column (current-column))
            ;; Identify the end position and start position of the currently
            ;; enclosing block
            (save-excursion
              (setq enclosing-block-end-pos (seed7--block-end-pos-for match-text))
              ;; Identify the beginning of the enclosing top-level block
              (setq enclosing-block-start-pos
                    (seed7-to-block-backward nil :dont-push-mark)))
            (when (<= block-start-pos current-pos enclosing-block-end-pos)
              ;; It's a real and valid block: 3 cases are possible:
              (cond
               ;; Case 1: point is on the start line of a top level block.
               ((and (seed7--on-lineof block-start-pos current-pos)
                     (member match-text '("const proc: "
                                          "const func "
                                          "const type: ")))
                ;; In that case the enclosing block start should be the same as
                ;; the block start.  If it's not the case there's a logic error.
                (when (eq enclosing-block-start-pos block-start-pos)
                  ;; When at the line of top-level enclosing block, use the
                  ;; `seed7-calc-indent' to get the indentation of the line just
                  ;; above the current line, as if the previous line was a noop
                  ;; statement.  Simulate the noop statement by temporary
                  ;; inserting it in the above line.  This way it will be
                  ;; possible to handle the case where the statement is inside
                  ;; the first line of another block statement like an if
                  ;; statement.
                  ;; Note that calling `seed7-calc-indent' means recursing into
                  ;; it since it's `seed7-calc-indent' that calls
                  ;; `seed7-line-inside-a-block'.  But doing it this way we use
                  ;; all the logic necessary to compute the indentation for this
                  ;; case.
                  (setq keep-searching nil
                        result (list (save-excursion (forward-line 0)
                                                     (insert " noop;\n")
                                                     (forward-line -1)
                                                     (prog1
                                                         (seed7-calc-indent)
                                                       (delete-region
                                                        (point)
                                                        (progn
                                                          (forward-line 1)
                                                          (point)))))
                                     match-text
                                     enclosing-block-start-pos
                                     enclosing-block-end-pos
                                     block-start-indent-column))))
               ;;
               ;; Case 2: point is on an internal block start line.
               ((seed7--on-lineof block-start-pos current-pos)
                ;; Look for the previous block and use info from it.
                (goto-char block-start-pos)
                (when (seed7-re-search-backward
                       seed7-block-line-start-regexp)
                  (setq match-text (substring-no-properties (match-string 1)))
                  (setq block-start-pos (point)) ; start point: line start.
                  (skip-chars-forward " \t")
                  (setq block-start-indent-column (current-column))
                  ;; Identify the end position and start position of the
                  ;; currently enclosing block
                  (save-excursion
                    (setq enclosing-block-end-pos
                          (seed7--block-end-pos-for match-text))
                    ;; Identify the beginning of the enclosing top-level block
                    (setq enclosing-block-start-pos
                          (seed7-to-block-backward nil :dont-push-mark)))
                  (when (<= block-start-pos current-pos enclosing-block-end-pos)
                    (setq keep-searching nil
                          result (list (+ block-start-indent-column
                                          (seed7--indent-offset-for match-text
                                                                    current-pos))
                                       match-text
                                       block-start-pos
                                       enclosing-block-end-pos
                                       block-start-indent-column)))))
               ;;
               ;; Case 3: point is on a line that is not on the block start,
               ;;         but between the block start and the block end.
               (t
                (setq keep-searching nil
                      result (list (+ block-start-indent-column
                                      (seed7--indent-offset-for match-text
                                                                current-pos))
                                   match-text
                                   block-start-pos
                                   enclosing-block-end-pos
                                   block-start-indent-column)))))
            (when keep-searching
              ;; Found something that looks like a block, but either not
              ;; a real block or not the block that holds the line.
              ;; Need to search back further for a bigger block.  If
              ;; block-start-pos is not a column 0, then keep searching
              ;; above for the beginning of a larger block.
              (if (eq (current-column) 0)
                  (setq keep-searching nil)
                (goto-char (1- block-start-pos))))))
        result))))


;; ---------------------------------------------------------------------------
            ;; (if (and (<= block-start-pos current-pos enclosing-block-end-pos)
            ;;          ;; check if block start/end is consistent for those
            ;;          ;; that support it, exclude this check for others.
            ;;          ;; The check is important for type definition blocks
            ;;          ;; to deal with short type definitions that are not
            ;;          ;; blocks.
            ;;          (or
            ;;           (member match-text '("local"
            ;;                                "begin"
            ;;                                "global"
            ;;                                "result"
            ;;                                "elsif "
            ;;                                "else"
            ;;                                "catch "
            ;;                                "case "
            ;;                                "exception"))
            ;;           (eq block-start-pos enclosing-block-start-pos)))
            ;;     (progn
            ;;       (setq line-n-indent-column
            ;;             (if (seed7--on-lineof enclosing-block-start-pos current-pos)
            ;;                 0
            ;;               (seed7--indent-offset-for match-text current-pos)))
            ;;       (setq keep-searching nil)
            ;;       (setq result  t))
            ;;   ;; found something that looks like a block, but either not
            ;;   ;; a real block or not the block that holds the line need
            ;;   ;; to search back further for a bigger block.  If
            ;;   ;; block-start-pos is not a column 0, then keep searching
            ;;   ;; above for the beginning of a larger block.
            ;;   (if (eq (current-column) 0)
            ;;       (setq keep-searching nil)
            ;;     (goto-char (1- block-start-pos))))


        ;; (when result
        ;;   (list (or line-n-indent-column 0)
        ;;         match-text
        ;;         block-start-pos
        ;;         enclosing-block-end-pos
        ;;         block-start-indent-column))


;; ---------------------------------------------------------------------------

(defun seed7-line-inside-until-logic-expression (n &optional
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)
  "Check if line N is inside a Seed7 until logic expression.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If it finds that the line is inside the until logic expression, it
returns the indentation column, corresponding to one column right of the
end or the until word.
If it detects that it is outside, it returns nil."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-until-logic-expression: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (block-start-pos nil)
            (enclosing-block-end-pos nil)
            (found-column nil)
            (keep-searching t))
        (while (and keep-searching
                    (not (bobp)))
          (if (seed7-re-search-backward "^[[:blank:]]+until " scope-begin-pos)
              (progn
                (setq keep-searching nil)
                (unless (seed7-line-code-ends-with 0 ";")
                  ;; found an incomplete until statement remember position
                  (setq block-start-pos (point))
                  ;; Now search the end, which ends on a ';'.
                  (setq enclosing-block-end-pos (seed7-statement-end-pos))
                  (when (and  (< block-start-pos current-pos enclosing-block-end-pos)
                              (or (not scope-end-pos)
                                  (< enclosing-block-end-pos scope-end-pos)))
                    (goto-char block-start-pos)
                    (seed7-to-indent)
                    (setq found-column (+ 6 (current-column))))))
            ;; not finding the until: stop
            (setq found-column nil)
            (setq keep-searching  nil)))
        found-column))))


(defun seed7-line-inside-func-return-statement (n &optional
                                                  scope-begin-pos
                                                  scope-end-pos
                                                  dont-skip-comment-start)
  "Check if line N is inside a Seed7 func return statement.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If it finds that the line is inside the func return statement, it
returns the indentation column of the return keyword.
If it detects that it is outside, it returns nil."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "Invalid boundaries in: seed7-line-inside-func-return-statement:\
 begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (start-pos nil)
            (end-pos nil)
            (found-column nil)
            (keep-searching t))
        (while (and keep-searching
                    (not (bobp)))
          (if (seed7-re-search-backward "^[[:blank:]]+return "
                                        scope-begin-pos)
              (progn
                (setq keep-searching nil)
                (seed7-to-indent)
                (setq start-pos (point))
                (setq found-column (+  (current-column) 5))
                (setq end-pos (seed7-forward-char-pos ";" scope-end-pos))
                (unless (and end-pos
                             (< start-pos current-pos end-pos)
                             (or (not scope-end-pos)
                                 (< end-pos scope-end-pos)))
                  (setq found-column nil)))
            ;; no return found
            (setq found-column nil)
            (setq keep-searching nil)))
        found-column))))

(defun seed7-line-inside-proc-argument-list-section (n &optional
                                                       scope-begin-pos
                                                       scope-end-pos
                                                       dont-skip-comment-start)
  "Check if line N is inside a Seed7 procedure argument list section.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If it finds that the line is inside the procedure list section it
returns the indentation column of the procedure.
If it detects that it is outside, it returns nil."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-proc-argument-list-section: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((current-pos (point))
            (start-pos nil)
            (end-pos nil)
            (found-column nil)
            (keep-searching t))
        (while (and keep-searching
                    (not (bobp)))
          (if (seed7-re-search-backward
               "^[[:blank:]]*const[[:blank:]
]+proc[[:blank:]
]*:[[:blank:]
]" scope-begin-pos)
              (seed7-to-indent)
            (setq start-pos (point))
            (setq found-column (current-column))
            (setq keep-searching nil)
            (if (seed7-re-search-forward " is func" scope-end-pos)
                (progn
                  (setq end-pos (point))
                  (unless (< start-pos current-pos end-pos)
                    (setq found-column nil)))
              ;; not finding the end: nothing found and stop.
              (setq found-column nil)
              (setq keep-searching nil))
            ;; not finding beginning: stop
            (setq found-column nil)
            (setq keep-searching nil)))
        found-column))))


(defun seed7-line-inside-array-definition-block (n &optional
                                                   dont-skip-comment-start)
  "Check if line N is inside an array definition block.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If nothing found it returns nil.
If line N is inside an array block, it returns a list with the following
information:
- 0: indent column : indentation column the line N should use,
- 1: string: \"array\"
- 2: block start position,
- 3: block end position."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-indent-column nil))
        (when (seed7-re-search-backward
               "^[[:blank:]]*?\\(?:const\\|var\\)[[:blank:]
]+?array[[:blank:]
]+?.+?:.+?(")
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (when (seed7-re-search-forward "(")
            (backward-char)
            (forward-sexp)
            ;; point is at block end
            (when (< block-start-pos original-pos (point))
              (list block-indent-column
                    "array"
                    block-start-pos
                    (point)))))))))


(defun seed7-line-at-endof-array-definition-block (n &optional
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
             (enclosing-block-end-pos nil))
        (when (seed7-re-search-backward ");")
          (forward-char)
          (setq enclosing-block-end-pos (point))
          (backward-sexp)
          (seed7-to-indent)
          (when (looking-at-p "\\(?:const\\|var\\) +?array +?.+?:.+?(")
            (setq block-start-pos (point))
            (when (< block-start-pos line-start-pos enclosing-block-end-pos line-end-pos)
              block-indent-column)))))))

(defun seed7-line-inside-set-definition-block (n &optional
                                                 dont-skip-comment-start)
  "Check if line N is inside a set definition block.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If nothing found it returns nil.
If line N is inside a set set definition block, it returns a list with the
following information:
- 0: indent column : indentation column the line N should use,
- 1: string: \"set\"
- 2: block start position,
- 3: block end position."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-indent-column nil))
        (when (seed7-re-search-backward
               "^[[:blank:]]*?\\(?:const\\|var\\)[[:blank:]
]+?set[[:blank:]
]+?.+?:.+?{")
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (when (seed7-re-search-forward "{")
            (backward-char)
            (forward-sexp)
            ;; point should be at block end
            (when (< block-start-pos original-pos (point))
              (list block-indent-column
                    "set"
                    block-start-pos
                    (point)))))))))


(defun seed7-line-inside-logic-check-expression (n &optional
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)
  "Check if line N is inside a logic check expression.
Return the indentation column of the space following the check keyword
if line N is inside an array block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If SCOPE-BEGIN-POS is non-nil specified, SCOPE-BEGIN-POS and
SCOPE-END-POS are boundary positions identifying the beginning and end
of scope where to search."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-logic-check-expression: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((current-pos (point))
             (start-pos (seed7-to-previous-line-starts-with
                         "\\(?:while\\|if\\|elsif\\|when\\)[[:blank:]]"
                         scope-begin-pos))
             (keyword  (and start-pos
                            (seed7--current-line-nth-word 1)))
             (end-re (cond
                      ((string= keyword "while") " do")
                      ((member  keyword '("if" "elsif")) " then")
                      ((string= keyword "when")  ":")
                      (t nil))))
        (when end-re
          (when (seed7-re-search-forward end-re scope-end-pos)
            (when (< start-pos current-pos (point))
              (goto-char start-pos)
              (skip-chars-forward " \t")
              (+ (current-column) (length keyword) 1))))))))

;; [:todo 2025-05-22, by Pierre Rouleau: is this handling comment?]
(defun seed7-line-inside-assign-statement-continuation (n
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

(defun seed7-line-at-endof-set-definition-block (n &optional
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
             (enclosing-block-end-pos nil))
        (when (seed7-re-search-backward "};")
          (forward-char)
          (setq enclosing-block-end-pos (point))
          (backward-sexp)
          (seed7-to-indent)
          (when (looking-at-p "\\(?:const\\|var\\) +?set +?.+?:.+?{")
            (setq block-start-pos (point))
            (when (< block-start-pos line-start-pos enclosing-block-end-pos line-end-pos)
              block-indent-column)))))))

(defun seed7-line-inside-parens-pair (n &optional
                                        scope-begin-pos
                                        scope-end-pos
                                        dont-skip-comment-start)
  "Check if line N is inside a parens pair.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If SCOPE-BEGIN-POS is non-nil specified, SCOPE-BEGIN-POS and
SCOPE-END-POS are boundary positions identifying the beginning and end
of scope where to search.

If nothing is found it returns nil.
If the appropriate parens pair is found it returns a list of 4 elements:
- 0: indentation column of the character after the opening parens
- 1: string: parens pair found.
- 2: position of the opening paren
- 3: position of the end paren."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-parens-pair: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
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
              (setq start-pos (seed7-backward-char-pos open-paren scope-begin-pos))
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
                      (if (and (< (or scope-begin-pos 0)
                                  start-pos line-start end-pos)
                               (or (not scope-end-pos)
                                   (< end-pos scope-end-pos)))
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
          (list (1+ (current-column))
                (nth 0 (nth 0 candidate-list))
                (nth 1 (nth 0 candidate-list))
                (nth 2 (nth 0 candidate-list))))))))


(defun seed7-line-inside-parens-pair-column (n &optional
                                               scope-begin-pos
                                               scope-end-pos
                                               dont-skip-comment-start)
  "Check if line N is inside a parens pair.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If SCOPE-BEGIN-POS is non-nil specified, SCOPE-BEGIN-POS and
SCOPE-END-POS are boundary positions identifying the beginning and end
of scope where to search.

If nothing is found it returns nil.
If the appropriate parens pair is found it returns the indentation column
of the character after the opening parens."
  (car-safe (seed7-line-inside-parens-pair n
                                           scope-begin-pos
                                           scope-end-pos
                                           dont-skip-comment-start)))

(defun seed7-line-inside-nested-parens-pairs (n nested-depth
                                                &optional
                                                scope-begin-pos
                                                scope-end-pos
                                                dont-skip-comment-start)
  "Check if line N is inside NESTED-DEPTH levels of parens pairs.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If nothing is found it returns nil.
If the appropriate parens pair is found it returns a list of 4 elements:
- 0: indentation column of the character after the opening parens of
     the inner-most nesting.
- 1: string: parens pair found.
- 2: position of the opening paren of the inner-most nesting.
- 3: position of the end paren of the the inner-most nesting."
  (save-excursion
    (let ((inner-most-nesting-spec-list nil)
          (spec-list nil))
      (while (and (> nested-depth 0)
                  (seed7--set
                   (seed7-line-inside-parens-pair n
                                                  scope-begin-pos
                                                  scope-end-pos
                                                  dont-skip-comment-start)
                   spec-list))
        (setq nested-depth (1- nested-depth)
              ;; on next iteration don't move from the new position
              n :dont-move)
        ;; the new position is the start paren just found
        (goto-char (nth 2 spec-list))
        ;; remember information of the inner-most nesting
        (unless inner-most-nesting-spec-list
          (setq inner-most-nesting-spec-list spec-list)))
      (when (eq nested-depth 0)
            inner-most-nesting-spec-list))))

(defun seed7-line-inside-nested-parens-pairs-column (n nested-depth
                                                       &optional
                                                       scope-begin-pos
                                                       scope-end-pos
                                                       dont-skip-comment-start)
  "Check if line N is inside NESTED-DEPTH levels of parens pairs.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If nothing is found it returns nil.
If the appropriate parens pair is found it return the indentation column of
the character after the opening parens of the the inner-most nesting."
  (car-safe (seed7-line-inside-nested-parens-pairs n nested-depth
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)))

;; [:todo 2025-05-30, by Pierre Rouleau: test the following with/without comments]
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
                     (not (looking-at-p "\n"))))
          (setq found t))))
    (skip-chars-forward " \t")
    (current-column)))


(defun seed7-line-is-defun-end (n &optional dont-skip-comment-start)
  "Check if line N is below the end of a func, proc, struc or enum block.
If that is the case, return the indentation column of the func, proc, struct
or enum definition, otherwise return nil.
if line N is below the end of a func definition block, nil otherwise.
N is: - :previous-non-empty for the previous non empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (let ((previous-defun-column nil)
        (end-pos (save-excursion (end-of-line) (point))))
    (cond
     ;; handle line that is a end func, struc or enum
     ((seed7-line-starts-with-any n
                                  (list
                                   "end[[:blank:]]+?func[[:blank:]]*?;"
                                   "end[[:blank:]]+?struct[[:blank:]]*?;"
                                   "end[[:blank:]]+?enum[[:blank:]]*?;")
                                  dont-skip-comment-start)
      (save-excursion
        (seed7-move-to-line n)
        (seed7-to-block-backward nil :dont-push-mark)
        (seed7-to-indent)
        (current-column)))
     ;; handle forward and native action declarations of func and proc
     ((seed7--set (seed7-line-starts-with-any
                   n
                   (list
                    seed7-forward-or-action-function-declaration-re
                    seed7---inner-callables-4
                    seed7-forward-or-action-procedure-declaration-re)
                   dont-skip-comment-start
                   end-pos)
                  previous-defun-column)
      previous-defun-column)
     ;; Line N is not a end func; struc or enum.
     ;; It might be another block such as a function forward or action
     ;; declaration. To check that, use the `seed7-beg-of-defun' and
     ;; `seed7-end-of-defun' to check if they match.
     (t
      (save-excursion
        (seed7-move-to-line n)
        (let ((endline-pos nil)
              (endline-pos-2 nil))
          (end-of-line)
          (setq endline-pos (point))
          (when (ignore-errors
                  (seed7-beg-of-defun nil :silent :dont-push-mark)
                  (setq previous-defun-column (current-column)))
            (setq endline-pos-2 (ignore-errors
                                  (seed7-end-of-defun nil :silent
                                                      :dont-push-mark)
                                  (point)))
            (when (eq endline-pos endline-pos-2)
              previous-defun-column))))))))

;;** Seed7 Indentation Comment Checking Function
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-comment-column (recurse-count)
  "Return the column number for comment start or continuation.
The RECURSE-COUNT argument should be 0 on first call, incremented by 1 on each
recursive call."
  (save-excursion
    ;; What type of comment is this line ?
    (seed7-to-indent)
    (cond
     ;; If at the beginning of a new comment
     ((and (or (bobp)
               (seed7-line-starts-with -1 "#")
               (not (seed7-inside-comment-p (- (point) 1))))
           (or  (looking-at-p "#")
                (looking-at-p "(\\*")))
      (if (seed7-line-code-ends-with :previous-non-empty ";")
          (let ((spec-list nil))
            ;; if just below a closed when case, leave the comment at the
            ;; same level as the when keyword
            (if (and (seed7-line-starts-with :previous-non-empty
                                             "when[[:blank:]]")
                     (seed7--set
                      (seed7-line-inside-a-block :previous-non-empty)
                      spec-list)
                     (string= (nth 1 spec-list) "case "))
                (nth 0 spec-list)
              ;; if there are (other) statements above, line up the comment
              ;; according to the nature of the previous line as if the
              ;; current line was not a comment: re-use the logic of
              ;; `seed7-calc-indent' to get the indentation.  Only 1 level of
              ;; recursion should be necessary (and allowed).
              (condition-case nil
                  (seed7-calc-indent
                   :treat-comment-line-as-code
                   (1+ recurse-count))
                ;; If no rule was found for the code, force the indentation to
                ;; 0 as if there was no statements above.
                (error 0))))
        ;; If there are no statements above indent a column 0.
        (condition-case nil
            (seed7-calc-indent
             :treat-comment-line-as-code
             (1+ recurse-count))
          ;; If no rule was found for the code, force the indentation to
          ;; 0 as if there was no statements above.
          (error 0))))

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
When TREAT-COMMENT-LINE-AS-CODE is non-nil a comment line is processed as if
  it was a code line, allowing the indentation logic to handle comments.
The RECURSE-COUNT should be nil on the first call, 1 on the first recursive
  call.  Only one recursion is allowed."
  (let ((recurse-count (or recurse-count 0))
        (indent-step (seed7-line-indent-step :previous-non-empty))
        (first-word-on-line      (seed7--current-line-nth-word 1))
        (indent-column nil)
        (indent-column2 nil)
        (spec-list nil))
    (cond
     ((> recurse-count 1)
      (error "Recursion done more than once: implementation logic error!"))

     ((and (seed7-current-line-start-inside-comment-p)
           (not treat-comment-line-as-code))
      (setq indent-column (seed7-comment-column recurse-count)))

     ;; In a array or set definition, indent 1 level unless the line is
     ;; inside 2 nested parens.  In that case align with the inside of
     ;; the inner-most parens.
     ((or (seed7--set (seed7-line-inside-array-definition-block 0)
                      spec-list)
          (seed7--set (seed7-line-inside-set-definition-block 0)
                      spec-list))
      (if (seed7--set (seed7-line-inside-nested-parens-pairs-column
                       0 2
                       (nth 2 spec-list)
                       (nth 3 spec-list))
                      indent-column2)
          (setq indent-column indent-column2)
        (setq indent-column (+ (nth 0 spec-list)
                               seed7-indent-width))))

     ((seed7-line-isa-string 0)
      (save-excursion
        ;; if previous line starts with a string, align the string to it.
        (cond
         ((seed7--set (seed7-line-inside-parens-pair-column 0)
                      indent-column))
         ((seed7-line-isa-string :previous-non-empty)
          (forward-line -1)
          (search-forward "\"")
          (setq indent-column (1- (current-column))))
         (t
          (message
           "At line %d: string line syntax not yet supported! Please report."
           (seed7-current-line-number))))))

     ;; Special rule: if a line starts with a Seed7 assignment operator,
     ;; consider that line manually indented and keep it where it is.
     ;; This allows a long multi-line statement to be lined up on the
     ;; assignment operator placed on the line after its rvalue for the
     ;; explicit purpose of allowing that manual alignment mechanism.
     ((seed7--set (seed7-line-starts-with
                   0
                   seed7-predef-assignment-operator-regxp)
                  indent-column))
     ((seed7--set (seed7-line-inside-assign-statement-continuation 0)
                  indent-column))

     ;; Handle special cases before checking if line is inside a block
     ;; --------------------------------------------------------------
     ;; Check if line is below end of func|struct|enum before checking if it
     ;; is inside a block and is not itself a 'end func|struct|enum;' line.
     ;; This ensures it handles the next line properly.
     ((and (not (seed7-line-is-defun-end 0))
           (seed7--set (seed7-line-is-defun-end :previous-non-empty)
                       indent-column)))

     ;; Also perform some tests that are fast to execute.
     ((string= first-word-on-line "$")
      (setq indent-step 0))
     ((string= first-word-on-line "include")
      (cond
       ((seed7--set (seed7-line-starts-with
                     :previous-non-empty "include ")
                    indent-column))
       ((seed7-line-starts-with :previous-non-empty "$ include ")
        (setq indent-step 1))
       (t (setq indent-step 0))))

     ((seed7--set (seed7-line-inside-a-block 0) spec-list)
      ;; Inside a block.  Check if inside any special zones first.
      ;; For all of those extra checks limit the zone to the scope of the
      ;; current block to improve efficiency. Extend the boundary by 1
      ;; character to allow searches to succeed if they match at the edges.
      (let ((begin-pos (1- (nth 2 spec-list)))
            (end-pos   (1+  (nth 3 spec-list))))
        (cond
         ((seed7--set                   ; inside parens pair?
           (seed7-line-inside-parens-pair-column 0 begin-pos end-pos)
           indent-column))
         ((seed7--set                   ; inside logic expression?
           (seed7-line-inside-logic-check-expression 0 begin-pos end-pos)
           indent-column))
         ((seed7--set                   ; inside argument list?
           (seed7-line-inside-proc-argument-list-section 0 begin-pos end-pos)
           indent-column)
          (setq indent-column (+ indent-column seed7-indent-width)))
         ((seed7--set                   ; inside until  ...; area?
           (seed7-line-inside-until-logic-expression 0 begin-pos end-pos)
           indent-column))
         ((seed7--set                   ; inside func return?
           (seed7-line-inside-func-return-statement 0 begin-pos end-pos)
           indent-column)
          (setq indent-column (+ indent-column seed7-indent-width)))
         ;; Not inside any special zones.
         ;; Use indentation identified by `seed7-line-inside-a-block'.
         (t (setq indent-column (nth 0 spec-list))))))
     ;; Outside of block.

     ;; Just after end of array or set definition blocks.
     ((or (seed7--set (seed7-line-at-endof-array-definition-block
                       :previous-non-empty)
                      indent-column)
          (seed7--set (seed7-line-at-endof-set-definition-block
                       :previous-non-empty)
                      indent-column))
      (setq indent-column (- indent-column seed7-indent-width)))

     ;; When inside a paren block, adjust indent to the column
     ;; following the open paren; any of: ( { [ <
     ((seed7--set (seed7-line-inside-parens-pair-column 0)
                  indent-column))
     ((and (string= first-word-on-line "end")
           (string= (seed7--current-line-nth-word 2) "block"))
      (setq indent-step (- indent-step 2)))
     ((string= first-word-on-line "until")
      (setq indent-step (1- indent-step)))

     ((and (string= first-word-on-line "var")
           (seed7-line-starts-with :previous-non-empty "include "))
      (setq indent-step 0))

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


(defun seed7--indent-one-line ()
  "Utility: indent the current Seed7 line of code."
  (seed7-to-indent)
  (let ((indent (seed7-calc-indent)))
    (when (not (= indent (current-column)))
      (forward-line 0)
      (delete-horizontal-space)
      (unless (eq indent 0)
        (indent-to indent)))))

(defun seed7-indent-line ()
  "Indent the current Seed7 line of code or all marked lines.
If point was inside the indentation space move it to first non white space,
otherwise leave point over the same character.
If a region is marked, use it to identify the lines that must be indented,
then deactivates it (to prevent the area to limit searches)."
  (interactive "*")
  (let ((move-point (seed7-inside-line-indent-p)))
    (save-excursion
      (if (use-region-p)
          ;; region active: indent complete region
          (let ((line-count (count-lines (region-beginning) (region-end))))
            (goto-char (region-beginning))
            (deactivate-mark)
            (dotimes (_ line-count)
              (seed7--indent-one-line)
              (forward-line 1)))
        ;; no region: indent current line
        (seed7--indent-one-line)))
    (when move-point
      (seed7-to-indent))))

;; ---------------------------------------------------------------------------
;;* Seed7 Code Template Expansion
;;  =============================

(defun seed7--delete-char-and-mark ()
  "Delete 1 character and put a tempo market at it's position."
  (delete-char 1)
  (tempo-insert-mark (point-marker)))

(defun seed7--delete-char-and-mark-at-column (indented-column)
  "Delete 1 char at specified INDENTED-COLUMN number.
Also add a tempo marker at that location."
  (seed7-to-indent)
  (when (> indented-column 0)
    (forward-char indented-column))
  (seed7--delete-char-and-mark))

(defun seed7--delete-char-and-mark-at (indented-column)
  "Delete 1 char at specified INDENTED-COLUMN number or each one in the list.
Also add tempo marker at each of these locations."
  (if (listp indented-column)
      (dolist (col indented-column)
        (seed7--delete-char-and-mark-at-column col))
    (seed7--delete-char-and-mark-at-column indented-column)))

;; --

(defun seed7--indent-lines (n)
  "Indent N lines starting from the current one.  Do not move point."
  (save-excursion
    (dotimes (_ n)
      (indent-for-tab-command)
      (forward-line 1))))
;; --

(defun seed7-insert-include ()
  "Insert the file include."
  (interactive "*")
  (insert "include \".s7i\"")
  (seed7-to-indent)
  (forward-char 9)
  (save-excursion
    (indent-for-tab-command)))

(defun seed7-insert-procedure-declaration ()
  "Insert the code template for a procedure declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  ;; insert a template with a temporary name N to allow auto-indent to work.
  (insert "const proc: N (A) is func\n local\n V\n begin\n C\n end func;")
  (forward-line -5)
  (forward-char 12)
  (seed7--indent-lines 6)
  (save-excursion
    (seed7--delete-char-and-mark-at '(15 12))
    (dotimes (_ 2)
      (forward-line 2)
      (seed7--delete-char-and-mark-at 0))))

(defun seed7-insert-func-declaration ()
  "Insert the code template for a function declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  ;; Insert template with N & T markers for name and type to allow auto-indent
  ;; to work, then remove them and leave point at the function name.
  (insert "const func T: N (A) is func\n result\n V \n local\n V\n begin\n C\n end func;")
  (forward-line -7)
  (forward-char 11)
  (seed7--indent-lines 8)
  (save-excursion
    (seed7--delete-char-and-mark-at '(17 14 11)) ; T N A
    (dotimes (_ 3)
      (forward-line 2)
      (seed7--delete-char-and-mark-at 0))))

(defun seed7-insert-short-function-declaration ()
  "Insert the code template for a short function declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "const func T: N (A) is\n return R;")
  (forward-line -1)
  (forward-char 11)
  (seed7--indent-lines 2)
  (save-excursion
    (seed7--delete-char-and-mark-at '(17 14 11)) ; T N A
    (forward-line 1)
    (seed7--delete-char-and-mark-at 7)))

(defun seed7-insert-enumeration-type-declaration ()
  "Insert the code template for a enumeration declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "const type: T is new enum\n V,\n end enum;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 12)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at 12)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-struct-type-declaration ()
  "Insert the code template for a structure declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "const type: T is new struct\n var V: N is v;\n end struct;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 12)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at 12)
    (forward-line 1)
    (seed7--delete-char-and-mark-at '(12 7 4))))

(defun seed7-insert-if-statement ()
  "Insert a if statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "if C then\n A\n end if;" )
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 3)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at 3)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-if-else-statement ()
  "Insert a if statement with an else clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "if C then\n A\n else\n B\n end if;" )
  (forward-line -4)
  (seed7-to-indent)
  (forward-char 3)
  (seed7--indent-lines 5)
  (save-excursion
    (seed7--delete-char-and-mark-at 3)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)
    (forward-line 2)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-if-elsif-statement ()
  "Insert a if statement with an elsif clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "if C then\n A\n elsif C then\n B\n end if;" )
  (forward-line -4)
  (seed7-to-indent)
  (forward-char 3)
  (seed7--indent-lines 5)
  (save-excursion
    (seed7--delete-char-and-mark-at 3)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 6)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-if-elsif-else-statement ()
  "Insert a if statement with an elsif and an else clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "if C then\n A\n elsif C then\n B\n else\n C\n end if;")
  (forward-line -6)
  (seed7-to-indent)
  (forward-char 3)
  (seed7--indent-lines 7)
  (save-excursion
    (seed7--delete-char-and-mark-at 3)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 6)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)
    (forward-line 2)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-case-statement ()
  "Insert a case statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "case V of\n when C:\n E\n end case;")
  (forward-line -3)
  (seed7-to-indent)
  (forward-char 5)
  (seed7--indent-lines 4)
  (save-excursion
    (seed7--delete-char-and-mark-at 5)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 5)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for ()
  "Insert a for statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for I range N to M do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(17 12 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-until ()
  "Insert a for-until statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V range N to N until C do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(25 17 12 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-step ()
  "Insert a for-step statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V range N to N step N do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(24 17 12 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-each ()
  "Insert a for-each statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V range L do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(12 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-each-until ()
  "Insert a for-each-until statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V range L until C do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(20 12 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-each-key ()
  "Insert a for-each-key statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V key I range L do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(18 10 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-each-key-until ()
  "Insert a for-each-key-until statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for V key I range L until C do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 4)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(26 18 10 4))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-key ()
  "Insert a for-key statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for key I range L do\n E\n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 8)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(16 8))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-for-key-until ()
  "Insert a for-key-until statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "for key I range L until C do\n \n end for;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 8)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at '(24 16 8))
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

(defun seed7-insert-repeat ()
  "Insert a repeat statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "repeat\n E\n until C;")
  (seed7-to-indent)
  (forward-char 6)
  (save-excursion
    (forward-line -2)
    (seed7--indent-lines 3)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 6)))

(defun seed7-insert-while ()
  "Insert a while statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "while C do\n E\n end while;")
  (forward-line -2)
  (seed7-to-indent)
  (forward-char 6)
  (seed7--indent-lines 3)
  (save-excursion
    (seed7--delete-char-and-mark-at 6)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0)))

;; https://seed7.net/manual/errors.htm#Handlers
(defun seed7-insert-block ()
  "Insert a block handler statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "block\n E\n exception\n catch X:\n H\n end block;")
  (forward-line -4)
  (seed7-to-indent)
  (save-excursion
    (forward-line -1)
    (seed7--indent-lines 6))
  (save-excursion
    (seed7--delete-char-and-mark-at 0)
    (forward-line 2)
    (seed7--delete-char-and-mark-at 6)
    (forward-line 1)
    (seed7--delete-char-and-mark-at 0))
  (end-of-line))

(defun seed7-insert-global ()
  "Insert a global block statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "global\n E\n end global;")
  (forward-line -1)
  (seed7-to-indent)
  (save-excursion
    (forward-line -1)
    (seed7--indent-lines 3))
  (save-excursion
    (seed7--delete-char-and-mark-at 0))
  (end-of-line))

(defun seed7-insert-var-declaration ()
  "Insert a variable declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "var T: N is V;")
  (seed7-to-indent)
  (forward-char 4)
  (save-excursion
    (indent-for-tab-command)
    (seed7--delete-char-and-mark-at '(12 7 4))))

(defun seed7-insert-const-declaration ()
  "Insert a constant declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "const T: N is V;")
  (seed7-to-indent)
  (forward-char 6)
  (save-excursion
    (indent-for-tab-command)
    (seed7--delete-char-and-mark-at '(14 9 6))))

(defun seed7-insert-in-parameter ()
  "Insert declaration of in parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "in T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

(defun seed7-insert-invar-parameter ()
  "Insert declaration of invar parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "in var T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

(defun seed7-insert-inout-parameter ()
  "Insert declaration of inout parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "inout T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

(defun seed7-insert-reference-parameter ()
  "Insert declaration of ref parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "ref T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

(defun seed7-insert-value-parameter ()
  "Insert declaration of value parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "val T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

(defun seed7-insert-call-by-name-parameter ()
  "Insert declaration of call by name parameter.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-forward-mark] to move to previous one."
  (interactive "*")
  (insert "in func T: N")
  (backward-char 1)
  (seed7--delete-char-and-mark)
  (backward-char 3)
  (seed7--delete-char-and-mark))

;; --

(defun seed7--delete-backward (n)
  "Delete N characters before point."
  (backward-char n)
  (delete-char n))

(defun seed7-complete-statement-or-indent ()
  "Adjust indentation of current line or block.
Expand statement if point follows specific keyword located at the beginning of
a code line.  The supported keywords are:

============ =========================================================
Keyword      Expansion
============ =========================================================
inc          include statement
proc         procedure declaration
func         function declaration
funcs        short function declaration
const        constant declaration
var          variable declaration
in           declaration of an in parameter
inout        declaration of an inout parameter
invar        declaration of an in-var parameter
callbn       declaration of a call-by-name parameter
ref          declaration of a reference parameter
val          declaration of a value parameter
bl           block (handler statement)
gl           global block
case         case statement
if           if statement
ife          if-else statement
ifei         if-elsif statement
ifeie        if-elsif-else statement
repeat       repeat statement
while        while statement
for          for statement
foru         for-until statement
fors         for-step statement
fore         for-each statement
foreu        for-each statement combined with an until condition,
forek        for-each-key statement
foreku       for-each-key statement combined with an until condition,
fork         for-key statement
forku        for-key statement combined with an until condition

enum         enum type definition
struct       struct type definition
============ ========================================================="
  (interactive "*")
  (let ((keyword nil)
        (col-at-keyword-beg nil)
        (in-indent nil))
    (save-excursion
      (backward-word)
      (setq col-at-keyword-beg (current-column)
            in-indent (seed7-inside-line-indent-p)
            keyword (thing-at-point 'word :no-properties)))
    ;; expand only if there's 1 word at the beginning of a line of code,
    ;; with nothing after and only for specified predefined keywords
    ;; or inside parens, just before the closing parens
    (if (and (not (use-region-p))
             keyword
             (not (seed7-inside-comment-p))
             (not (seed7-inside-string-p))
             (or (and (or in-indent
                          (eq col-at-keyword-beg 0))
                      (or (looking-at-p "\n")
                          (eobp))
                      (member keyword '("inc"
                                        "const" "var"
                                        "enum" "struct"
                                        "proc"
                                        "func" "funcs"
                                        "if" "ife" "ifei" "ifeie"
                                        "case"
                                        "for"   "foru"
                                        "fors"
                                        "fore"  "foreu"
                                        "forek" "foreku"
                                        "fork"  "forku"
                                        "repeat"
                                        "while"
                                        "bl"
                                        "gl")))
                 (and (looking-at-p " ?)")
                      (member keyword '("in"
                                        "invar"
                                        "inout"
                                        "ref"
                                        "val"
                                        "callbn")))))
        (progn
          (when seed7-template-expansion-disables-overwrite-mode
            (overwrite-mode 0))
          (cond
           ((string= keyword "if")
            (seed7--delete-backward 2)
            (seed7-insert-if-statement))
           ((string= keyword "ife")
            (seed7--delete-backward 3)
            (seed7-insert-if-else-statement))
           ((string= keyword "ifei")
            (seed7--delete-backward 4)
            (seed7-insert-if-elsif-statement))
           ((string= keyword "ifeie")
            (seed7--delete-backward 5)
            (seed7-insert-if-elsif-else-statement))
           ((string= keyword "case")
            (seed7--delete-backward 4)
            (seed7-insert-case-statement))
           ((string= keyword "for")
            (seed7--delete-backward 3)
            (seed7-insert-for))
           ((string= keyword "foru")
            (seed7--delete-backward 4)
            (seed7-insert-for-until))
           ((string= keyword "fors")
            (seed7--delete-backward 4)
            (seed7-insert-for-step))
           ((string= keyword "fore")
            (seed7--delete-backward 4)
            (seed7-insert-for-each))
           ((string= keyword "foreu")
            (seed7--delete-backward 5)
            (seed7-insert-for-each-until))
           ((string= keyword "forek")
            (seed7--delete-backward 5)
            (seed7-insert-for-each-key))
           ((string= keyword "foreku")
            (seed7--delete-backward 6)
            (seed7-insert-for-each-key-until))
           ((string= keyword "fork")
            (seed7--delete-backward 4)
            (seed7-insert-for-key))
           ((string= keyword "forku")
            (seed7--delete-backward 5)
            (seed7-insert-for-key-until))
           ((string= keyword "repeat")
            (seed7--delete-backward 6)
            (seed7-insert-repeat))
           ((string= keyword "while")
            (seed7--delete-backward 5)
            (seed7-insert-while))
           ((string= keyword "bl")
            (seed7--delete-backward 2)
            (seed7-insert-block))
           ((string= keyword "gl")
            (seed7--delete-backward 2)
            (seed7-insert-global))

           ((string= keyword "proc")
            (seed7--delete-backward 4)
            (seed7-insert-procedure-declaration))
           ((string= keyword "func")
            (seed7--delete-backward 4)
            (seed7-insert-func-declaration))
           ((string= keyword "funcs")
            (seed7--delete-backward 5)
            (seed7-insert-short-function-declaration))

           ((string= keyword "var")
            (seed7--delete-backward 3)
            (seed7-insert-var-declaration))
           ((string= keyword "const")
            (seed7--delete-backward 5)
            (seed7-insert-const-declaration))
           ((string= keyword "in")
            (seed7--delete-backward 2)
            (seed7-insert-in-parameter))
           ((string= keyword "invar")
            (seed7--delete-backward 5)
            (seed7-insert-invar-parameter))
           ((string= keyword "inout")
            (seed7--delete-backward 5)
            (seed7-insert-inout-parameter))
           ((string= keyword "ref")
            (seed7--delete-backward 3)
            (seed7-insert-reference-parameter))
           ((string= keyword "val")
            (seed7--delete-backward 3)
            (seed7-insert-value-parameter))
           ((string= keyword "callbn")
            (seed7--delete-backward 6)
            (seed7-insert-call-by-name-parameter))

           ((string= keyword "inc")
            (seed7--delete-backward 3)
            (seed7-insert-include))
           ((string= keyword "enum")
            (seed7--delete-backward 4)
            (seed7-insert-enumeration-type-declaration))
           ((string= keyword "struct")
            (seed7--delete-backward 6)
            (seed7-insert-struct-type-declaration))))

      ;; not on keyword; just indent
      (seed7-indent-line))))

;; ---------------------------------------------------------------------------
;;* Seed7 Compilation
;;  =================
;;

(defun seed7-compile (&optional compile)
  "Static check current Seed7 file, show errors in `compilation-mode' buffer.

If optional COMPILE argument set, compile the file to executable instead."
  (interactive "P")
  (compile (format "%s %s"
                   (if compile seed7-compiler seed7-checker)
                   (shell-quote-argument (buffer-file-name)))))

;; ---------------------------------------------------------------------------
;;* Seed7 Cross Reference
;;  =====================
;;
;; The `seed7-mode` supports the xref framework introduced in Emacs 25: the
;; code implements a xref framework compliant back-end using the s7xref Seed7
;; program that parses Seed7 program and library files to extract all required
;; information about global variable, functions and procedures which includes
;; all operators (both word and special operators).
;;
;; Therefore to use this feature you must have the Seed7 compiler and
;; interpreter installed in your system and the s7xref.sd7 file available in
;; the seed7-mode/tools directory installed.  You can use it as is with the s7
;; interpreter or compile it with the s7c compiler.  The `seed7-xref' user
;; option must identify the appropriate information that will allow execution
;; of the program.
;;
;; Once this is done, you will be able yo use the various xref commands with
;; the xref front-end of your choice, the default being xref own front-end,
;; but also with the helm, ivy or other xref front-ends.

(defvar-local seed7---xref-buffer nil
  "Internal/hidden buffer holding cross-reference info for visited Seed7 file.")

(defun seed7--build-xref ()
  "Build a cross reference buffer for the current Seed7 file.
The buffer holds 1 line per object referenced.
Each line holds 3 tab-separated elements:
- object name,
- name of file where object is defined,
- line number in file where the object is defined.

This uses the Seed7 cross reference tool identified by the `seed7-xref'
user-option."
  (let* ((xref-uo-list (split-string seed7-xref))
        (xref-executable-name (executable-find (car-safe xref-uo-list))))
    (if (and xref-executable-name
             (file-executable-p xref-executable-name))
      (let* ((sd7-source-fname-with-path
              (expand-file-name buffer-file-truename))
             (fbasename (file-name-sans-extension
                         (file-name-nondirectory
                          buffer-file-truename)))
             (outbuf (or (and seed7---xref-buffer
                              (buffer-live-p seed7---xref-buffer))
                         (setq-local
                          seed7---xref-buffer
                          (get-buffer-create
                           ;; Create a hidden buffer by using a leading
                           ;; space in its name.
                           (format " *s7xref-for-%s*" fbasename))))))
        ;; In case the command was executed before, erase prior content
        (with-current-buffer outbuf
          (erase-buffer))
        (if (eq (length xref-uo-list) 1)
            ;; seed7-xref is just 1 word, the name of the xref program
            (call-process xref-executable-name
                          nil outbuf nil
                          sd7-source-fname-with-path)
          ;; seed7-xref has more than 1 word.  The first word is the program,
          ;; and the following words are its arguments.  For instance the
          ;; program name could be s7 and the next word a Seed7 source file to
          ;; interpret.  And there could be other options.  Pass them all to
          ;; the `call-process' as args.
          ;; Since we do not use the shell, the file paths *must* be expanded.
          (let ((args (list sd7-source-fname-with-path)))
            (if (string= (file-name-nondirectory (car xref-uo-list)) "s7")
                ;; If Seed7 interpreter is used, ensure that the second element
                ;; is a fully expanded file name that exists.  If it exists
                ;; prepend the fully expanded file name to args.
                (progn
                  (setq xref-uo-list (cdr xref-uo-list))
                  (let ((sd7-script-fn
                         (expand-file-name (car-safe xref-uo-list))))
                    (if (and sd7-script-fn (file-exists-p sd7-script-fn))
                        (progn
                          ;; remove script filename from the list
                          (setq xref-uo-list (cdr-safe xref-uo-list))
                          ;; prepend scripts args if any
                          (when xref-uo-list
                            (setq args (append xref-uo-list args)))
                          ;; then prepend script file-name
                          (setq args (append (list sd7-script-fn) args)))
                      (user-error
                       "Invalid seed7-xref: %s\nseed7-xref = %s"
                       (if sd7-script-fn
                           (format "Can't find script: %s" sd7-script-fn)
                         "No Seed7 file identified after s7 interpreter.")
                       seed7-xref))))
              ;; The command is not a s7 command but something else.
              ;; We know the first word is a valid executable, just proceed
              ;; by setting pre-pending the cdr of seed7-xref list.
              (when (> (length xref-uo-list) 1)
                (setq args (append (cdr xref-uo-list) args))))
            ;; Execute the command with appropriate arguments.
            (apply #'call-process
                   xref-executable-name
                   nil outbuf nil
                   args))))
      (user-error "\
The seed7-xref user-option does not identify an executable file: %s
Please update!"
                  seed7-xref))))


;; [:todo 2025-06-20, by Pierre Rouleau: Add support for multi-line signatures]
(defun seed7--signature-at (&optional pos)
  "Return Seed7 element signature for element at point or POS."
  (when pos (goto-char pos))
  (substring-no-properties (buffer-substring (line-beginning-position) (line-end-position)))

  ;; previous failed attempt at multi-line signatures:
  ;;
  ;; (let ((original-pos (point))
  ;;       (original-column (current-column)))
  ;;   (when (re-search-forward seed7-procfunc-regexp nil :noerror)
  ;;     (forward-line 0)
  ;;     (forward-char original-column)
  ;;     (if (eq original-pos (point))
  ;;         (substring-no-properties (match-string 0))
  ;;       ;; the search led to something else, don't use it; use the line
  ;;       (goto-char original-pos)
  ;;       (buffer-substring (line-beginning-position) (line-end-position)))))
  )

(defun seed7--signature-from (filename line column)
  "Return Seed7 element signature for element in FILENAME at LINE, COLUMN."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char column)
    (seed7--signature-at)))

(defun seed7--xref-in-list (list filename lineno)
  "Return t if FILENAME @ LINENO is inside LIST. Return nil otherwise."
  (let ((found nil)
        (entry (car-safe list)))
    (while (and entry
                (not found))
      (when (and (string= (nth 0 entry) filename)
                 (eq (nth 1 entry) lineno))
        (setq found t))
      (setq list (cdr-safe list))
      (setq entry (car-safe list)))
    found))

(defun seed7--local-var-area-for-block (block-spec)
  "Return cons of star and end position of local-begin area of block if any.
Return nil if there is none.
Block specification is in specified BLOCK-SPEC, which is a list returned by
the function `seed7-line-inside-a-block'."
  (save-excursion
    (let ((block-start-pos (nth 2 block-spec))
          (enclosing-block-end-pos   (nth 3 block-spec))
          (block-local-start nil)
          (block-local-end   nil))
      (goto-char block-start-pos)
      ;; look for local-begin or result-begin block
      (when (seed7-re-search-forward
             "^[[:blank:]]+\\(?:local\\>\\|result\\>\\)"
             enclosing-block-end-pos)
        (forward-line 1)
        (setq block-local-start (point))
        (when (seed7-re-search-forward "^[[:blank:]]+begin\\>"
                                       enclosing-block-end-pos)
          (forward-line 0)
          (setq block-local-end (point))))
      (when (and block-local-start block-local-end)
        (cons block-local-start block-local-end)))))

(defun seed7--find-info-about (identifier &optional block-spec)
  "Find information about IDENTIFIER, globally or inside BLOCK-SPEC.

Return a list of 4-element lists, where each 4-element list has:
- The file name where this identifier entry was found.
- The line number integer,
- The column number integer
- A description string (the signature, if found).

This function is used only when the IDENTIFIER is not identified in the output
of s7xref program."
  (save-excursion
    (let ((start-pos nil)
          (end-pos nil)
          (start.end nil))
      (when block-spec
        (setq start.end (seed7--local-var-area-for-block block-spec)
              start-pos (nth 2 block-spec) ; include  argument list or return
              end-pos (cdr start.end)))    ; end when the begin starts
      (goto-char (or start-pos (point-min)))
      (when (search-forward identifier end-pos :noerror)
        (let ((specs nil))
          (when (or (seed7--set (seed7-line-inside-a-block 0) specs)
                    (not block-spec))
              (list (expand-file-name buffer-file-truename)
                    (seed7-current-line-number)
                    (- (current-column) (length identifier))
                    (seed7--signature-at
                     (if block-spec
                         ;; inside-block definition
                         (nth 2 specs)
                       ;; global definition
                       (progn (forward-line 0) (point)))))))))))


(defun seed7--xref-get-from-s7xref (identifier)
  "Get a list of all entries matching IDENTIFIER literally using the s7xref.

Return a list of 4-element lists, where each 4-element list has:
- 0: The file name where this identifier entry was found.
- 1: The line number integer,
- 2: The column number integer
- 3: A description string (the signature, if found, otherwise a replacement)."
  ;; build the xref list for this See7 file if it does not already exist.
  ;; store it inside an internal buffer `seed7---xref-buffer'.
  (unless (and seed7---xref-buffer
               (buffer-live-p seed7---xref-buffer))
    (seed7--build-xref))
  ;; Then search the identifier references in the `seed7---xref-buffer'.
  ;; There may be several entries for a specific identifier and some of them
  ;; may be duplicated.  Filter the duplicate return a list of all candidates.
  ;; Each line of the xref buffer hold a tab-separated set of 3 values: the
  ;; identifier string, the file name and the line number.
  (let ((entries nil)
        (keep-searching t)
        (text-re (format "^\\(%s\\)\t\\(.+?\\)\t\\(.+?\\)$" (regexp-quote
                                                             identifier))))
    (with-current-buffer seed7---xref-buffer
      (goto-char (point-min))
      (while (and keep-searching)
        (not (eobp))
        (if (seed7-re-search-forward text-re)
            (let ((filename (match-string 2))
                  (lineno (string-to-number (match-string 3))))
              (unless (seed7--xref-in-list entries filename lineno)
                (push (list filename
                            lineno
                            0      ; column not identified by s7xref: set to 0
                            (or  (seed7--signature-from filename lineno 0)
                                 (format "No signature for: %s"
                                         (match-string 1))))
                      entries)))
          (setq keep-searching nil))))
    entries))

(defun seed7--xref-get (identifier)
  "Get a list of all entries matching IDENTIFIER literally.
Return a list of 4-element lists, where each 4-element list has:
- 0: The file name where this identifier entry was found.
- 1: The line number integer,
- 2: The column number integer
- 3: A description string (the signature, if found, otherwise a replacement)."
  (let ((point-face (get-char-property (point) 'face))
        (local-block-spec (save-excursion (seed7-to-top-of-block)
                                          (seed7-line-inside-a-block 0)))
        (candidate nil)
        ;; prevent case fold searching: Seed7 is case sensitive.
        (case-fold-search nil))

    (cond
     ((eq point-face 'font-lock-comment-face)
      (user-error "Comments cross reference is not supported."))

     ;; For identifiers, look in local block first, then in the s7xref built
     ;; table and then in the global scope of the current file.
     ((eq point-face 'seed7-name-identifier-face)
      (if (seed7--set
           (seed7--find-info-about identifier local-block-spec)
           candidate)
          (list candidate)
        ;; if nothing found in current block, search at program scope
        ;; using the cross reference list of object created by s7xref
        (let ((entries (seed7--xref-get-from-s7xref identifier)))
          ;; if that also fails to find something, then look into the global
          ;; scope of the current file.
          (unless entries
            (setq entries (list (seed7--find-info-about identifier))))
          entries)))

     ;; for other keywords only look into the xref extracted by s7xref
     (t (seed7--xref-get-from-s7xref identifier)))))

;; [:todo 2025-06-13, by Pierre Rouleau: Should we also skip parens?.]
(defun seed7-operator-at-point ()
  "Return the Seed7 operator at point as a string."
  (save-excursion
    (let ((c nil)
          (start-pos nil))
      ;; move point to whitespace
      (while (and (not (eq (setq c (preceding-char)) ?\s))
                  (not (eq c ?\t))
                  (not (eq c ?\n)))
        (backward-char))
      (setq start-pos (point))
      ;; move point to next whitespace
      (while (and (not (eq (setq c (following-char)) ?\s))
                  (not (eq c ?\t))
                  (not (eq c ?\n)))
        (forward-char))
      (substring-no-properties (buffer-substring start-pos (point))))))

(defun seed7-symbol-at-point ()
  "Return the element at point as a string."
  (if  (and  (looking-at seed7--special-char-re)
             (not (looking-at ";")))
      (seed7-operator-at-point)
    (or (thing-at-point 'symbol t)
        (seed7-operator-at-point))))

(defun seed7--make-xref-from-file-loc (elems)
  "Create an xref object pointing to the given file location.
FILE, LINE, and COLUMN point to the location of the xref,
DESC describes it."
  (xref-make (nth 3 elems)               ; desc
             (xref-make-file-location (nth 0 elems)
                                      (nth 1 elems)
                                      (nth 2 elems))))

(defun seed7--find-symbol (symbol)
  "Get list of xref locations objects for Seed7 SYMBOL."
  ;; First get a list of candidates using the s7xref mechanism.
  (let ((candidates (seed7--xref-get symbol)))
    ;; return a list of xref location objects for those candidates.
    (mapcar (function seed7--make-xref-from-file-loc)
            candidates)))

;;** Seed7 Cross Reference Xref Backend Framework

(defun seed7--xref-backend ()
  "Use the Seed7 backend for Xref in seed7-mode files."
  'seed7)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql seed7)))
  (seed7-symbol-at-point))


(cl-defmethod xref-backend-definitions ((_backend (eql seed7)) symbol)
  (seed7--find-symbol symbol))

;; [:todo 2025-06-16, by Pierre Rouleau: Complete xref support]
;; (cl-defmethod xref-backend-references ((_backend (eql seed7)) symbol)
;;   (seed7--find-uses-of symbol symbol))
;;
;; (cl-defmethod xref-backend-apropos ((_backend (eql seed7)) symbol)
;;   (seed7-find-symbol-apropos symbol))
;;
;; (cl-defmethod
;;     xref-backend-identifier-completion-table ((_backend (eql seed7)))
;;   "Return a list of terms for completions taken from the symbols in the current buffer."
;;   (seed7--list-of-terms)))


;; ---------------------------------------------------------------------------
;;* Seed7 Completion Support
;;  ========================
;; [:todo 2025-06-07, by Pierre Rouleau: add completion support]
;; (defun seed7-completions-at-point)

;; ---------------------------------------------------------------------------
;;* Seed7 Abbreviation Support
;;  ==========================

(defcustom  seed7-support-abbrev-mode t
  "When non-nil, support Seed7-specific abrev-mode abbreviations."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;; All Seed7 system keyword abbreviations are defined in a customizable
;; list, allowing users to modify them if needed.
(defcustom seed7-abbreviations '(
                                 ;; pragmas
                                 (";msg"  "message")
                                 (";in"   "info")
                                 (";tr"   "trace")
                                 (";de"   "decls")
                                 (";na"   "names")
                                 ;; pragmas with optional $
                                 (";syn"  "syntax")
                                 (";sys"  "system")

                                 ;; lead-in-statement-keywords
                                 (";ra"   "raise")
                                 (";rt"   "return")
                                 ;; in-statement-keywords
                                 ;; "is"
                                 (";no"   "noop")
                                 ;; is-statement-keywords
                                 (";fo"   "forward")
                                 (";n"    "new")
                                 (";u"    "sub")
                                 ;; in-middle-statement-keywords
                                 ;; "begin"
                                 ;; "do"
                                 (";dt"   "downto")
                                 (";exc"  "exception")
                                 (";lo"   "local")
                                 (";rg"   "range")
                                 (";rs"   "result")
                                 (";st"   "step")
                                 ;; "then"
                                 ;; "to"
                                 ;; block clause keywords
                                 (";w"    "when")
                                 (";o"    "otherwise")
                                 (";ct"   "catch")
                                 (";ei"   "elsif")
                                 (";e"    "else")

                                 ;; predefined-types
                                 (";a"    "array")
                                 (";bi"   "bigInteger")
                                 (";br"   "bigRational")
                                 (";b3"   "bin32")
                                 (";b6"   "bin64")
                                 (";bt"   "bitset")
                                 (";bo"   "boolean")
                                 (";bs"   "bstring")
                                 (";ca"   "category")
                                 (";c"    "char")
                                 (";cf"   "clib_file")
                                 (";co"   "color")
                                 (";cx"   "complex")
                                 (";db"   "database")
                                 (";du"   "duration")
                                 (";en"   "enum")
                                 (";ex"   "expr")
                                 (";fi"   "file")
                                 (";fs"   "fileSys")
                                 (";fl"   "float")
                                 ;; "func"
                                 (";h"    "hash")
                                 (";i"    "integer")
                                 (";ob"   "object")
                                 ;; "proc"
                                 (";pro"  "process")
                                 (";pr"   "program")
                                 (";rat"  "rational")
                                 (";rf"   "reference")
                                 (";rfl"  "ref_list")
                                 (";s"    "set")
                                 (";sq"   "sqlStatement")
                                 (";sti"  "string")
                                 (";stu"  "struct")
                                 (";tx"   "text")
                                 (";ti"   "time")
                                 (";ty"   "type")
                                 (";v"    "void")
                                 (";pw"   "PRIMITIVE_WINDOW")

                                 ;; predefined-constants
                                 ;; "E"
                                 ;; "EOF"
                                 (";f"    "FALSE")
                                 (";inf"  "Infinity")
                                 ;; "NIL"
                                 ;; "NaN"
                                 ;; "PI"
                                 (";t"    "TRUE")
                                 (";em"   "empty")

                                 ;; predefined-variables
                                 (";ck"   "CONSOLE_KEYBOARD")
                                 (";gk"   "GRAPH_KEYBOARD")
                                 ;; "IN"
                                 (";kb"   "KEYBOARD")
                                 ;; "OUT"
                                 (";sc"   "STD_CONSOLE")
                                 (";se"   "STD_ERR")
                                 (";si"   "STD_IN")
                                 (";sn"   "STD_NULL")
                                 (";so"   "STD_OUT")

                                 ;; errinfo-values
                                 (";ok"   "OKAY_NO_ERROR")
                                 (";me"   "MEMORY_ERROR")
                                 (";ne"   "NUMERIC_ERROR")
                                 (";oe"   "OVERFLOW_ERROR")
                                 (";re"   "RANGE_ERROR")
                                 (";ie"   "INDEX_ERROR")
                                 (";fe"   "FILE_ERROR")
                                 (";dbe"  "DATABASE_ERROR")
                                 (";ge"   "GRAPHIC_ERROR")
                                 (";ae"   "ACTION_ERROR")
                                 (";cre"  "CREATE_ERROR")
                                 (";dse"  "DESTROY_ERROR")
                                 (";ce"   "COPY_ERROR")
                                 (";ine"  "IN_ERROR")

                                 ;; operator-symbols
                                 ;; "and"
                                 ;; "conv"
                                 ;; "digits"
                                 ;; "div"
                                 ;; "exp"
                                 ;; "in"
                                 ;; "lapd0"
                                 ;; "lpad"
                                 ;; "mdiv"
                                 ;; "mod"
                                 ;; "mult"
                                 ;; "not"
                                 ;; "or"
                                 ;; "parse"
                                 ;; "rem"
                                 ;; "rpad"
                                 ;; "sci"
                                 ;; "times"
                                 ;; "varConv"
                                 )
  "List of Seed7-specific abbreviation to expansion.

These abbreviations are made available to the `abbrev-mode' when the
`seed7-support-abbrev-mode' user option is on.

The list included here corresponds to what is documented.
Each entry shows the `abbrev' and its expanded text.
You can add, delete or modify any of these.
Make sure you have no duplication of keywords if you edit the list."
  :group 'seed7
  :type '(repeat
          (list
           (string :tag "abbrev")
           (string :tag "expand"))))

(defvar seed7-mode-abbrev-table nil
  "Abbrev table in use in Seed7 mode buffers.")


(when seed7-support-abbrev-mode
  (define-abbrev-table 'seed7-mode-abbrev-table
    (mapcar
     (lambda (e) (list (car e) (cadr e) nil :system t))
     seed7-abbreviations)
    "Abbrev table for Seed7 mode."
    ;; Accept ; as the first char of an abbrev.  Also allow _ in abbrevs.
    :regexp "\\(?:[^[:word:]_;]\\|^\\)\\(;?[[:word:]_]+\\)[^[:word:]_]*"))

;; ---------------------------------------------------------------------------
;;* Seed7 Key Map
;;  =============
;;

(defvar seed7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'seed7-complete-statement-or-indent)
    (define-key map (kbd "<backtab>") 'tempo-forward-mark)
    (define-key map (kbd "C-c g c")   'seed7-toggle-menu-callable-list)
    (define-key map (kbd "C-c g s")   'seed7-toggle-menu-sorting)
    (define-key map (kbd "C-c C-a") 'seed7-to-block-backward)
    (define-key map (kbd "C-c C-e") 'seed7-to-block-forward)
    (define-key map (kbd "C-c C-n") 'seed7-beg-of-next-defun)
    (define-key map (kbd "C-c C-t") 'seed7-to-top-of-block)
    (define-key map "\M-\C-a"  'seed7-beg-of-defun)
    (define-key map "\M-\C-e"  'seed7-end-of-defun)
    (define-key map "\M-\C-h"  'seed7-mark-defun)
    (define-key map (kbd "C-c ;")  'seed7-toggle-comment-style)
    (define-key map (kbd "C-c v")  'seed7-mode-version)
    (define-key map (kbd "C-c C")  'seed7-mode-customize)
    (define-key map (kbd "C-c C-c") 'seed7-compile)
    map)
  "Keymap used in `seed7-mode'.")

;; ---------------------------------------------------------------------------
;; Seed7 Menu
;; ==========

(easy-menu-define seed7-mode-menu seed7-mode-map
  "Menu for Seed7 Mode."
  '("Seed7"
    ["Version"              seed7-mode-version]
    ("Comments"
     ["Comment/un-comment"     comment-dwim]
     ["Toggle comment style"   seed7-toggle-comment-style])
    ["Customize seed7-mode" seed7-mode-customize]
    "---"
    ["Toggle abbrev-mode"   abbrev-mode]
    ["List abbreviations"   list-abbrevs]
    "---"
    ("Code Template"
     ["Expand keyword/Indent"   seed7-complete-statement-or-indent
      :help "Hit <tab> after any keyword to expand it to code."  ]
     ["Move to next marker"     tempo-forward-mark
      :help "Move to next tempo marker identifying are to fill in code template."
      ]
     ["Move to previous marker" tempo-backward-mark
      :help "Move to previous tempo marker."]
     ("Insert"
      ["Include"            seed7-insert-include]
      "---"
      ["Procedure"          seed7-insert-procedure-declaration]
      ["Function"           seed7-insert-func-declaration]
      ["Function Short"     seed7-insert-short-function-declaration]
      ["Enum"               seed7-insert-enumeration-type-declaration]
      ["Struct"             seed7-insert-struct-type-declaration]
      "---"
      ["var"                seed7-insert-var-declaration]
      ["const"              seed7-insert-const-declaration]
      "---"
      ["in"                 seed7-insert-in-parameter]
      ["invar"              seed7-insert-invar-parameter]
      ["inout"              seed7-insert-inout-parameter]
      ["ref"                seed7-insert-reference-parameter]
      ["val"                seed7-insert-value-parameter]
      ["callbn"             seed7-insert-call-by-name-parameter]
      "---"
      ["Case"               seed7-insert-case-statement]
      ["For"                seed7-insert-for]
      ["For until"          seed7-insert-for-until]
      ["For step"           seed7-insert-for-step]
      ["For each"           seed7-insert-for-each]
      ["For each until"     seed7-insert-for-each-until]
      ["For each key"       seed7-insert-for-each-key]
      ["For each key until" seed7-insert-for-each-key-until]
      ["For key"            seed7-insert-for-key]
      ["For key until"      seed7-insert-for-key-until]
      ["If"                 seed7-insert-if-statement]
      ["Repeat"             seed7-insert-repeat]
      ["While"              seed7-insert-while]
      ["Error handler block" seed7-insert-block]
      ["Global block"       seed7-insert-global]))
    ("Mark"
     ["Mark Function/Procedure" seed7-mark-defun ])
    "---"
    ["Toggle menu listing functions & procedure together"
     seed7-toggle-menu-callable-list]
    ["Toggle listing menu entries in sorted or code order"
     seed7-toggle-menu-sorting]
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
      :help "Go backward to start of block"]
     ["To top of current block" seed7-to-top-of-block
      :help "Go to the top of the current block"])
    "---"
    ["Static check"  seed7-compile
     :help "Perform static analysis of Seed7 code in visited file."]
    ["Compile"       (seed7-compile t)
     :help "Compile Seed7 visited file. Key binding is: C-u C-c C-c"]
    "---"
    ["Customize Mode" (customize-group 'seed7)
     :help "Open the seed7 customization buffer"]))

;; ---------------------------------------------------------------------------
;;* Seed7 Major Mode
;;  ================

;;;###autoload
(define-derived-mode seed7-mode prog-mode "seed7"
  "Major mode for editing Seed7 files."

  ;; Seed7 Font Locking Control
  (setq-local font-lock-defaults '((seed7-font-lock-keywords)))

  ;; Seed7 Mode Syntax Propertize Function
  (setq-local syntax-propertize-function #'seed7-mode-syntax-propertize)

  ;; Seed7 iMenu Support
  (seed7--setup-imenu)

  ;; Seed7 Comments Control
  (seed7--set-comment-style seed7-uses-block-comment)

  ;; Seed7 Code Navigation
  ;; Allow code familiar with the standard `beginning-of-defun' and
  ;; `end-of-defun' to work inside Seed7 buffers.  This includes iedit,
  ;; expand-region, etc...
  (setq-local beginning-of-defun-function
              (function seed7--beg-of-defun-silently))
  (setq-local end-of-defun-function
              (function seed7--end-of-defun-silently))

  ;; Seed7 outline minor-mode support
  (setq-local outline-regexp
              "const \\(type: \\|proc: \\|func \\)")
  (setq-local outline-heading-end-regexp
              "\\( is\\(?:\\ new struct| func\\)?\\)")

  ;; Seed7 Indentation
  (when seed7-auto-indent
    (setq-local indent-line-function (function seed7-indent-line)))

  (when seed7-support-abbrev-mode
    ;; Seed7 Abbreviation Support
    (setq-local local-abbrev-table seed7-mode-abbrev-table))

  ;; Seed7 Cross Reference
  ;; - Use the xref framework : implement a backend for Seed7 here. See above.
  (add-hook 'xref-backend-functions #'seed7--xref-backend nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;;; seed7-mode.el ends here
