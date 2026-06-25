;;; seed7-mode.el --- Support for the Seed7 Programming Language  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Pierre Rouleau

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Maintainer: Pierre Rouleau <prouleau001@gmail.com>
;; URL: https://github.com/pierre-rouleau/seed7-mode
;; Created   : Wednesday, March 26 2025.
;; Version: 0.1
;; Package-Version: 20260624.2305
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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
;; Emacs Compatibility: Emacs >= 25.1
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
;;   characters with auto-fill mode support.   procedure code, argument blocks,
;;                                             logic blocks and code inside
;;                                             parens of the 3 shapes (), [],
;;                                             and {} is supported and imposed.
;;                                             Potential improvement: add
;;                                             customization for it.
;;                                             The `seed7-indent-width'
;;                                             user-option, which defaults to
;;                                             2, controls the indentation
;;                                             width.
;;
;; Static checking/compilation                 Done.  `seed7-check-or-compile'
;;                                             runs `seed7-checker' or
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
;;
;; seed7-mode key map.                         Done.
;; Top Menu.                                   Done.
;; imenu support                               Done.
;; Speedbar support.                           Done.
;;
;; =========================================== ===============================

;; Please report any problem you may notice by creating a bug report in the
;; GitHub project: https://github.com/pierre-rouleau/seed7-mode


;; Future:
;; - Launch help on keywords, perhaps implement statement help
;; - Keyword Completion help.

;;
;; [ TODO 2025-04-06, by Pierre Rouleau: Fix following problems:
;;  Known problems and improvements:
;;  # 01  Problem: Complete defface definitions:
;;        - Maybe add ability to reduce number of faces used (or re-use the
;;          same face for various elements).  It would allow dual use: one
;;          with lots of different renderings and another with not that many,
;;          a more conservative approach.
;;  # 02  Problem: Escaped single and double quote in strings are now recognized.
;;        However a string continuation that ends with a backslash just before
;;        the terminating quote is not supported.
;;  # 03  Improvement: Check if following Emacs functions can help reduce code
;;        size: `indent-line-to', `indent-next-tab-stop'
;;  # 04  Improvement: The <f12><left> should move to the top of current block,
;;        <f12><right> to the end of current block. <f12><<left> several times
;;        would move toward the top of the current function or procedure.
;;        And <f12><right> several time would move toward the end of the
;;        current function/procedure.
;; ]
;;
;;
;;* Table of Content
;;  ================
;;
;; Code Organization Layout (use these as markers to locate related code; both
;; in the titles and inside other code locations).
;; With `lispy-mode' active, you can also use the `outline-minor-mode'
;; commands to navigate across section titles as well as hide/show the content
;; of sections.
;;
;; - Version Info
;;   . `seed7-mode-version'
;;   * `seed7-mode-customize'
;; - Seed7 Customization
;; - Seed7 Keyword Regexp
;;   - Seed7 Tokens
;;     - Seed7 Comments Control
;;     - Seed7 Basic Element Regexp
;;     - Seed7 Float Literals
;;     - Seed7 Numbers with Exponents
;;     - Seed7 BigInteger Literals
;;     - Seed7 Integer Literals
;;   - Seed7 Pragmas
;;   - Seed7 include
;;   - Seed7 keywords used in statements
;;   - Seed7 is-statement keywords
;;   - Seed7 keywords used in middle of statements
;;   - Seed7 statement enclosing keywords
;;   - Seed7 declaration introduction keywords
;;   - Seed7 Predefined Types
;;   - Seed7 Predefined Constants
;;   - Seed7 Predefined Variables
;;   - Seed7 Predefined errinfo value
;;   - Seed7 Predefined Functions
;;   - Seed7 Operator Symbols
;;   - Seed7 Predefined Assignment Operators
;;   - Seed7 Predefined Comparison Operators
;;   - Seed7 Other Predefined Operators
;;   - Seed7 Arithmetic Operators
;;   - Seed7 Array/Set Regexp
;;   - Seed7 Block Processing Regexp
;;   - Seed7 Procedure/Function Parameters Regexp
;;   - Seed7 Procedure/Function Regexp
;;     . `seed7-func-beg-of-decl-re-fmt'
;;     . `seed7--procfunc-beg-of-decl-re-fmt'
;; - Seed7 iMenu Support Regexp
;;   - Seed7 Procedure/Function iMenu Regexp
;;   - Seed7 Enum/Structure iMenu Regexp
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
;; - Seed7 Speedbar Support
;; - Seed7 Low-level Macros
;;   . `seed7--set'
;; - Seed7 Utilities
;;   . `seed7--plural-s'
;;   . `seed7--run'
;; - Seed7 Code Navigation
;;   - Seed7 Comment and String Identification Macros and Functions
;;     . `seed7--point-in-code-p'
;;       . `seed7--inside-string-p'
;;       . `seed7--inside-comment-p'
;;     . `seed7-inside-comment-p'
;;     . `seed7-inside-string-p'
;;       . `seed7--inside-string-p'
;;   - Seed7 Code Search Functions
;;     . `seed7-re-search-forward'
;;     . `seed7-re-search-forward-closest'
;;     . `seed7-re-search-backward'
;;     . `seed7-re-search-backward-closest'
;;   - Seed7 Skipping Comments
;;     . `seed7-skip-comment-backward'
;;     . `seed7-skip-comment-forward'
;;       . `seed7---skip-block-comment-forward'
;;       . `seed7---skip-line-end-comment'
;;   - Seed7 forward-sexp/backward-sexp Support
;;     . `seed7--forward-sexp-function'
;;       . `seed7--forward-block-comment'
;;       . `seed7--at-line-comment-start-p'
;;       . `seed7--forward-line-comments'
;;         . `seed7--line-comment-hash'
;;       . `seed7--at-array-definition-end-line-p'
;;       . `seed7--at-set-definition-end-line-p'
;;   - Seed7 Navigation by Block/Procedure/Function
;;     - Navigation to Outer Block
;;       . `seed7-top-block-name'
;;         . `seed7--to-top'
;;         . `seed7--block-name'
;;       . `seed7-to-top-of-block'
;;         . `seed7--to-top'
;;     - Seed7 Procedure/Function Search Utility functions
;;       . `seed7--move-and-mark'
;;       . `seed7--pos-msg'
;;       . `seed7--show-info'
;;       . `seed7--no-defun-found-msg-for'
;;     - Seed7 Procedure/Function Navigation Commands
;;       * `seed7-beg-of-defun'
;;       * `seed7-beg-of-next-defun'
;;       * `seed7-end-of-defun'
;;         o `seed7--move-and-mark'
;;         o `seed7--pos-msg'
;;      - Seed7 Procedure/Function Navigation Mode Functions
;;        > `seed7-nav-beginning-of-defun'
;;          o `seed7-beg-of-defun'
;;        > `seed7-nav-end-of-defun'
;;          o `seed7-end-of-defun'
;;   - Seed7 Navigation by Block
;;     * `seed7-to-block-forward'
;;       . `seed7--end-regexp-for'
;;         . `seed7--type-regexp'
;;     * `seed7-to-block-backward'
;;       . `seed7--current-line-nth-word'
;;       . `seed7--start-regexp-for'
;;         . `seed7--type-regexp'
;; - Seed7 iMenu Support
;;   . `seed7--setup-imenu'
;;   . `seed7--refresh-imenu'
;;   * `seed7-toggle-menu-callable-list'
;;   * `seed7-toggle-menu-sorting'
;; - Seed7 Code Marking
;;   * `seed7-mark-defun'
;;   . `seed7-lines-in-defun'
;; - Seed7 Indentation
;;   - Seed7 Indentation Customization
;;   - Seed7 Indentation Utility Macros
;;     . `seed7--inside-block-p'
;;   - Seed7 Indentation Utility Functions
;;     - Seed7 Indentation Base utilities
;;       . `seed7-blank-line-p'
;;       . `seed7-indent-step-for-column'
;;       . `seed7-current-line-number'
;;       . `seed7-current-line-start-inside-comment-p'
;;         o `seed7-inside-comment-p'
;;         . `seed7-to-indent'
;;       . `seed7-current-line-indent'
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
;;     . `seed7-line-ends-with'
;;   - Seed7 Indentation Line Type Checking Functions
;;     . `seed7-line-isa-string'
;;     . `seed7-line-is-block-end'
;;     . `seed7-line-inside-a-block-cached'
;;       . `seed7--cached-block-spec-current-line'
;;       . `seed7--cached-block-bounds'
;;       . `seed7--cache-block-spec'
;;       . `seed7--indent-last-block-spec'
;;       . `seed7--indent-block-bounds'
;;       . `seed7-line-inside-a-block'
;;         . `seed7--block-end-pos-for'
;;         . `seed7--indent-offset-for'
;;         . `seed7--on-lineof'
;;     . `seed7-line-inside-until-logic-expression'
;;     . `seed7-line-inside-func-return-statement'
;;     . `seed7-line-inside-argument-list-section'
;;     . `seed7-line-inside-array-definition-block'
;;     . `seed7-line-at-endof-array-definition-block'
;;     . `seed7-line-inside-set-definition-block'
;;     . `seed7-line-inside-logic-check-expression'
;;     . `seed7-line-inside-assign-statement-continuation'
;;     . `seed7-line-at-endof-set-definition-block'
;;     . `seed7-line-inside-parens-pair'
;;       . `seed7-line-inside-syntax-parens-pair'
;;         . `seed7--paren-pair-string'
;;     . `seed7-line-inside-parens-pair-column'
;;     . `seed7-line-inside-nested-parens-pairs'
;;     . `seed7-line-inside-nested-parens-pairs-column'
;;     . `seed7-indentation-of-previous-non-string-line'
;;     . `seed7-line-is-procfunc-beg-of-decl'
;;     . `seed7-line-is-defun-end'
;;       o `seed7-line-starts-with-any'
;;         o `seed7-line-starts-with'
;;   - Seed7 Gradual Block Selection Support for expand-region
;;     . `seed7--activate-expand-region'
;;       . `seed7--setup-expand-region'
;;     . `seed7-er-mark-enclosing-block'
;;   - Seed7 Indentation Comment Checking Function
;;     . `seed7-comment-column'
;;   - Seed7 Position Value Identification
;;     . `seed7-position-of-end-of-statement'
;;     . `seed7-bol-position'
;;   - Seed7 Indentation Calculator Function
;;     o `seed7-complete-statement-or-indent'
;;       * `seed7-indent-line'
;;         . `seed7-calc-indent'
;;           . `seed7--cached-block-bounds'
;;           . `seed7--indent-one-line'
;;       * `seed7-fill'
;;         * `seed7-indent-block'
;;           o `seed7-indent-line'
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
;;   * `seed7-check-or-compile'
;;     . `seed7-check-file'
;;       . `seed7--cmd-specs-for'
;;       . `seed7--expand-args'
;;       . `seed7--run-and-parse'
;;         . `seed7--parse-diagnostics'
;;       . `seed7--end-msg-for'
;; - Seed7 Run Program
;;   - Seed7 Run – process filters and sentinel
;;   * `seed7-run-program'
;;     o `seed7--cmd-specs-for'
;;     . `seed7--run-program-filter'
;;     . `seed7--run-sentinel'
;;     o `seed7--end-msg-for'
;;   - Seed7 Run – interactive input commands
;;     . `seed7-run-send-input'
;;     . `seed7-run-interrupt'
;;     . `seed7-run-raw-send-key'
;;   - Seed7 Run – run-buffer major mode
;;     * `seed7-run-mode'
;;     * `seed7-run-enter-raw-mode'
;;     * `seed7-run-exit-raw-mode'
;; - Seed7 Cross Reference
;;    > `seed7--xref-backend'
;;    + `xref-backend-identifier-at-point'
;;      . `seed7-symbol-at-point'
;;        . `seed7-operator-at-point'
;;    + `xref-backend-definitions'
;;      . `seed7--find-symbol'
;;        . `seed7--xref-get'
;;          . `seed7-point-on-defined-identifier-p'
;;          . `seed7--find-candidates-for'
;;            . `seed7--symbol-definition-areas-for-block'
;;            . `seed7--find-identifier-in'
;;          . `seed7--xref-get-from-s7xref'
;;            . `seed7--build-xref'
;;            . `seed7--xref-in-list'
;;            . `seed7--signature-from'
;;              . `seed7--signature-at'
;;    + `xref-backend-completions'
;;      . `seed7--xref-identifiers'
;;        . `seed7--procfun-identifiers'
;;        . `seed7--list-of-terms'
;;        . `seed7--make-xref-from-file-loc'
;;    / `seed7--invalidate-xref-cache'   -- hook
;; - Seed7 Completion Support
;; - Seed7 Abbreviation Support
;;   * `seed7-rebuild-abbrev-table'
;; - Seed7 Key Map
;; - Seed7 Menu
;; - Seed7 Major Mode
;;   * `seed7-mode'

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
;;
(require 'simple)     ; use: `move-beginning-of-line'
(require 'speedbar)   ; use: `speedbar-add-supported-extension'
(require 'subr-x)     ; use: `string-trim', `string-blank-p'
(require 'easymenu)   ; use: `easy-menu-define'
(require 'tempo)      ; use: `tempo-forward-mark', `tempo-backward-mark'
(require 'imenu)      ; use: `imenu--menubar-select', `imenu--rescan-item'
;;                    ;      `imenu-update-menubar',
;;                    ;      `imenu-generic-expression'
(require 'outline)    ; use: `outline-minor-mode', `outline-regexp',
;;                    ;      `outline-heading-end-regexp',
(require 'xref)       ; use: `xref-make', 'xref-make-file-location'
(require 'align)      ; use: `align-mode-rules-list', `align-region-separate'
(require 'abbrev)     ; use: `clear-abbrev-table', `abbrev-table-p', ...
(require 'cl-lib)     ; use: `cl-flet'
(require 'compile)    ; use: `compilation-num-warnings-found',
;;                    ;      `compilation-num-infos-found'
(require 'rx)         ; use: `rx-to-string'
(require 'seq)        ; use: `seq-filter'
(require 'which-func) ; use: `which-func-functions'
(require 'add-log)    ; use: `add-log-current-defun-function'
(require 'tabulated-list) ; use: `tabulated-list-mode',
;;                        ; `tabulated-list-mode-map'

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

(defconst seed7-mode-version-timestamp "2026-06-25T03:05:30+0000 W26-4"
  "Version UTC timestamp of the `seed7-mode' file.
Automatically updated when saved during development.
Please do not modify.")

(defun seed7-mode-version ()
  "Print `seed7-mode' version UTC timestamp."
  (interactive)
  (message "seed7-mode version UTC timestamp: %s" seed7-mode-version-timestamp))

;; ---------------------------------------------------------------------------
;;* Seed7 Customization
;;  ===================

(defun seed7-mode-customize ()
  "Open the `seed7-mode' customization buffer."
  (interactive)
  (customize-group 'seed7))

(defgroup seed7 nil
  "Seed7 Programming Language support configuration."
  :group 'languages
  :link '(url-link :tag "seed7-mode @ GitHub"
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
  "When on, list functions and procedures together, otherwise separately.

This affects the way the callables are displayed in imenu commands,
in the top menu and inside the Speedbar."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

(defcustom seed7-menu-list-functions-sorted t
  "When on, list menu entries in sorted order, otherwise in code order.

This affects the way the callables are displayed in imenu commands,
in the top menu and inside the Speedbar."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Code Template Expansion

(defcustom seed7-template-expansion-disables-overwrite-mode t
  "When non-nil `overwrite-mode' is forced off when code template is expanded.

When `seed7-complete-statement-or-indent' performs code expansion and
`seed7-template-expansion-disables-overwrite-mode' is on, it forces
`overwrite-mode' off in the current buffer to prevent writing over the
expanded code.

To disable this behavior turn this user-option off."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Code Navigation

(defcustom seed7-verbose-navigation t
  "When non-nil, Seed7 navigation commands print a success message.
If you do not want these navigation success messages printed set this to
nil.  Setting it to nil does not prevent user error messages to show up
when the navigation commands fail."
  :group 'seed7
  :type 'boolean
  :safe #'booleanp)

;;** Seed7 Compilation
(defcustom seed7-checker "s7check"
  "Seed7 source code check command line.

The command line must identify the Seed7 static check tool, s7check,
by default.
You may:
- Specify the program name without a path if it can be found by Emacs.
- Specify the program name with an absolute path.
- Specify static checker options after the program name if necessary.

IMPORTANT NOTE: seed7-mode expects this tool to print diagnostics on stdout.

The name of the source code file is appended to the end of that line.
Note that the s7check is part of the example programs located inside
the Seed7 prg directory.  Compile the prg/s7check.sd7 with s7c to create
the executable you can use for this."
  :group 'seed7
  :type 'string)

(defcustom seed7-interpreter "s7"
  "Seed7 interpreter command line.

The command line must identify the Seed7 interpreter, s7, by default.
You may:
- Specify the program name without a path if it can be found by Emacs.
- Specify the program name with an absolute path.
- Specify interpreter options after the program name if necessary.

The name of the Seed7 source file and any user-supplied arguments are
appended at the end of the command line at run time.

The ~ character, if you use it, is expanded to identify your HOME
directory."
  :group 'seed7
  :type 'string)

(defcustom seed7-compiler "s7c"
  "Seed7 compiler command line.

The command line must identify the Seed7 compiler, s7c, by default.
You may:
- Specify the program name without a path if it can be found by Emacs.
- Specify the program name with an absolute path.
- Specify compiler options after the program name if necessary.

IMPORTANT NOTE: seed7-mode expects this tool to print diagnostics on stderr.

The name of the source code file is appended to the end of that line."
  :group 'seed7
  :type 'string)

;;** Seed7 Cross Reference

(defcustom seed7-xref
  (combine-and-quote-strings
   (list "s7"
         (expand-file-name
          "tools/s7xref.sd7"
          (file-name-directory
           (or (locate-library "seed7-mode")
               (let ((buf (get-buffer "seed7-mode.el")))
                 (when buf
                   (buffer-file-name buf)))
               (expand-file-name "utils/" user-emacs-directory))))))
  "Seed7 cross reference builder command line.

The command line must identify the Seed7 cross reference builder,
s7xref, by default.

You may type:
- the s7 interpreter, followed by the Seed7 source file to use, or
- the cross reference builder executable program to use.

The seed7 mode repository includes the s7xref.sd7 file inside the tools
sub-directory.  You can either create an executable for it or use the s7
interpreter to run it without having to compile it.

Using the interpreted version is the preferred method: an interpreted
version of the program will continue to work if you later update the
Seed7 system.  If you use the compiled version, you will need to
re-compile the s7xref.sd7 file each time you update Seed7.

The name of the cross reference executable or the s7 Seed7 interpreter
program must include their absolute path unless these programs can be
found through the PATH environment variable accessible to Emacs.

The ~ character, if you use it, is expanded to identify your HOME
directory.

The default value attempts to locate `seed7-mode' from Emacs
`load-path'.  If this fails it uses the utils/tools directory inside
your `user-emacs-directory'.

Modify this value if `seed7-mode' is not in your Emacs load path
and the default path is not appropriate."
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
;; the mode to dynamically adapt to the Seed7 extended syntax.
;;
;; See: https://seed7.net/faq.htm#add_syntax_highlighting
;;
;; Notes:
;;
;; - For several keywords, the "\\<" and "\\>" are important to prevent
;;   detection of words inside other words, specially for regexp where
;;   those keywords are not surrounded by whitespace.
;;
;; - Naming conventions: symbol names ending in `nc-re' or `nc-regexp' are
;;   non-capturing ;; version of another, similar, regular expression, that
;;   has the same name without the '-nc' in its name.
;;


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
  "Match any horizontal whitespace character.")

(defconst seed7--whitespace-re
  "[[:blank:]
]"
  "Match any horizontal whitespace character and new line.")

(defconst seed7--anychar-re
  "[^\\0]"
  "Match any character including new-line.")

(defconst seed7--any-non-semicolon-re
  "[^;]+"
  "Any sequence of non-semicolon characters.
Used in `return'-statement patterns to match the body up to the terminating `;'.
Uses a negated character class instead of nested lazy quantifiers to prevent
catastrophic backtracking (ReDoS) on large files.")

;; --
;; Note: Ensure that something like 0_ is not matched by seed7-name-identifier-nc-re
(defconst seed7-name-identifier-fmt "\\(%s[[:alpha:]][[:alnum:]_]*\\|_[[:alnum:]_]+\\)"
  ;; Note: the regexp has 2 branches but the first character on each differs:
  ;; this means that the regexp engine does not need to backtrack.
  ;; Backtracking can be expensive, so it is avoided.
  "A complete, valid name identifier.  Format string.")

(defconst seed7-name-identifier-nc-re (format seed7-name-identifier-fmt "?:")
  "A complete, valid name identifier.  No capturing group.")

(defconst seed7-name-identifier-re (format seed7-name-identifier-fmt "")
  "A complete, valid name identifier. Group 1: identifier : 1 word")

;; --
(defconst seed7--open-paren-regexp
  (regexp-opt '("(" "[" "{" ))
  "Regexp matching Seed7 opening paren-like delimiters.")

(defconst seed7--close-paren-chars '(?\) ?\] ?\})
  "Seed7 closing paren-like delimiter characters.")

;; --

(defconst seed7-type-identifier-nc-re
  (format "\\(?:%s\\(?:[[:blank:]]+?%s\\)??\\)"
          seed7-name-identifier-nc-re
          seed7-name-identifier-nc-re)
  "A complete, valid type identifier name with one or 2 identifiers.
Has no capturing group.")

(defconst seed7-type-identifier-re
  (format "\\(%s\\(?:[[:blank:]]+?%s\\)??\\)"
          seed7-name-identifier-nc-re
          seed7-name-identifier-nc-re)
  "A complete, valid type identifier name with one or 2 identifiers.
Group 1: type identifier (1 or 2 words).")

;; --

;; (defconst seed7--syntax-spec-identifier-re
;;   "\\(\\.\\(?:[[:alpha:]][[:alnum:]]*\\)\\|\\(?:[[:alpha:]][[:alnum:]]*\\)\\.\\)"
;;   "Seed7 syntax spec keyword (surrounded by period). In group 1")

(defconst seed7--special-char-re
  "[-!$%&*+,\\./:;<=>?@\\^`|~]"
  "Any one of the special characters.")

(defconst seed7--very-special-char-re
  "[];:)(}{.,[]"
  "Regexp for special characters to extract.

Some of those characters, but not all, are not implemented as callable,
but used in Seed7 syntax via different mechanisms.  Some may be implemented
by Seed7 code, but if s7xref does not create a reference for them it's because
they are understood by the Seed7 compiler/interpreter.")

(defconst seed7--compile-time-symbols '(".."
                                        "len")
  "List of symbols that Seed7 uses but are not defined by Seed7 code.
These are known by the Seed7 compiler and interpreter and run at compile time.")

(defconst seed7--special-identifier-nc-re
  (format "\\(?:%s+\\)" seed7--special-char-re)
  "A complete, valid Seed7 special identifier.  Non capturing.")

(defconst seed7-special-identifier-re
  (format "\\(%s\\)" seed7--special-identifier-nc-re)
  "A complete, valid Seed7 special identifier.  One capturing group.")

(defconst seed7--any-op-identifier-re
  (format "\\(?:\\(%s\\|%s\\)\\)"
          seed7-name-identifier-nc-re
          seed7--special-identifier-nc-re)
  "A name or special identifier.  1 capturing group.")

(defconst seed7--any-identifier-re
  (format "\\(\\(%s\\|%s\\)\\)"
          seed7-name-identifier-nc-re
          seed7--special-identifier-nc-re)
  "A name or special identifier.  1 capturing group.")

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
(defvaralias 'seed7-big-number-re 'seed7-big-integer-re)
(make-obsolete-variable 'seed7-big-number-re 'seed7-big-integer-re
                        "2026-06-16")

(defconst seed7-big-integer-re
  "\\(\\(?:\\(?:\\([2-9]\\|1[0-9]\\|2[0-9]\\|3[0-6]\\)#\\)?[0-9]+_\\)\\)"
  ;; 1            2
  ;; Group 1: Complete Big Number with or without base. "1_" or "1234322_" or "2#0001_", etc...
  ;; Group 2: base: "2" to "36".  nil if no base.
  "Big number with/without base.  Group 1: number, group 2: base or nil.
The old name, seed7-big-number-re remains available until then end of 2026.
Please update your code to use the new name before this deadline.")

;; Backward compatibility for renamed big-integer symbols.
;; Keep old names working for external user configs/extensions.

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
                                          "[^#0-9a-zA-Z]"))

(defconst seed7-any-valid-char-integer-semicolon-re
  (format "'\\\\%s;'\\)\\|\\(?:'\\\\[[:digit:]]+;'"
          (format seed7--base-x-integer-re-format
                  "(?:"
                  "")))

(defconst seed7-base-x-big-integer-re (format seed7--base-x-integer-re-format
                                              "(\\(?:"
                                              "_\\)[^#0-9a-zA-Z]")
  "Seed7 big integer regular expression.")

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
    "system"))

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
;; All keywords are listed here, but some are commented out because
;; they are part of another list below.  The ones left are the ones that
;; are at the beginning of a line (with or without leading white space),
;; identified in `seed7--lead-in-statement-keywords' and some that can also
;; be in the middle or end of line, which are identified by
;; `seed7--in-statement-keywords'.

(defconst seed7--lead-in-statement-keywords
  '(
    "raise"     ; https://seed7.net/manual/errors.htm#Exceptions
    "return"    ; https://seed7.net/faq.htm#no_return_statement
    ))

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

(defconst seed7-in-statement-keywords-regexp
  (format ". %s\\(%s\\)%s"        ; these are all the first keyword on a line
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--in-statement-keywords)))
          "\\>"))


;;** Seed7 is-statement keywords
;;   ---------------------------
;;
;; These keywords are exclusively used following the 'is' keyword.
;;
(defconst seed7-is-statement-keywords
  '(
    "forward"
    "DYNAMIC"
    "new"
    "sub"
    "action"  ; https://seed7.net/manual/actions.htm
    ))

(defconst seed7--is-statement-keywords-regexp
  (format " is\\(?:%s+?[^;]\\)?%s+\\(%s\\)\\>"
          seed7--whitespace-re
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

(defconst seed7-in-middle-statement-keywords-regexp
  (format "%s\\(%s\\)%s"
          "[[:space:]]"
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
  (format "^[[:blank:]]*%s\\(%s\\)%s"
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
  "Argument declaration intro keyword: group 1.")

(defconst seed7-declaration-intro-keywords-nc-regexp
  (format "%s\\(?:%s\\)%s"
          "\\<"
          (rx-to-string
           `(:  (or ,@seed7--declaration-intro-keywords)))
          "\\>")
  "Argument declaration intro keyword: no capturing group.")

(defconst seed7-parameter-var-declaration-regexp
  ;;       in         T              T
  (format "%s%s+?\\(?:%s%s+?\\)?\\(?:%s\\)%s*?:%s+?\\(%s\\)"
          ;;%%        % %             %    %    %      %
          ;;12        3 4             5    6    7      8

          seed7-declaration-intro-keywords-nc-regexp ; 1
          seed7--whitespace-re                       ; 2
          seed7-type-identifier-nc-re                ; 3
          seed7--whitespace-re                       ; 4
          seed7-type-identifier-nc-re                ; 5
          seed7--whitespace-re                       ; 6
          seed7--whitespace-re                       ; 7
          seed7-name-identifier-nc-re)               ; 8
  "Regexp extracting identifier from parameter or local variable declaration.
1 group: identifier name")

;;** Seed7 Predefined Types
;;   ----------------------
;;
;; Ref: Abstract data type: https://seed7.net/manual/decls.htm#Abstract_data_types

(defconst seed7--predefined-types
  '("array"            ; https://seed7.net/manual/types.htm#array        abstract data type.
    "subtype"          ; abstract data type
    "subrange"         ; abstract data type
    "interface"        ; abstract data type
    "bigInteger"       ; https://seed7.net/manual/types.htm#bigInteger
    "bigRational"      ; https://seed7.net/manual/types.htm#bigRational
    "bin32"            ; https://seed7.net/manual/types.htm#bin32
    "bin64"            ; https://seed7.net/manual/types.htm#bin64
    "bitset"           ; https://seed7.net/libraries/bitset.htm#bitset
    "boolean"          ; https://seed7.net/manual/types.htm#boolean
    "bstring"          ; https://seed7.net/manual/types.htm#bstring
    "category"         ; https://seed7.net/manual/types.htm#category
    "char"             ; https://seed7.net/manual/types.htm#char
    "clib_file"        ; https://seed7.net/libraries/clib_file.htm
    "color"            ; https://seed7.net/manual/types.htm#color
    "complex"          ; https://seed7.net/manual/types.htm#complex
    "creator"          ; https://seed7.net/manual/types.htm#creator
    "database"         ; https://seed7.net/manual/types.htm#database
    "destroyer"        ; https://seed7.net/manual/types.htm#destroyer
    "duration"         ; https://seed7.net/manual/types.htm#duration
    "enum"             ; https://seed7.net/manual/types.htm#enumeration  abstract data type.
    "expr"             ; https://seed7.net/manual/types.htm#expr
    "file"             ; https://seed7.net/manual/types.htm#file
    "fileSys"          ; https://seed7.net/manual/types.htm#fileSys
    "float"            ; https://seed7.net/manual/types.htm#float
    "func"             ; https://seed7.net/manual/types.htm#func
    "hash"             ; https://seed7.net/manual/types.htm#hash         abstract data type.
    "integer"          ; https://seed7.net/manual/types.htm#integer
    "listener"         ; https://seed7.net/manual/types.htm#listener
    "object"           ; https://seed7.net/manual/types.htm#object
    "pollData"         ; https://seed7.net/manual/types.htm#pollData
    "proc"             ; https://seed7.net/manual/types.htm#proc
    "process"          ; https://seed7.net/manual/types.htm#process
    "program"          ; https://seed7.net/manual/types.htm#program
    "rational"         ; https://seed7.net/manual/types.htm#rational
    "reference"        ; https://seed7.net/manual/types.htm#reference
    "ref_list"         ; https://seed7.net/manual/types.htm#ref_list
    "set"              ; https://seed7.net/manual/types.htm#set          abstract data type
    "sqlStatement"     ; https://seed7.net/manual/types.htm#sqlStatement
    "string"           ; https://seed7.net/manual/types.htm#string
    "struct"           ; https://seed7.net/manual/types.htm#struct
    "structElement"    ; https://seed7.net/manual/types.htm#structElement
    "text"             ; https://seed7.net/manual/types.htm#text
    "time"             ; https://seed7.net/manual/types.htm#time
    "type"             ; https://seed7.net/manual/types.htm#type
    ;; varfunc (listed in seed7--statement-enclosing-keywords)
    "void"             ; https://seed7.net/manual/types.htm#void
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
(defconst seed7-predefined-constants-regexp
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
(defconst seed7-predefined-variables-regexp
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
(defconst seed7-errinfo-values-regexp
  (format "%s\\(%s\\)%s"
          "\\<"
          (rx-to-string
           `(: (or ,@seed7--errinfo-values)))
          "\\>"))

;;** Seed7 Operator Symbols
;;   ----------------------

;; [ TODO 2025-04-10, by Pierre Rouleau: categorize 'noop' according to Seed7 spec once I find it ]
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

(defconst seed7-predef-assignment-operator-regexp
  (rx-to-string
   `(:  (or ,@seed7--assignment-operator-symbols)))
  "Seed7 assignment operator symbols.  Captured in Group 0.")


;;** Seed7 Predefined Comparison Operators
;;   -------------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Predefined comparison operators are: = <> < <= > >=

(defconst seed7-predef-comparison-operator-regexp
  "\\(?:[=><]\\|\\(?:<>\\|<=\\|>=\\)\\)"
  "Symbol is in group 0.")


;;** Seed7 Other Predefined Operators
;;   --------------------------------
;;
;; Ref: https://seed7.sourceforge.net/faq.htm#syntax_highlighting
;;
;; Other predefined operators are: + - * / ** ! << >> & | >< <& ?
;;                                            -------     -------
(defconst seed7-other-predef-operator-regexp
  "[!?]\\|<<\\|>>\\|><\\|<&"
  "Symbol is in group 0.")


;;** Seed7 Arithmetic Operators
;;   --------------------------
;;
;;   + - * / **
;;
(defconst seed7-arithmetic-operator-regexp
  "[[:alnum:]_ )]\\([/*]\\)[[:alnum:]_ (]"
  "Arithmetic operator except the minus sign.
Match group 1")

(defconst seed7-minus-operator-regexp
  "[^+-]\\([+-]\\)[^+-]"
  "Arithmetic minus operator in group 1.")


;;** Seed7 Block Processing Regexp
;;   -----------------------------

(defconst seed7-block-start-regexp
  (concat
   "\\("
   (regexp-opt '("case" "catch" "const func" "const proc:"
                 "const type:" "elsif" "for" "if" "while"))
   "[[:blank:]]\\|"
   (regexp-opt '("begin" "block" "else" "exception"
                 "global" "local" "repeat" "result")
               'words)
   "\\)")
  "Regexp for the beginning of a Seed7 block.  One capture group.")

(defconst seed7-block-line-start-regexp (concat
                                         "^[[:blank:]]*?"
                                         seed7-block-start-regexp)
  "Regexp to find location of blocks.")

(defconst seed7-block-end-regexp
  (concat
   "\\(?:end +"
   "\\(?:"
   (regexp-opt '("case" "enum" "for" "func" "global" "if" "struct" "while"))
   ";\\|block\\)\\)"
   "\\|\\(?:until[[:blank:]]\\)")    ; tab allowed after until and rest of logic
  "Regexp for generic end of block.")

(defconst seed7-block-top-start-regexp
  (concat
   "\\("
   (regexp-opt '("case" "catch" "const array" "const func" "const proc:"
                 "const set" "const type:" "elsif" "for" "if"
                 "var array" "var set" "while"))
   "[[:blank:]]\\|"
   (regexp-opt '("begin" "block" "else" "exception"
                 "global" "local" "repeat" "result")
               'words)
   "\\)")
  "Regexp for the top of a Seed7 block.  One capture group.")

(defconst seed7--callable-declaration-start-regexp
  (concat
   "\\("
   (regexp-opt '("const array" "const func" "const proc:"
                 "const set"   "const type:" "var array" "var set"))
   "[[:blank:]]\\)")
  "Regexp matching the start of a Seed7 callable or type declaration.
This is a strict subset of `seed7-block-top-start-regexp' that excludes
all control-flow keywords (`if', `while', `for', `elsif', `case', `catch')
and structural markers (`begin', `local', `result', `else', `exception',
`global', `repeat', `block').

Used by `seed7--to-top' to find the enclosing top-level or template-level
declaration without visiting every control-flow keyword on the way.

Performance improvement (measured keyword match counts):
  chkarr.sd7 : 2,943 total keywords → ~150 declarations  (~20× fewer iters)
  castle.sd7  :   706 total keywords →  ~50 declarations  (~14× fewer iters)

The column check in `seed7--to-top' (< start-col) is still correct for
indented `const func'/`const proc' in .s7i template bodies such as
`ENABLE_SORT' in aarray.s7i, since those are either `is action \"...\"'
one-liners or short `return...;' functions — neither creates a
`begin...end func' block you can be indenting code *inside*.")

;;** Seed7 Array Regexp
;;   ------------------

(defconst seed7--array-definition-start-regexp
  ;; "\\(?:const\\|var\\) +?array[[:blank:]]+?.+?:.+?("
  ;; "\\(?:const\\|var\\) +array[[:blank:]]+.+:.+("
  "\\(?:const\\|var\\) +array[[:blank:]]+[^:\n]+:[^(;\n]*("
  "Regexp matching the start of a Seed7 array definition block header.
See `seed7--set-definition-start-regexp' for the whitespace convention.")

;; Note: another possibility for seed7--array-definition-start-regexp would
;; be: ?:const\\|var\\)[[:blank:]]+array[[:blank:]]+[^:\n]+:[^(;\n]*("

(defconst seed7--line-array-definition-start-regexp
  (concat "^[[:blank:]]*?" seed7--array-definition-start-regexp)
  "Regexp matching line starting with a  Seed7 array definition block header.")

(defconst seed7--array-definition-name-regexp
  (concat "\\(?:const\\|var\\)[[:blank:]]+"
          "array[[:blank:]]+[^:\n]+:"
          "[[:blank:]]*\\([[:alpha:]][[:alnum:]_]*\\)")
  "Regexp matching a Seed7 array declaration; group 1 is the entity name.
The name is the first identifier that follows the `:' in the declaration.
Unlike `seed7--array-definition-start-regexp', this regexp captures the name.

Example matches:
  const array integer: myArray is (1, 2, 3);   → group 1: myArray
  var   array string:  names;                  → group 1: names")

;;** Seed7 Set Regexp
;;   ----------------

(defconst seed7--set-definition-start-regexp
  ;; "\\(?:const\\|var\\) +?set[[:blank:]]+?.+?:.+?{"
  "\\(?:const\\|var\\) +set[[:blank:]]+[^:\n]+:[^{;\n]*{"
  "Regexp matching the start of a Seed7 set definition block header.
Matches `const set' or `var set' followed by a type annotation and the
opening `{'.

Design note: a literal space (` +?') is required between `const'/`var' and
`set' — hard tabs between adjacent keywords are intentionally not supported
per Seed7 style (dual-keyword restriction).  A hard tab or space
\(`[[:blank:]]') is accepted after `set', because `set' is the last keyword
in the phrase before the user-supplied identifier.")

(defconst seed7--set-definition-name-regexp
  (concat "\\(?:const\\|var\\)[[:blank:]]+"
          "set[[:blank:]]+[^{:\n]+:"
          "[[:blank:]]*\\([[:alpha:]][[:alnum:]_]*\\)")
  "Regexp matching a Seed7 set declaration; group 1 is the entity name.
The name is the first identifier that follows the `:' in the declaration.
Unlike `seed7--set-definition-start-regexp', this regexp captures the name.

Example matches:
  const set of integer: mySet is {1, 2, 3};    → group 1: mySet
  var   set of string:  names;                 → group 1: names")

(defconst seed7--line-set-definition-start-regexp
  (concat "^[[:blank:]]*?" seed7--set-definition-start-regexp)
  "Regexp matching line starting with a Seed7 set definition block header.")


;;** Seed7 Procedure/Function Parameters Regexp
;;   ------------------------------------------

(defconst seed7-one-arg-re
  ;;         (-------------------------------------------------------------------)
  ;;                      in       type1      type   :   id             attr attribute
  ;;              (    (          (----------)          )         )  |  (--------------)
  (format "\\(?:\\(?:\\(?:%s%s+?\\(?:%s%s+?\\)?%s%s*?:\\)?%s+?%s\\)\\|\\(?:attr%s+?%s\\)\\)"
          ;;              % %        % %       % %        %   %                %   %
          ;;              1 2        3 4       5 6        7   8                9   10
          seed7-declaration-intro-keywords-nc-regexp ; 1 in/out/inout...
          seed7--whitespace-re                       ; 2
          seed7-name-identifier-nc-re                ; 3 type (opt)
          seed7--whitespace-re                       ; 4
          seed7-name-identifier-nc-re                ; 5 type
          seed7--whitespace-re                       ; 6
          seed7--whitespace-re                       ; 7
          seed7-name-identifier-nc-re                ; 8 identifier
          seed7--whitespace-re                       ; 9
          seed7-name-identifier-nc-re                ; 10 attribute
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
  "Regexp for zero or more arguments inside parenthesis pair.")

;; --

(defconst seed7-name-paramparens-re               ; 1
  (format "\\(%s\\)%s+?%s"
          seed7-name-identifier-nc-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for fctname ( args... ).  Group1 : fctname.")

(defconst seed7-paramparens-name-paramparens-re     ; 2
  (format "%s%s+?\\(%s\\|%s\\)%s+?%s"
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7-name-identifier-nc-re
          seed7--special-identifier-nc-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for (args...) fctname (args...).  Group1: fctname.")


(defconst seed7-arrparens-name-arrparens-re     ; 3
  (format "\\[%s*?%s%s+?\\(%s\\|%s\\)%s+?%s%s*?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7-name-identifier-nc-re
          seed7--special-identifier-nc-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for [ (args...) fctname (args...) ].  Group1: fctname.")

(defconst seed7-arg-arrparens-name-arrparens-re ; 4
  (format "%s%s+?\\[%s*?%s%s+?\\(%s\\|%s\\)%s+?%s%s*?]"
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7-name-identifier-nc-re
          seed7--special-identifier-nc-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for (args...)  [ (args...) fctname (args...) ].  Group1: fctname.")

(defconst seed7-emptyarr-paramparens-re           ; 5
  (format "\\[]%s+?%s"
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for [] (args...).")

(defconst seed7-arrparens-paramparens-re          ; 6
  (format "\\[%s+?%s%s+?]%s+?%s"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re)
  "Regexp for [(args...)] (args...).")

(defconst seed7-paramparens-arrparens-re          ; 7
  (format "%s+?%s%s+?\\[%s+?%s%s+?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re)
  "Regexp for (args...) [(args...)].")

(defconst seed7-paramparens-arrparens-op-re       ; 8
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s%s+?]"
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--whitespace-re
          seed7-args-in-parens-re
          seed7--whitespace-re
          seed7--special-identifier-nc-re
          seed7--whitespace-re)
  "Regexp for (args...) [(args...) op ].")

(defconst seed7-paramparens-op-arrparens-re       ; 9
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s%s+?]"
          ;;%  % %      %   % %   % %
          ;;1  2 3      4   5 6   7 8
          seed7--whitespace-re                ; 1
          seed7-args-in-parens-re             ; 2
          seed7--whitespace-re                ; 3
          seed7--whitespace-re                ; 4
          seed7--special-identifier-nc-re     ; 5
          seed7--whitespace-re                ; 6
          seed7-args-in-parens-re             ; 7
          seed7--whitespace-re)               ; 8
  "Regexp for (args...) [ op (args...) ].")

(defconst seed7-paramparens-arrparens-op-arrparens-re ; 10
  (format "%s+?%s%s+?\\[%s+?%s%s+?%s+?%s%s+?%s%s+?]"
          ;;%  % %      %   % %   %   % %   % %
          ;;1  2 3      4   5 6   7   8 9   10 11
          seed7--whitespace-re               ; 1
          seed7-args-in-parens-re            ; 2
          seed7--whitespace-re               ; 3
          seed7--whitespace-re               ; 4
          seed7-args-in-parens-re            ; 5
          seed7--whitespace-re               ; 6
          seed7--whitespace-re               ; 7
          seed7--any-op-identifier-re        ; 8 : op
          seed7--whitespace-re               ; 9
          seed7-args-in-parens-re            ; 10
          seed7--whitespace-re)              ; 11
  "Regexp for (args...) [ (args..) op (args...) ].")

;; --

(defconst seed7-any-param-pattern-re
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
\\)"                                                  ; match the more complex first
          seed7-paramparens-arrparens-op-arrparens-re ; 10
          seed7-paramparens-op-arrparens-re           ; 9
          seed7-paramparens-arrparens-op-re           ; 8
          seed7-paramparens-arrparens-re              ; 7
          seed7-arrparens-paramparens-re              ; 6
          seed7-emptyarr-paramparens-re               ; 5
          seed7-arg-arrparens-name-arrparens-re       ; 4
          seed7-arrparens-name-arrparens-re           ; 3
          seed7-paramparens-name-paramparens-re       ; 2
          seed7-name-paramparens-re)                  ; 1
  "Regexp for all possible argument patterns.")

;; --

;; (defconst seed7-arg-name-parens-re
;;   (format "\\(?:%s??%s+?\\)??\\(%s\\|%s\\)%s+?%s"
;;           seed7-args-in-parens-re
;;           seed7--whitespace-re
;;           seed7-name-identifier-nc-re
;;           seed7--special-identifier-nc-re
;;           seed7--whitespace-re
;;           seed7-args-in-parens-re)
;;   "Regexp for name followed by args within parens pair. Group1: function name.")


(defconst seed7--procfunc-forward-or-action-re
  (format "forward;\\|DYNAMIC;\\|action%s+?\\\"%s+?\\\";"
          ;;                           %       %
          ;;                           1       2
          seed7--whitespace-re
          seed7-name-identifier-nc-re)
  "Regexp matching forward or action declaration.  No capture group.")


;;** Seed7 Procedure/Function Regexp
;;   -------------------------------

(defun seed7-func-beg-of-decl-re-fmt (&optional capture)
  "Return regexp for the beginning part of a function.
When optional CAPTURE is non-nil, The returned regexp captures
- Group 1: \"proc\", \"varfunc \" or \"func \"
- Group 2: The func return type.
Otherwise the returned regexp captures nothing."
  (declare (side-effect-free t))
  (format
   ;;    const     (varfunc| func          )RT    :
   ;;              (-----------------------)
   ;;              G1                      G2
   "^%s*?const%s+\\(%s\\(?:var\\)?func%s+\\)%s%s??:"
   ;;%        %     %                 %     % %
   ;;1        2     3                 4     5 6
   seed7--blank-re                      ; 1
   seed7--whitespace-re                 ; 2
   (if capture "" "?:")                 ; 3
   seed7--blank-re                      ; 4
   (if capture
       seed7-type-identifier-re         ; 5 : RT : Return Type
     seed7-type-identifier-nc-re)
   seed7--whitespace-re))               ; 6

(defconst seed7-proc-beg-of-decl-re
  (format
   "^%s*?const%s+proc%s*?:%s+?"
   ;;%        %       %   %
   ;;1        2       3   4
   seed7--blank-re                      ; 1
   seed7--whitespace-re                 ; 2
   seed7--blank-re                      ; 3
   seed7--whitespace-re)                ; 4
  "Regexp for the beginning of a procedure declaration.
No capture group.")

;; --

(defun seed7--procfunc-beg-of-decl-re-fmt (&optional capture)
  "Return a regexp for the beginning of a function or procedure with params.
If CAPTURE is non-nil the returned regexp has 2 capture groups:
- Group 1: \"proc\", \"varfunc \" or \"func \"
- Group 2: The func return type.  May be empty.
If CAPTURE is nil, the regexp has no capture group."
  (declare (side-effect-free t))
  (format
   ;;    const     (varfunc| func       | proc) RT?     :
   ;;              (--------------------------)
   ;;              G1                          G2
   "^%s*?const%s+\\(%s\\(?:var\\)?func%s\\|proc\\)%s??%s??:"
   ;;%        %     %                 %           %   %
   ;;1        2     3                 4           5   6
   seed7--blank-re                      ; 1
   seed7--whitespace-re                 ; 2
   (if capture "" "?:")                 ; 3
   seed7--blank-re                      ; 4
   (if capture
       seed7-type-identifier-re         ; 5 : RT : function Return Type
     seed7-type-identifier-nc-re)
   seed7--whitespace-re))               ; 6

(defconst seed7-procfunc-beg-of-decl-re
  (seed7--procfunc-beg-of-decl-re-fmt :with-g1-g2)
  "Regexp for the beginning part of a function or procedure.
Group 1: \"proc\", \"varfunc \" or \"func \"
Group 2: The func return type.  May be empty.")

(defconst seed7-procfunc-beg-of-decl-nc-re
  (seed7--procfunc-beg-of-decl-re-fmt)
  "Regexp for the beginning part of a function or procedure.")

;; --

(defconst seed7-procfunc-start-regexp
  (format
   ;;                                    fct name                           is
   ;;                  (----)            (---------)        (----)
   ;;               (----------)         G3            (------------)
   ;;      (----------------------)
   ;; w[      w                                                       .  w]w
   "%s%s?\\(?:%s+?\\(?:(%s+?)\\)\\)?%s*\\(%s\\|%s\\)\\(?:%s(%s+?)\\)?%s*?%s?is\\>"
   ;;%%        %         %           %   G3%    %         %  %       %   %
   ;;1         3         4           5     6    7         8  9       10  11
   ;; 2
   seed7-procfunc-beg-of-decl-re        ; 1 : includes G1 and G2
   seed7--opt-square-brace-start-re     ; 2 w[
   seed7--whitespace-re                 ; 3
   seed7--anychar-re                    ; 4
   seed7--whitespace-re                 ; 5
   seed7-name-identifier-nc-re          ; 6
   seed7--special-identifier-nc-re      ; 7
   seed7--whitespace-re                 ; 8
   seed7--anychar-re                    ; 9
   seed7--anychar-re                    ; 10
   seed7--opt-square-brace-end-re)      ; 11 w]w
  "Regexp identifying beginning of procedures and functions.
Group 1: \"proc\", \"varfunc\" or \"func \"
Group 2: The func return type.  May be empty.
Group 3: The func or proc name." )

;; [ TODO 2025-07-17, by Pierre Rouleau: Enhance seed7-procfunc-regexp to support array parameters.]
(defconst seed7-procfunc-regexp
  (format
   ;;                                      fct name                           is     func | return | ...
   ;;                  (----)            (---------)        (----)                  (-------------------)
   ;;               (----------)         G3            (------------)               G4
   ;;      (----------------------)
   ;;  w[      w                                                      .   w]w w
   "%s%s?\\(?:%s+?\\(?:(%s+?)\\)\\)?%s*\\(%s\\|%s\\)\\(?:%s(%s+?)\\)?%s*?%s?is%s+\\(func\\|return\\|%s\\)"
   ;;%%        %         %           %   G3%    %         %  %       %   %    %    G4               %
   ;;1         3         4           5     6    7         8  9       10  11   12                    13
   ;;  2
   seed7-procfunc-beg-of-decl-re         ; 1 : includes G1 and G2
   seed7--opt-square-brace-start-re      ; 2 w[
   seed7--whitespace-re                  ; 3
   "[^;]" ; seed7--anychar-re            ; 4
   seed7--whitespace-re                  ; 5
   seed7-name-identifier-nc-re           ; 6
   seed7--special-identifier-nc-re       ; 7
   seed7--whitespace-re                  ; 8
   "[^;]" ; seed7--anychar-re            ; 9
   "[^;]" ; seed7--anychar-re            ; 10
   seed7--opt-square-brace-end-re        ; 11 w]w
   seed7--whitespace-re                  ; 12
   seed7--procfunc-forward-or-action-re) ; 13
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
  "end +func;"
  "Regexp to detect end of procedure or long function.  No group.")

(defconst seed7-short-func-end-regexp
  "^[[:blank:]]+return[^;]*;"
  "Regexp to detect end of short function.  No group.
Uses `[^;]*' (negated character class) instead of nested lazy quantifiers
to prevent catastrophic backtracking on large files.
Matches a return statement starting on a line beginning with whitespace,
spanning any number of continuation lines, ending at the first `;'.")

(defconst seed7-forward-declaration-end-regexp
  "[[:blank:]]*?is[[:blank:]]+?forward;"
  "Regexp to detect end of forward declaration.  No group.")

;;* Seed7 iMenu Support Regexp
;;  ==========================

;;** Seed7 Procedure/Function iMenu Regexp
;;   -------------------------------------

(defconst seed7-proc-forward-or-action-declaration-re
  (format
   "%s\\(%s\\)\\(?:%s\\)+?is%s+?\\(%s\\)"
   ;;   G1                        G2
   ;;%   %         %        %      %
   ;;1   2         3        4      5
   seed7-proc-beg-of-decl-re             ; 1
   seed7-name-identifier-nc-re           ; 2 : G1 : procedure name
   "[^;]"                                ; 3
   seed7--whitespace-re                  ; 4
   seed7--procfunc-forward-or-action-re) ; 5 : G2 : declaration form
  "Regexp matching forward or action procedure declaration.
- Group 1: procedure name,
- Group 2: \"func\", \"forward\", \"DYNAMIC\", \"action XYZ\".")


(defconst seed7-func-forward-or-action-declaration-nc-re
  ;;              (----------------)            (------)
  ;;         (--------------------------------------------)
  ;;              name
  (format "\\(?:%s\\(%s%s+?([^;]+?)\\)%s+?is%s+?\\(?:%s\\)\\)"
          ;;         g     G1
          ;;    %    %  %             %     %        %
          ;;    1    2  3             4     5        6
          (seed7-func-beg-of-decl-re-fmt)       ; 1
          "?:"                                  ; 2: don't capture g
          "[^;]"                                ; 3
          seed7--whitespace-re                  ; 4
          seed7--whitespace-re                  ; 5
          seed7--procfunc-forward-or-action-re) ; 6
  "Regexp matching the complete declaration of value, forward or action function.
No capture group.")

(defconst seed7-func-forward-or-action-declaration-re
  ;;                (----------------)            (------)
  ;;         (----------------------------------------------)
  ;;                name   (params   )
  (format "\\(?:%s\\(%s%s+?([^;]+?)\\)%s+?is%s+?\\(?:%s\\)\\)"
          ;;        G1
          ;;    %    %  %             %     %        %
          ;;    1    2  3             4     5        6
          (seed7-func-beg-of-decl-re-fmt)       ; 1
          ""                                    ; 2: capture G1: name & params
          "[^;]"                                ; 3
          seed7--whitespace-re                  ; 4
          seed7--whitespace-re                  ; 5
          seed7--procfunc-forward-or-action-re) ; 6
  "Regexp matching value, forward or action function declaration.
1 group: function name and parameters.")

(defconst seed7-procfunc-forward-or-action-declaration-re
  ;;                (----------------)            (------)
  ;;         (----------------------------------------------)
  ;;                name   (params   )
  (format "%s\\(%s%s+?([^;]+?)\\)%s+?is%s+?\\(?:%s\\)"
          ;;   G1
          ;;%   %  %             %     %        %
          ;;1   2  3             4     5        6
          seed7-procfunc-beg-of-decl-nc-re      ; 1
          ""                                    ; 2: capture G1: name & params
          "[^;]"                                ; 3
          "[^;]"                                ; 4
          seed7--whitespace-re                  ; 5
          seed7--procfunc-forward-or-action-re) ; 6
  "Regexp matching value, forward or action function declaration.
1 group: function name and parameters.")

;; --
;; Regexp for procedure and function declarations or beginning of block.

(defconst seed7-procedure-regexp-4imenu
  (format
   "^[[:blank:]]*const%s+proc:%s+?\\(%s\\)%s*?is%s+?\\(func\\|%s\\)"
   ;;                               G1                 G2
   ;;                 %       %      %    %     %             %
   ;;                 1       2      3    4     5             6
   seed7--whitespace-re                    ; 1
   seed7--whitespace-re                    ; 2
   seed7-name-identifier-nc-re             ; 3
   seed7--anychar-re                       ; 4
   seed7--whitespace-re                    ; 5
   seed7--procfunc-forward-or-action-re)   ; 6
  "Procedure search regexp.
- Group 1: procedure name,
- Group 2: \"func\", \"forward\", \"DYNAMIC\", \"action XYZ\".")

(defconst seed7-function-regexp-4imenu
  (format
   ;;             const   func T       :                                                                     is      func
   ;;                              w    w[      w                                                      .   w]w w
   "^[[:blank:]]*?const%s+\\(?:var\\)?func %s??%s??:%s?\\(?:%s+?\\(?:(%s+?)\\)\\)?%s*\\(%s\\|%s\\)\\(?:%s(%s+?)\\)?%s*?%s?is%s+\\(func\\|return\\|%s\\)"
   ;;                  %                   G2  %    %        %         %           %   G3%    %         %  %       %   %    %    G4               %
   ;;                  1                   %2  3    4        5         6           7     8    9         10 11      12  13   14                    15
   ;;
   seed7--whitespace-re                         ; 1
   seed7-type-identifier-re                     ; 2
   seed7--whitespace-re                         ; 3
   seed7--opt-square-brace-start-re             ; 4 w[
   seed7--whitespace-re                         ; 5
   seed7--anychar-re                            ; 6
   seed7--whitespace-re                         ; 7
   seed7-name-identifier-nc-re                  ; 8
   seed7--special-identifier-nc-re              ; 9
   seed7--whitespace-re                         ; 10
   seed7--anychar-re                            ; 11
   seed7--anychar-re                            ; 12
   seed7--opt-square-brace-end-re               ; 13 w]w
   seed7--whitespace-re                         ; 14
   seed7--procfunc-forward-or-action-re)        ; 15
  "Regexp identifying beginning of procedures and functions.
Group 1: The function return type.
Group 2: The function name.
Group 3: - \"func\" for proc or function that ends with \"end func\".
         - \"return\" for a func that only has a return statement.
         - \"forward\" for a forward declaration.
         - \"action ACTION\" for an action function." )

;;** Seed7 Enum/Structure iMenu Regexp
;;   ---------------------------------

(defconst seed7-enum-regexp-4imenu
  "const type: \\([[:alpha:]][[:alnum:]_]+\\) is new enum")

(defconst seed7-interface-regexp-4imenu
  (format "const%stype:%s\\(%s\\)%sis%s\\(?:new\\|sub%s%s\\)%sinterface;"
          ;;    %      %    %    %   %             % %    %
          ;;    1      2    3    4   5             6 7    8
          seed7--whitespace-re                    ; 1
          seed7--whitespace-re                    ; 2
          seed7-name-identifier-nc-re             ; 3
          seed7--whitespace-re                    ; 4
          seed7--whitespace-re                    ; 5
          seed7--whitespace-re                    ; 6
          seed7-name-identifier-nc-re             ; 7
          seed7--whitespace-re)                   ; 8
  "Regexp to extract interface type declaration.  Group 1: name of type.")

(defconst seed7-struct-regexp-4imenu
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
;;  - The `seed7-mode-syntax-propertize' neutralizes '#' as comment when used
;;    as a number base separator.

(defvar seed7-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w"   st)   ; underscores are allowed in
                                        ; identifiers, which have word syntax.
    (modify-syntax-entry ?\\ "."   st)
    ;;
    (modify-syntax-entry ?\( "()1n" st) ; The comment "(*" can be nested ...
    (modify-syntax-entry ?\) ")(4n" st) ; ...  and end with the matching "*)"
    (modify-syntax-entry ?* ". 23" st) ; '*' as second of "(*" and first of "*)"
    ;;
    ;; Seed7 Comments Control : line-end comment.
    (modify-syntax-entry ?# "<"  st)
    (modify-syntax-entry ?\n ">" st)
    ;;
    ;; string escape
    (modify-syntax-entry ?\\ "\\"  st)
    ;; single quote: Seed7 supports ''' as well as '\''.  Deal with it in `seed7-mode-syntax-propertize'.
    (modify-syntax-entry ?\' "." st) ; attribute; see `seed7-mode-syntax-propertize' for character literal
    st)
  "Syntax table in use in `seed7-mode' buffers.")

;;** Seed7 Mode Syntax Propertize Function
;;   -------------------------------------

(defconst seed7-char-literal-re-no-comments
  (format
   "\\(?:\\(?:[[:digit:]]\\(#\\)[[:alnum:]]\\)\\|\\(\\(?:'\\\\''\\)\\|\\(?:'.'\\)\\|\\(?:'\\\\[abefnrtv\"A-Z\\\\]'\\)\\|\\(?:%s\\)\\)\\)"
   ;; (----(----------------------------------)----(--------------------------------------------------------------------------------)--)
   ;;                      (---)                   (-----------------------------------------------------------------------------------)
   ;;                                                 (-----------)     (-------)     (-----------------------------)     (------)
   ;;                      G1                      G2                                  backslash-character                   %s
   ;; format argument number:                                                                                                 1
   ;;
   ;; G1: #
   ;; G2: single quote character expression
   ;; backslash characters: \a, \b... :standard named escape letters, including \e for the Escape character.
   ;; backslash characters: \A .. \Z  : standard Seed7 control characters: Ctrl-A .. Ctrl-Z
   seed7-any-valid-char-integer-semicolon-re)
  "Regexp for # and char literals only — no block-comment delimiters.")

(defconst seed7-block-comment-delim-re "(\\*\\|\\*)"
  "Regexp matching the two-character Seed7 block-comment delimiters.
Matches either the opening `(*' or the closing `*)'.")

;; Emacs supports two-character comment delimiters via "style b"
;; syntax text properties. The characters of each delimiter are
;; tagged with 4 codes (see `modify-syntax-entry'):
;;
;;              syntax-
;;              entry
;; Position     code        Meaning
;; ==========   =========   ====================================
;; ( in (*      "< 1bn"     first char of style-b comment-start
;; * in (*      "< 2bn"     second char of style-b comment-start
;; * in *)      "> 3bn"     first char of style-b comment-end
;; ) in *)      "> 4bn"     second char of style-b comment-end
;;
;; Each syntax-entry code ends with 'n' because Seed7 (* *) style
;; comments can be nested.

(defconst seed7--syntax-symbol-constituent
  (string-to-syntax "_")
  "Syntax property value for a symbol constituent.")

(defconst seed7--syntax-block-comment-start-1
  (string-to-syntax "< 1bn")
  "Syntax property value for first character of Seed7 block-comment opener.")

(defconst seed7--syntax-block-comment-start-2
  (string-to-syntax "< 2bn")
  "Syntax property value for second character of Seed7 block-comment opener.")

(defconst seed7--syntax-block-comment-end-3
  (string-to-syntax "> 3bn")
  "Syntax property value for first character of Seed7 block-comment closer.")

(defconst seed7--syntax-block-comment-end-4
  (string-to-syntax "> 4bn")
  "Syntax property value for second character of Seed7 block-comment closer.")

(defun seed7-mode-syntax-propertize (start end)
  "Apply syntax-table text properties between START and END.

Handle four cases:
- the `#' number-base separator,
- single-quoted character literals,
- the `(*' two-character block-comment opener,
- the `*)' two-character block-comment closer."
  ;; See:  (info "(elisp)Syntax Properties")
  (with-silent-modifications
    (save-excursion
      (save-match-data
        ;; Loop 1: `#' and single-quoted char literals.
        ;; Fires only at [[:digit:]] and `'' — rare in a Seed7 file.
        (goto-char start)
        (while (re-search-forward seed7-char-literal-re-no-comments end t)
          (cond
           ((match-beginning 1)         ; # number-base separator
            (put-text-property (match-beginning 1) (match-end 1)
                               'syntax-table seed7--syntax-symbol-constituent))
           ((match-beginning 2)         ; single-quoted char literal
            (put-text-property (match-beginning 2) (1+ (match-beginning 2))
                               'syntax-table '(7 . ?'))
            (put-text-property (1- (match-end 2)) (match-end 2)
                               'syntax-table '(7 . ?')))))
        ;; Loop 2: `(*' and `*)' block-comment delimiters.
        ;; Uses the minimal two-alternative pattern — no 35-branch subpattern.
        ;; These text properties also serve as jit-lock safe-point anchors,
        ;; preventing backward scans across the whole file on each chunk.
        (goto-char start)
        (while (re-search-forward seed7-block-comment-delim-re end t)
          (if (eq (char-after (match-beginning 0)) ?\()
              (progn                    ; (* opener
                (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                   'syntax-table seed7--syntax-block-comment-start-1)
                (put-text-property (1+ (match-beginning 0)) (match-end 0)
                                   'syntax-table seed7--syntax-block-comment-start-2))
            (progn                      ; *) closer
              (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                                 'syntax-table seed7--syntax-block-comment-end-3)
              (put-text-property (1+ (match-beginning 0)) (match-end 0)
                                 'syntax-table seed7--syntax-block-comment-end-4))))))))

(defun seed7--font-lock-block-comment-delimiter (limit)
  "Search for a Seed7 block-comment delimiter before LIMIT.

Match only syntactically active `(*' and `*)' delimiters, and expose the
whole two-character delimiter as match 0 for font-lock."
  ;; Uses O(1) `get-text-property' checks instead of `syntax-ppss' to avoid
  ;; catastrophic O(N²) backward scanning when a buffer contains an unterminated
  ;; block comment (e.g., `prg/err.sd7').
  (catch 'found
    (while (re-search-forward seed7-block-comment-delim-re limit t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (when
            (cond
             ;; Opening delimiter `(*':
             ;; `seed7-mode-syntax-propertize' sets `seed7--syntax-block-comment-start-1'
             ;; on the `(' of every syntactically active opener — O(1) check.
             ((eq (char-after beg) ?\()
              (equal (get-text-property beg 'syntax-table)
                     seed7--syntax-block-comment-start-1))
             ;; Closing delimiter `*)':
             ;; at the `*', parser should still be inside comment.
             ((eq (char-after beg) ?*)
              (equal (get-text-property beg 'syntax-table)
                     seed7--syntax-block-comment-end-3)))
          ;; Preserve match data for font-lock.
          (set-match-data (list beg end))
          (throw 'found t))))
    nil))

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
  "Use the first color available from the specified LIST of color names.

Allows selecting similar colors for various systems."
  (let (answer)
    (while list
      (or answer
          (if (or (color-defined-p (car list))
                  (null (cdr list)))
              (setq answer (car list))))
      (setq list (cdr list)))
    answer))

(defcustom seed7-dark-background
  'unspecified
  "Background color for Seed7 faces when Emacs uses a dark background display.

The default value `unspecified' lets faces inherit the background from the
user's active theme, which is the modern Emacs convention and works correctly
with popular dark themes (Modus Vivendi, Doom, Solarized Dark, etc.).

Set this to a color name string (e.g. \"navy\") if you prefer an explicit
background stripe behind Seed7 syntax elements."
  :group 'seed7-faces
  :type '(choice (const  :tag "Inherit from theme (recommended)" unspecified)
                 (color  :tag "Explicit background color")))


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
  "Font Lock mode face that highlights float values."
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
  "Font Lock mode face that highlights integer values."
  :group 'seed7-faces)


(define-obsolete-face-alias 'seed7-number-face 'seed7-big-integer-face
                             "2026-06-16")

(defface seed7-big-integer-face
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
  "Font Lock mode face that highlights number values.
The old name, seed7-number-face, remains available until the end of 2026.
Please update your code to use the new name before this deadline."
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
     (:foreground "gray80" :background ,seed7-dark-background
                  :weight bold))

    (t (:weight bold)))
  "Font Lock mode face that highlights identifiers."
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
   ;; block-comment delimiters
   '(seed7--font-lock-block-comment-delimiter        (0 'font-lock-comment-delimiter-face t))
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
   (cons seed7-base-x-big-integer-re                 (list 1 ''seed7-big-integer-face))
   (cons seed7-base-x-integer-re                     (list 1 ''seed7-integer-face))
   (cons seed7-number-with-negative-exponent-re      (list 0 ''font-lock-warning-face))
   (cons seed7-number-with-exponent-re               (list 0 ''seed7-integer-face))
   (cons seed7-integer-invalid-0x-re                 (list 1 ''font-lock-warning-face))
   ;; predefined constants (includes 'E'). Must be rendered after float numbers.
   (cons seed7-predefined-constants-regexp           (list 1 ''font-lock-constant-face))
   ;; predefined variables
   (cons seed7-predefined-variables-regexp           (list 1 ''seed7-predefined-variables-face))
   ;; predefined errinfo values
   (cons seed7-errinfo-values-regexp                 (list 1 ''seed7-errinfo-value-face))
   ;; operator symbols
   (cons seed7-operator-symbols-regexp               (list 1 ''font-lock-keyword-face))
   (cons seed7-predef-assignment-operator-regexp     (list 0 ''font-lock-keyword-face))
   (cons seed7-other-predef-operator-regexp          (list 0 ''font-lock-keyword-face)) ; before comparison because that has single char operators that are part of other predef
   (cons seed7-predef-comparison-operator-regexp     (list 0 ''font-lock-keyword-face))
   (cons seed7-arithmetic-operator-regexp            (list 1 ''font-lock-keyword-face))
   (cons "[[:alnum:] _)]\\(/\\)[[:alnum:] _(]"       (list 1 ''font-lock-keyword-face)) ; /
   (cons "[[:alnum:] _)]\\(\\*\\*\\)[[:alnum:] _(]"  (list 1 ''font-lock-keyword-face)) ; **
   ;; logic operator
   (cons "[\n[:alnum:] _)\\\"]\\([&|]\\)[\n[:alnum:] _(\\\"]" (list 1 ''font-lock-keyword-face)) ; &

   ;; invalid single quote char literals
   (cons seed7--invalid-char-literal-re              (list 1 ''font-lock-warning-face))

   ;; identifiers: include the underscore (must be done before
   ;; seed7-big-integer-re to prevent something like 'f10_fct' from being
   ;; rendered as a partial number.)
   (cons seed7-name-identifier-re                    (list 1 ''seed7-name-identifier-face))

   ;; big numbers: have an underscore
   (cons seed7-big-integer-re                         (list 1 ''seed7-big-integer-face))

   ;; other numbers
   (cons seed7-integer-re                            (list 1 ''seed7-integer-face))
   ;; low priority rendering of arithmetic + and -
   (cons seed7-minus-operator-regexp                 (list 1 ''font-lock-keyword-face))

   ;; other low priority characters
   ;; [ TODO 2025-07-09, by Pierre Rouleau: check if any missing and improve control of ..]
   (cons "[[:print:]]\\(\\(?:~\\)\\|\\(?:\\.\\.\\)\\)[[:print:]]"   (list 1 ''font-lock-keyword-face)))
  "Associates regexp to a regexp group and a face to render it.")

;; ---------------------------------------------------------------------------
;;* Seed7 Comments Control
;;  ======================
;;
;; - Region:      "(\*" "\*)"
;; - To line end: "#"

(defconst seed7-block-comment-starter  "(*")
(defconst seed7-block-comment-ender    "*)")
(defconst seed7-block-comment-prefix   "**")
(defconst seed7-block-comment-continue "* ")

(defconst seed7-line-comment-starter  "#")
(defconst seed7-line-comment-continue "#")


(defun seed7--new-state-for (arg prevstate)
  "Calculate the new state of PREVSTATE, t or nil, based on ARG.

- If ARG is nil or zero, toggle the state,
- If ARG is negative, turn the state off,
- If ARG is positive, turn the state on."
  (declare (side-effect-free t))
  (if (or (not arg)
          (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

(defun seed7--set-comment-style (use-block &optional verbose)
  "Set Seed7 buffer comment style to block style when USE-BLOCK is non-nil.
Set it to line-style otherwise.  Only affects current buffer.
Print message when VERBOSE is non-nil.
Note: the default style for all Seed7 buffers is controlled by the
`seed7-uses-block-comment' customizable user-option."
  (let ((cblock-start    seed7-block-comment-starter)
        (cblock-end      seed7-block-comment-ender)
        (cblock-continue seed7-block-comment-continue)
        (cline-start     seed7-line-comment-starter)
        (cline-continue  seed7-line-comment-continue))
    (setq-local seed7-uses-block-comment use-block)
    ;; comment-start, comment-end, comment-continue are not regexp
    ;; they are strings matched directly.
    (setq-local comment-start    (concat (if use-block
                                             cblock-start
                                           cline-start)
                                         " "))
    (setq-local comment-end      (if use-block (concat " " cblock-end) ""))
    (setq-local comment-continue (if use-block
                                     (concat " " cblock-continue)
                                   cline-continue))
    ;; comment-skip-start is a regexp
    (setq-local comment-start-skip (if use-block
                                       (format "%s\\s-*"
                                               (regexp-quote cblock-start))
                                     "#+\\s-*") )

    (when verbose
      (message "Now use %s style comments" (if use-block "block" "line")))))

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
;;* Seed7 Utilities
;;  ===============

(defun seed7--plural-s (n)
  "Return \"s\" if N is not equal to 1, \"\" otherwise."
  (declare (pure t) (side-effect-free t))
  (if (= n 1) "" "s"))

(defun seed7--run (program args stdout-buffer
                           &optional stderr-buffer context-msg)
  "Run PROGRAM with ARGS synchronously.

- If STDERR-BUFFER is nil: collect PROGRAM output from stdout and stderr in
  STDOUT-BUFFER.
- If STDERR-BUFFER is non-nil, it must be a buffer; the stdout and stderr
  streams of PROGRAM are collected separately.

If execution of PROGRAM fails (e.g. the OS cannot start it), the function
issues a `user-error'.  When CONTEXT-MSG is non-nil it is appended to the
error message; when nil the message ends with the OS error description.

Return the exit code of the PROGRAM execution as an integer.
When the process is killed by a signal, `call-process' returns a string;
this function normalises it to 1."
  ;; When stderr capture is required, create a temporary file to hold the
  ;; output.
  (let ((temp-stderr-file (when stderr-buffer
                            (make-temp-file "emacs-seed7-mode-stderr-"))))
    ;; Clear previous contents of both buffers
    (with-current-buffer stdout-buffer (erase-buffer))
    (when stderr-buffer
      (with-current-buffer stderr-buffer (erase-buffer)))
    (unwind-protect
        (let* ((dest (if stderr-buffer
                         (list stdout-buffer temp-stderr-file)
                       stdout-buffer))
               (exit-code
                (condition-case err
                    (apply #'call-process program nil dest nil args)
                  (error
                   (user-error "\
seed7: cannot run \"%s\"%s: %s"
                               program
                               (if context-msg
                                   (format "  with %s" context-msg)
                                 "")
                               (error-message-string err))))))
          ;; When used, read the collected stderr file back into the target
          ;; stderr buffer.
          (when stderr-buffer
            (with-current-buffer stderr-buffer
              (unless (integerp exit-code)
                (insert (format "Error: %S\n" exit-code)))
              (insert-file-contents temp-stderr-file)))
          ;; Normalise: call-process returns a string when killed by a signal.
          (unless (integerp exit-code)
            (setq exit-code 1))
          ;; Return the exit code of the process
          exit-code)
      ;; When used, delete temp file even if the process or insertion fails.
      (when (and stderr-buffer (file-exists-p temp-stderr-file))
        (delete-file temp-stderr-file)))))

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

(defun seed7-inside-comment-p (&optional pos)
  "Return non-nil if POS or point is part of a comment.

This includes positions where `syntax-ppss' reports being inside a
comment, plus comment delimiter boundary characters that belong to
the comment text:

- the `#' character that starts a line-end comment,
- the `(' character of a `(*' block-comment opener,
- the `)' character of a `*)' block-comment closer.

Return nil otherwise.
Does not move point."
  (save-excursion
    (let* ((pos (or pos (point)))
           (syntax (syntax-ppss pos)))
      (or
       ;; Normal case: POS is syntactically inside a comment.
       (nth 4 syntax)

       ;; Boundary case 1: POS is at the `#' that starts a line-end comment.
       ;; `syntax-ppss' at POS reports the state before consuming `#', so look
       ;; one character later.  For number-base separators, syntax-propertize
       ;; gives `#' symbol syntax, so looking after it will not enter a comment.
       (and (eq (char-after pos) ?#)
            (< pos (point-max))
            (nth 4 (syntax-ppss (1+ pos))))

       ;; Boundary case 2: POS is at the `(' of a `(*' block-comment opener.
       ;; Check after the full two-character delimiter has been consumed.
       (and (eq (char-after pos) ?\()
            (< (1+ pos) (point-max))
            (eq (char-after (1+ pos)) ?*)
            (nth 4 (syntax-ppss (+ pos 2))))

       ;; Boundary case 3: POS is at the `)' of a `*)' block-comment closer.
       ;; At `)' itself the comment is already closed, so check the previous
       ;; `*', which is still syntactically inside the comment.
       (and (eq (char-after pos) ?\))
            (> pos (point-min))
            (eq (char-before pos) ?*)
            (nth 4 (syntax-ppss (1- pos))))))))

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
          (unless (or (seed7-inside-comment-p)
                      (seed7-inside-string-p))
            ;; Found in code!
            (setq found-pos (point)
                  keep-searching nil))
        ;; Not found. stop.
        (setq keep-searching nil)))
    found-pos))

(defun seed7-re-search-forward-closest (regexps)
  "Search for all specified regexp in REGEXPS and stop at the closest found.

Return position of the closest match end, nil if nothing found.
After returning, match data reflects the regexp that produced the closest
match."
  (let* ((candidates nil)
         (closest-beg.end.match-data
          (dolist (regexp regexps (car-safe
                                   (sort
                                    (seq-filter #'identity candidates)
                                    ;; sort by start position (the nearest)
                                    (lambda (a b) (< (nth 0 a) (nth 0 b))))))
            (save-excursion
              (when (seed7-re-search-forward regexp)
                ;; Store (start-of-match-pos end-of-match-pos match-data).
                ;; seed7-re-search-forward returns end-of-match-pos
                ;; match-beginning 0 is the true match start.
                (push (list (match-beginning 0) (match-end 0) (match-data))
                      candidates))))))
    (when closest-beg.end.match-data
      ;; restore match start pos
      (goto-char (nth 0 closest-beg.end.match-data))
      ;; restore the match data by searching again without moving
      (set-match-data (nth 2 closest-beg.end.match-data))
      ;; set point to end of match and return it
      (goto-char (nth 1 closest-beg.end.match-data)))))

(defun seed7-re-search-backward (regexp &optional bound)
  "Search for REGEXP inside code.  Skip comment and strings.
The optional second argument BOUND is a buffer position that bounds
the search.

The match found must not begin before that position. A nil value means
search to the beginning of the accessible portion of the buffer.

Return found position or nil if nothing found.
Move point."
  (let ((found-pos nil)
        (keep-searching t)
        ;; prevent case fold searching: Seed7 is case sensitive.
        (case-fold-search nil))
    (with-timeout
        (10                             ; ← give up after 10 s; warn user.
         (message
          (concat "seed7-mode: regexp search timed out at line %d. "
                  "This indicates a catastrophic-backtracking bug. "
                  "Please enable the debugger (M-x toggle-debug-on-quit), "
                  "reproduce with C-g, and report the backtrace together "
                  "with the current file and line number.")
          (line-number-at-pos (point))))
      (while (and keep-searching
                  (not (bobp)))
        (if (re-search-backward regexp bound :noerror)
            (unless (or (seed7-inside-comment-p)
                        (seed7-inside-string-p))
              ;; Found in code!
              (setq found-pos (point))
              (setq keep-searching nil))
          ;; Not found. stop.
          (setq keep-searching nil))))
    found-pos))

(defun seed7-re-search-backward-closest (regexps &optional get-end-pos)
  "Search for all specified regexp in REGEXPS and stop at the closest found.

Return position of the closest match start (or end when GET-END-POS is
non-nil), nil if nothing found.

After returning, match data reflects the regexp that produced the closest
match."
  ;; search with all regexp in the regexps list.
  (let* ((candidates nil)
         (closest-beg.end.match-data
          (dolist (regexp regexps (car-safe
                                   (sort
                                    (seq-filter #'identity candidates)
                                    ;; sort by end position (the nearest)
                                    (lambda (a b) (> (nth 1 a) (nth 1 b))))))
            ;; For each search, store the begin/end positions and the regexp
            (save-excursion
              (when (seed7-re-search-backward regexp)
                (push (list (match-beginning 0) (match-end 0) (match-data))
                      candidates))))))
    ;; If something is found, restore the global match data that corresponds
    ;; to the regexp that finds the closest position and return the requested
    ;; position
    (when closest-beg.end.match-data
      ;; restore match start pos
      (goto-char (nth 0 closest-beg.end.match-data))
      ;; restore the match data by searching again without moving
      (set-match-data (nth 2 closest-beg.end.match-data))
      ;; set point and return position of the requested end
      (goto-char (nth (if get-end-pos 1 0)
                      closest-beg.end.match-data)))))


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
  "Skip comment block -- ignores nesting.
Only used by `seed7-skip-comment-forward'."
  ;; note: Seed7 supports nested block comments.  This function does not
  ;;       move point out of nested comments, it just moves point to the end
  ;;       of the current comment even if it is nested inside another one.
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

;;** Seed7 forward-sexp/backward-sexp Support
;;   ----------------------------------------

(defun seed7--forward-block-comment (n)
  "Move over N nested `(* ... *)' block comment pairs.
Positive N: point must be at `(' of `(*'.
Negative N: point must be just past `)' of `*)'."
  (let ((dir (if (> n 0) 1 -1))
        (count (abs n)))
    (while (> count 0)
      (if (> dir 0)
          ;; -- Forward --------------------
          ;; Point is at ( of (*
          (let ((depth 1))
            (forward-char 2)                      ; step past (*
            (while (> depth 0)
              (cond
               ((eobp)
                (signal 'scan-error
                        (list "Unbalanced `(*'" (point) (point))))
               ((looking-at-p "(\\*")
                (setq depth (1+ depth))
                (forward-char 2))
               ((looking-at-p "\\*)")
                (setq depth (1- depth))
                (forward-char 2))
               (t
                (forward-char 1)))))
        ;; -- Backward ---------------------
        ;; Point is just after ) of *)
        (let ((depth 1))
          (backward-char 2)                       ; step before *)
          (while (> depth 0)
            (let ((found (re-search-backward
                          "(\\*\\|\\*)" nil t)))
              (unless found
                (signal 'scan-error
                        (list "Unbalanced `*)'" (point) (point))))
              ;; re-search-backward leaves point at start of match
              (if (looking-at-p "(\\*")
                  (setq depth (1- depth))    ; found (*  → close a level
                (setq depth (1+ depth))))))) ; found *)  → open a level
      (setq count (1- count)))))

(defun seed7--line-comment-hash ()
  "Return position of the `#' line-comment start on the current line, or nil.
Only returns a position if the `#' has comment-start syntax (class 11),
excluding `#' used as a Seed7 number-base separator."
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (while (re-search-forward "#" (line-end-position) t)
        (let* ((pos (1- (point)))
               (s   (syntax-after pos)))
          (when (and s (= (syntax-class s) 11))   ; 11 = comment-start (<)
            (throw 'found pos))))
      nil)))

(defun seed7--at-line-comment-start-p ()
  "Return non-nil if point is at a `#' that is a line-comment start."
  (and (eq (char-after) ?#)
       (let ((s (syntax-after (point))))
         (and s (= (syntax-class s) 11)))))

(defun seed7--forward-line-comments (n)
  "Move over N consecutive `#' line-end comment blocks.
Positive N: point must be at the `#' of a line comment.
  Moves forward to end of the last consecutive commented line.
Negative N: point is on a line that contains a line comment.
  Moves backward to the `#' of the first consecutive commented line."
  (let ((dir   (if (> n 0) 1 -1))
        (count (abs n)))
    (while (> count 0)
      (if (> dir 0)
          ;; -- Forward ----------
          ;; Point is at # on the current line; advance to end of the
          ;; last consecutive commented line.
          (progn
            (end-of-line)
            (while (save-excursion                  ; peek — don't move yet
                     (and (= (forward-line 1) 0)
                          (seed7--line-comment-hash)))
              (forward-line 1)                      ; next line is a comment: commit
              (end-of-line)))                       ; point stays here when loop exits
        ;; -- Backward ------------
        ;; Point is somewhere on a commented line; retreat to the # of
        ;; the first consecutive commented line.
        (let ((hash-pos (seed7--line-comment-hash)))
          (unless hash-pos
            (signal 'scan-error
                    (list "Not on a `#' comment line" (point) (point))))
          (goto-char hash-pos)
          (while (save-excursion                    ; peek — don't move yet
                   (and (= (forward-line -1) 0)
                        (seed7--line-comment-hash)))
            (forward-line -1)
            (goto-char (seed7--line-comment-hash)))))
      (setq count (1- count)))))

;;** Seed7 Navigation by Block/Procedure/Function
;;   --------------------------------------------


;;*** Navigation to Outer Block
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7--to-top (&optional pos)
  "Move point to beginning of outer block surrounding code at POS or point.
Searches backward for the nearest enclosing block-start keyword whose
column is strictly less than the column at POS.  This is O(nesting depth),
not O(number-of-keyword-occurrences), and correctly handles both top-level
declarations at column 0 (in .sd7 files) and indented declarations inside
Seed7 template bodies (in .s7i files)."
  (when pos (goto-char pos))
  (let ((start-col (current-column))
        (found nil))
    (with-timeout
        (15
         (message
          (concat "seed7-mode: seed7--to-top timed out at line %d "
                  "(could not find enclosing block start). Please report.")
          (line-number-at-pos (point))))
      (while (and (not found)
                  (not (bobp))
                  (seed7-re-search-backward
                   seed7--callable-declaration-start-regexp))
        (seed7-to-indent)
        ;; Stop as soon as we find a block-start keyword whose column is
        ;; strictly less than the starting column.  This correctly handles:
        ;; • .sd7 files: indented lines (col ≥ 2) → column-0 keyword found
        ;;   in 1–2 iterations.
        ;; • .s7i template bodies: indented `const func' (col 4) → enclosing
        ;;   `begin' or `const proc:' at col 0 found in 1–3 iterations.
        (when (< (current-column) start-col)
          (setq found t))))))

(defun seed7-to-top-of-block ()
  "Move point to the top of the current block."
  (interactive)
  (seed7--to-top))

(defun seed7--block-name (&optional pos end-pos)
  "Return the name of the block declared at POS or point.
If END-POS is non-nil it specifies last position.
Return nil if name is not found."
  (save-excursion
    (when pos (goto-char pos))
    (when (seed7-re-search-forward seed7-procfunc-regexp end-pos)
      (substring-no-properties (match-string
                                seed7-procfunc-regexp-item-name-group)))))

(defun seed7-top-block-name (&optional pos end-pos)
  "Return the name of the top block item surrounding code at POS or point.
If END-POS is specified it specifies the last possible position."
  (save-excursion
    (seed7--to-top pos)
    (or (seed7--block-name nil end-pos) "?")))

;;*** Seed7 Procedure/Function Search Utility functions
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7--move-and-mark (original-pos final-pos dont-push-mark info)
  "Move point if necessary, push mark if necessary, print info if any.

- ORIGINAL-POS and FINAL-POS are the original and final position
  of the operations.
- DONT-PUSH-MARK a flag indicating whether mark should be pushed.
- INFO a string to issue as message if non-nil."
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
  (declare (side-effect-free t))
  (if (eq position 'at-start-of)
      (format "@ start of %s: '%s'" qualifier name)
    (format "@ end of %s : '%s'" qualifier name)))

(defun seed7--show-info (position name type tail-type)
  "Return a navigation message for a function or procedure at POSITION.

The returned string is formatted by `seed7--pos-msg' as either
  \"@ start of TYPE: \\='DISPLAY-NAME\\='\"
or
  \"@ end of TYPE : \\='DISPLAY-NAME\\='\"
depending on POSITION (symbol `at-start-of' or `at-end-of').

Arguments:

- POSITION: symbol `at-start-of' or `at-end-of', passed directly to
  `seed7--pos-msg' to select the start/end label.

- NAME: the bare name of the function or procedure, extracted from
  match group `seed7-procfunc-regexp-item-name-group' (group 3) of
  `seed7-procfunc-regexp'.

- TYPE: the definition keyword: one of \"proc\", \"varfunc\", or \"func \",
  extracted from match group `seed7-procfunc-regexp-item-type-group'
  (group 1) of `seed7-procfunc-regexp'.  Used as the qualifier label
  in the message.

- TAIL-TYPE: the declaration form, extracted from match group
  `seed7-procfunc-regexp-tail-type-group' (group 4) of
  `seed7-procfunc-regexp'.  Controls the DISPLAY-NAME in the message:
  - \"func\" or \"return\": DISPLAY-NAME is NAME unchanged.
  - \"forward;\": DISPLAY-NAME is \"forward declaration of NAME\".
  - Anything else (e.g. \"action \\\"FOO\\\";\"):
    DISPLAY-NAME is \"NAME, TAIL-TYPE\"."
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

- DIRECTION: symbol: either forward or backward."
  (format "There's no %sSeed7 function%s or procedure%s found %s!"
          (if (= n 1) "" (format "%d " n))
          (seed7--plural-s n)
          (seed7--plural-s n)
          (if (eq direction 'forward) "below" "above")))


;;*** Seed7 Procedure/Function Navigation Commands
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; [ TODO 2025-06-30, by Pierre Rouleau: Add support for multiple lines]
(defconst seed7---inner-callables-1
  ;;          (---------------)               (----------)
  ;;                                     (--------------------)
  ;; (-----------------------------------------------------------)
  "\\(const \\(?:func\\|proc\\)[^;]+?is\\(?:\\(?: +func\\)?$\\)\\)"
  "Group 1: complete text.")

;; [ TODO 2025-06-30, by Pierre Rouleau: Add support for multiple lines]
;; AFTER — with fixed seed7--any-wp-text-re the format is already correct;
;;          also drop the extra +? that caused the +?+? double-quantifier:
(defconst seed7---inner-callables-2
  (format
   "\\(\\(?:end \\(?:func\\|proc\\);\\)\\|\\(?:return%s;\\)\\)"
   ;;                                                ^^ no +? here; [^;]+ already quantifies
   ;;             (---------------)
   ;;    (----------------------------)     (-------------)
   ;;  (-----------------------------------------------------)
   seed7--any-non-semicolon-re)
  "Group 1: entire text.")


(defconst seed7---inner-callables-4
  "const proc: .+? is forward;")

(defconst seed7--callable-return-re
  (format "return%s;"
          seed7--any-non-semicolon-re))

(defconst seed7--inner-callables-triplets-re
  (format
   "^[[:blank:]]*?\\(?:%s\\|%s\\|%s\\)"
   seed7---inner-callables-1                        ; G1
   seed7---inner-callables-2                        ; G2
   seed7-func-forward-or-action-declaration-nc-re)  ; G3
  "A regexp with 3 groups:
- group 1: function/procedure start,
- group 2: function/procedure end,
- group 3: action or forward function declaration.")

(defun seed7-beg-of-defun (&optional n silent dont-push-mark)
  "Move backward to the beginning of the current function or procedure.

- With optional argument N, repeat the search that many times and succeed
  only when that many function or procedures are found.
  A value of zero means no action.  A nil value is equivalent to 1.
  A Negative N means move forward to the Nth following beginning of defun.
- Unless SILENT, the function prints a message showing the name of the
  found function or procedure.  If it found nothing it issues a user error.
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
         (tail-type nil)
         (long-body-only nil))
    (if (< n 0)
        (seed7-end-of-defun (abs n) silent dont-push-mark)
      (unless (eq n 0)
        (save-excursion
          ;; ---------------------------------------------------------------------------
          ;; Choose a stable starting position for the backward scan:
          ;; - If already on a declaration line, leave point there; searching backward
          ;;   from the declaration should find the enclosing/previous callable.
          ;; - If on an `end func;' / block-end line, start at BOL so the current end
          ;;   is not counted as an inner nested close.
          ;; - If on a short-function `return ...;' line, also start at BOL so the
          ;;   matching declaration immediately above can be found.
          ;; - Otherwise search backward from EOL of the current line.
          (cond
           ((save-excursion
              (forward-line 0)
              (looking-at-p seed7-procfunc-beg-of-decl-re))
            (forward-line 0))
           ((save-excursion
              (seed7-to-indent)
              (looking-at-p seed7-block-end-regexp))
            (forward-line 0))
           ((save-excursion
              (forward-line 0)
              (looking-at-p seed7-short-func-end-regexp))
            (forward-line 0))
           (t
            (end-of-line)))
          (dotimes (_ n)
            (setq found-pos nil
                  ;; When an iteration starts on an `end func;' line,
                  ;; only a long-body callable declaration can match it.
                  long-body-only
                  (save-excursion
                    (seed7-to-indent)
                    (looking-at-p seed7-procfunc-end-regexp)))
            ;;
            ;; Block A — nesting-aware backward search for the enclosing
            ;; long-body proc/func declaration (`const proc/func … is func').
            ;;
            ;; Scan backward with `seed7--inner-callables-triplets-re':
            ;;
            ;;   Group 1 — proc/func declaration start:
            ;;     nesting = 0 → this is the enclosing declaration; record it.
            ;;     nesting > 0 → a previously entered nested scope has been fully
            ;;                   traversed; decrement depth and continue.
            ;;
            ;;   Group 2 — "end func;" / "end proc;" or "return …;" (short func):
            ;;     Only text starting with "end " increments the nesting counter.
            ;;     Short-function return lines do not open a new scope, so they
            ;;     are silently skipped.
            ;;
            ;;   Group 3 — forward / action declaration:
            ;;     Not a nesting boundary; ignored here.  Handled in Block B.
            ;;
            (let ((long-func-pos  nil)
                  (long-item-type nil)
                  (long-item-name nil)
                  (long-tail-type nil))
              (save-excursion
                (let ((nesting   0)
                      (searching t))
                  (while (and searching (not (bobp)))
                    (if (seed7-re-search-backward seed7--inner-callables-triplets-re)
                        (cond
                         ;; Group 1: a proc/func declaration start.
                         ((match-beginning 1)
                          (let ((is-long-body
                                 (string-match-p
                                  "\\bis[[:blank:]]+\\(?:func\\|proc\\)[[:blank:]]*\\'"
                                  (match-string-no-properties 1))))
                            (if (eq nesting 0)
                                ;; At nesting=0: stop only when this declaration
                                ;; is compatible with the current search mode.
                                ;; In long-body-only mode (invoked from `end func;'):
                                ;;   skip short-function declarations (plain `is');
                                ;;   they have no matching `end func;'.
                                ;; In normal mode (invoked from `return …;' etc.):
                                ;;   stop at any callable declaration.
                                (when (or is-long-body (not long-body-only))
                                  (setq searching nil)
                                  (when (looking-at seed7-procfunc-regexp)
                                    (setq long-func-pos (point)
                                          long-item-type
                                          (substring-no-properties
                                           (or (match-string
                                                seed7-procfunc-regexp-item-type-group)
                                               "?"))
                                          long-item-name
                                          (substring-no-properties
                                           (or (match-string
                                                seed7-procfunc-regexp-item-name-group)
                                               "?"))
                                          long-tail-type
                                          (substring-no-properties
                                           (or (match-string
                                                seed7-procfunc-regexp-tail-type-group)
                                               "?")))))
                              ;; nesting > 0: only long-body declarations
                              ;; (`is func'/`is proc') have a matching `end func;'
                              ;; that incremented the counter.  Short-function
                              ;; declarations must not change the nesting depth.
                              (when is-long-body
                                (setq nesting (1- nesting))))))
                         ;; Group 2: long-function end or short-function return.
                         ;; Only "end func;" / "end proc;" changes nesting depth.
                         ((match-beginning 2)
                          (let ((matched-text (match-string 2)))
                            (when (and matched-text
                                       (string-match-p "\\`end " matched-text))
                              (setq nesting (1+ nesting)))))
                         ;; Group 3: forward/action declaration.
                         ;; Not a nesting boundary; skip silently.
                         (t nil))
                      ;; No more matches: stop the search.
                      (setq searching nil)))))
              ;;
              ;; Block B — non-nesting search for the nearest forward or action
              ;; declaration.  These declarations cannot contain nested functions,
              ;; so no nesting counter is needed.
              ;;
              (let ((fwd-pos       nil)
                    (fwd-item-type nil)
                    (fwd-item-name nil)
                    (fwd-tail-type nil))
                (save-excursion
                  ;; Skip this search when `long-body-only' is set: action and
                  ;; forward declarations have no `end func;', so they can never
                  ;; be the matching start of the `end func;' we came from.
                  (when (and (not long-body-only)
                             (seed7-re-search-backward-closest
                              (list seed7-proc-forward-or-action-declaration-re
                                    seed7-procfunc-forward-or-action-declaration-re)))
                    (setq fwd-pos       (point)
                          fwd-item-type
                          (substring-no-properties
                           (or (match-string seed7-procfunc-regexp-item-type-group) "?"))
                          fwd-item-name
                          (substring-no-properties
                           (or (match-string seed7-procfunc-regexp-item-name-group) "?"))
                          fwd-tail-type
                          (substring-no-properties
                           (or (match-string seed7-procfunc-regexp-tail-type-group) "?")))))
                ;;
                ;; Choose the candidate that is closer to the starting point,
                ;; i.e., the one with the greater buffer position (since we
                ;; searched backward, a higher position means it is nearer).
                ;;
                (cond
                 ;; Both candidates found: keep the one closest to the start.
                 ((and long-func-pos fwd-pos)
                  (if (> long-func-pos fwd-pos)
                      (setq found-pos  long-func-pos
                            item-type  long-item-type
                            item-name  long-item-name
                            tail-type  long-tail-type)
                    (setq found-pos  fwd-pos
                          item-type  fwd-item-type
                          item-name  fwd-item-name
                          tail-type  fwd-tail-type)))
                 ;; Only the long-func candidate was found.
                 (long-func-pos
                  (setq found-pos long-func-pos
                        item-type long-item-type
                        item-name long-item-name
                        tail-type long-tail-type))
                 ;; Only the forward/action candidate was found.
                 (fwd-pos
                  (setq found-pos fwd-pos
                        item-type fwd-item-type
                        item-name fwd-item-name
                        tail-type fwd-tail-type)))))
            ;;
            ;; Advance point to the found position so that the next dotimes
            ;; iteration starts from there (Block A and Block B each run
            ;; under their own `save-excursion', so point must be moved
            ;; explicitly here, inside the outer `save-excursion').
            ;;
            (when found-pos
              (goto-char found-pos))))
        (if found-pos
            (let ((top-block-name (seed7-top-block-name nil original-pos)))
              (when (and top-block-name
                         (not (string= top-block-name item-name)))
                (setq item-name (format "%s %s" top-block-name item-name)))
              (seed7--move-and-mark
               original-pos
               found-pos
               dont-push-mark
               (unless silent
                 (seed7--show-info 'at-start-of item-name item-type tail-type))))
          (user-error (seed7--no-defun-found-msg-for n 'backward)))))))

(defun seed7-beg-of-next-defun (&optional n silent dont-push-mark)
  "Move forward to the beginning of the next function or procedure.

- With optional argument N, repeat the search that many times and succeed
  only when that many function or procedures are found.
  A value of zero means no action.   A negative value is not allowed and
  raises a user error.
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
  A value of zero means no action.  A negative value means move backward to
  the Nth preceding start of defun.
- Unless SILENT, the function prints a message showing the item-name of the new
  found function or procedure.  If it found nothing it issues a user error.
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
         (long-body-final-pos nil)
         (item-name nil)         (item-name2 nil)
         (item-type nil)         (item-type2 nil)
         (tail-type nil)         (tail-type2 nil)
         (top-block-name nil)    (top-block-name2 nil))
    (if (< n 0)
        (seed7-beg-of-defun (abs n) silent dont-push-mark)
      (unless (eq n 0)
        (save-excursion
          (dotimes (_ n)
            (setq found-candidate nil
                  final-pos nil
                  found-pos nil
                  long-body-final-pos nil)
            ;; Search for all possible function/procedure end.
            ;; - Retain the one that is closest to point.

            ;; -- Search for next procedure or long function, nesting-aware. -
            ;; Uses `seed7--inner-callables-triplets-re' to count open/close
            ;; func/proc scopes so that a nested function does not cause an
            ;; early stop.
            ;;   Group 1: proc/func declaration start   → nesting depth increases
            ;;   Group 2: proc/func end  ("end func/proc;") or short-func return
            ;;            → only "end func/proc;" affects nesting; "return ...;"
            ;;              belongs to an inner short function and is ignored here.
            ;;   Group 3: action/forward declaration    → not a nesting boundary
            (save-excursion
              ;; If point is on the declaration line, skip that declaration before
              ;; scanning.  Otherwise the current callable is counted as nested and its
              ;; real `end func;' is skipped.
              (when (save-excursion
                      (forward-line 0)
                      (looking-at-p seed7-procfunc-beg-of-decl-nc-re))
                (end-of-line))
              (let ((nesting 0)
                    (searching t)
                    matched-end-pos)
                (while (and searching (not (eobp)))
                  (if (seed7-re-search-forward seed7--inner-callables-triplets-re)
                      (cond
                       ;; Group 1: callable declaration start.
                       ;; Only long-body declarations ending in "is func" open a nested
                       ;; callable for this scan.  Short functions ending in plain "is"
                       ;; are handled by the short-function candidate search below.
                       ((match-beginning 1)
                        (when (string-match-p "\\bis[[:blank:]]+func[[:blank:]]*\\'"
                                              (match-string-no-properties 1))
                          (setq nesting (1+ nesting))))
                       ;; Group 2: end func/proc; or return ...;
                       ((match-beginning 2)
                        (let ((matched-text (match-string 2)))
                          (when (and matched-text
                                     (string-match-p "\\`end " matched-text))
                            ;; It is a long-function end ("end func;" or "end proc;").
                            (if (> nesting 0)
                                ;; Closes a nested inner block.
                                (setq nesting (1- nesting))
                              ;; nesting = 0: this is the end that matches our
                              ;; starting declaration.
                              (setq matched-end-pos (point)
                                    searching nil))))))
                    ;; No more matches in the buffer.
                    (setq searching nil)))
                ;; after the loop, run the backward search and assign all metadata variables
                (when matched-end-pos
                  (setq top-block-name2 (seed7-top-block-name))
                  (when (seed7-re-search-backward seed7-procfunc-regexp)
                    (setq item-type (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                          item-name (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                          tail-type (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group))
                          top-block-name top-block-name2
                          long-body-final-pos matched-end-pos
                          final-pos matched-end-pos
                          found-candidate t)))))
            ;; -- Search for next short function. ----------------------------
            (save-excursion
             (let (short-func-decl-pos)
               (when
                   (and
                    (setq found-pos (seed7-re-search-forward seed7-short-func-end-regexp)
                          top-block-name2 (seed7-top-block-name))
                    (when (seed7-re-search-backward seed7-procfunc-regexp)
                      (setq short-func-decl-pos (point)
                            item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                            item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                            tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                      t)
                    ;; Reject this candidate only when it is nested inside the
                    ;; outer long-body callable already captured
                    ;; When no outer end has been found yet (long-body-final-pos is nil),
                    ;; the short function is a top-level sibling/successor and must be accepted.
                    (not (and long-body-final-pos
                              short-func-decl-pos
                              (< original-pos short-func-decl-pos long-body-final-pos)))
                    (seed7-re-search-forward seed7-short-func-end-regexp)
                    (eq (point) found-pos)
                    (or (not final-pos)
                        (< found-pos final-pos)))
                 (setq final-pos found-pos
                       found-candidate t
                       item-name item-name2
                       item-type item-type2
                       tail-type tail-type2
                       top-block-name top-block-name2))))
            ;; -- Search for next forward declaration. -----------------------
            (save-excursion
              (let (fwd-decl-pos)
                (when
                    (and
                     (setq found-pos
                           (seed7-re-search-forward seed7-forward-declaration-end-regexp)
                           top-block-name2 (seed7-top-block-name))
                     (when (seed7-re-search-backward seed7-procfunc-regexp)
                       (setq fwd-decl-pos (point)
                             item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                             item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                             tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                       t)
                     ;; Reject if nested inside the outer long-body callable already found
                     (not (and long-body-final-pos
                               fwd-decl-pos
                               (< original-pos fwd-decl-pos long-body-final-pos)))
                     (seed7-re-search-forward seed7-forward-declaration-end-regexp)
                     (eq (point) found-pos)
                     (or (not final-pos)
                         (< found-pos final-pos)))
                  (setq final-pos found-pos
                        found-candidate t
                        item-name item-name2
                        item-type item-type2
                        tail-type tail-type2
                        top-block-name top-block-name2))))
            ;; -- Search for next function implementation declaration. -------
            (save-excursion
              (let (action-decl-pos)
                (when
                    (and
                     (setq found-pos
                           (seed7-re-search-forward seed7-procfunc-forward-or-action-declaration-re)
                           top-block-name2 (seed7-top-block-name))
                     (when (seed7-re-search-backward seed7-procfunc-regexp)
                       (setq action-decl-pos (point)
                             item-type2 (substring-no-properties (match-string seed7-procfunc-regexp-item-type-group))
                             item-name2 (substring-no-properties (match-string seed7-procfunc-regexp-item-name-group))
                             tail-type2 (substring-no-properties (match-string seed7-procfunc-regexp-tail-type-group)))
                       t)
                     ;; Reject if nested inside the outer long-body callable already found
                     (not (and long-body-final-pos
                               action-decl-pos
                               (< original-pos action-decl-pos long-body-final-pos)))
                     (seed7-re-search-forward seed7-procfunc-forward-or-action-declaration-re)
                     (eq (point) found-pos)
                     (or (not final-pos)
                         (< found-pos final-pos)))
                  (setq final-pos found-pos
                        found-candidate t
                        item-name item-name2
                        item-type item-type2
                        tail-type tail-type2
                        top-block-name top-block-name2))))
            ;; --
            (if found-candidate
                ;; move to the end of first function to allow next search in loop
                (goto-char final-pos)
              ;; Nothing found in this loop.  Quit searching right away
              (user-error (seed7--no-defun-found-msg-for n 'forward)))))
        ;; --
        (when (and top-block-name
                   (not (string= top-block-name item-name)))
          (setq item-name (format "%s %s" top-block-name item-name)))
        (seed7--move-and-mark
         original-pos
         final-pos
         dont-push-mark
         (unless silent
           (seed7--show-info 'at-end-of item-name item-type tail-type)))))))

;;*** Seed7 Procedure/Function Navigation Mode Functions
;;    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; The following functions are used as mode navigation callbacks that conform
;; to Emacs lisp.el conventions to perform various operations such as
;; `beginning-of-defun', `end-of-defun' and `mark-defun' for marking a
;; complete function. These functions must therefore be silent and not push
;; mark. They use their interactive and verbose counterparts specifying the
;; extra parameters, silence errors, move to the top of buffer or end of
;; buffer on failure and return t on success, nil on failure.

(defun seed7-nav-beginning-of-defun (&optional n)
  "Move backward to the beginning of defun.
Move once (nil is equivalent to 1), unless N specifies a different count.
With negative N, move point forward to the beginning of the Nth defun,
N of 1 meaning the current one (or the one after the current comment
or whitespace.
- Operate silently; do not issue an error when nothing is found.
- Return t if point moved to the beginning of function, nil if nothing found.
- This is meant to be used as the `beginning-of-defun-function' of lisp.el."
  ;; (message ":seed7-nav-beginning-of-defun: n=%S, called from point:%d, line %d" n
  ;;            (point)
  ;;            (seed7-current-line-number))
  (unless n (setq n 1))
  (condition-case nil
      (progn
        (if (> n 0)
            (seed7-beg-of-defun n :silent)
          (seed7-beg-of-next-defun (abs n) :silent))
        t)
    (error nil)))

(defun seed7-nav-end-of-defun (&optional n)
  "Simple end of defun to use as `end-of-defun-function'.

Move once, unless N specifies a different count.
Operate silently; do not issue an error when nothing is found.
Return t if point moved to the beginning of function, nil if nothing found."
  (unless n (setq n 1))
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
  "Return the N-th word on the current line, nil if none.
For the first word N must be 1.
Negative N starts counting from the end of the line: -1 is the last word."
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (if (< n 0)
          ;; Count from end of current line, independent of original point.
          (progn
            (goto-char line-end)
            (dotimes (_ (- n))
              (backward-word)))
        ;; Count from indentation of current line, independent of original point.
        (goto-char line-start)
        (skip-chars-forward " \t" line-end)
        (dotimes (_ (1- n))
          (forward-word)
          (skip-chars-forward " \t" line-end)))
      ;; Return the identified word or nil if found on a different line.
      (when (and (>= (point) line-start)
                 (< (point) line-end))
        (thing-at-point 'word :no-properties)))))

;; -- Compilation Behavior Control for forward-sexp/backward-sexp protection
(eval-and-compile
  (defconst seed7--debug-sexp-scan nil
    "Forward/Backward Sexp debug control.  For DEVELOPMENT ONLY!

When non-nil at byte-compile time, `seed7--with-forward-sexp' and
`seed7--with-backward-sexp' expand to bare calls without any
`condition-case' wrapper, so scan-errors surface immediately.
To use: set this to t before byte-compiling seed7-mode.el.
Has no effect at runtime on already-compiled code.")

  (defconst seed7--log-sexp-scan-errors nil
    "Forward/Backward Sexp error log control.  For DEVELOPMENT ONLY!

When non-nil at byte-compile time (and `seed7--debug-sexp-scan' is nil),
`seed7--with-forward-sexp' and `seed7--with-backward-sexp' expand with a
`message' call inside the scan-error handler.
To use: set this to t before byte-compiling seed7-mode.el.
Has no effect at runtime on already-compiled code."))

;; -- Protective macros
(defmacro seed7--with-forward-sexp (&rest body)
  "Call `forward-sexp' then evaluate BODY, returning the last value.

BODY may contain an `:on-error' keyword to separate success forms from
error forms:

  (seed7--with-forward-sexp
    SUCCESS-FORMS...
    :on-error
    ERROR-FORMS...)

When `:on-error' is absent, the error handler returns nil.

The expansion is chosen once, at byte-compile time, based on
`seed7--debug-sexp-scan' and `seed7--log-sexp-scan-errors':
- Both nil (production): `scan-error' is caught silently; SUCCESS-FORMS
  are skipped and ERROR-FORMS evaluated when the delimiter is unmatched.
- `seed7--log-sexp-scan-errors' non-nil: same as production but also emits
  a `message' for each caught scan-error before ERROR-FORMS.
- `seed7--debug-sexp-scan' non-nil: bare `forward-sexp' with no handler;
  scan-errors propagate normally; ERROR-FORMS are not emitted.
Changing either constant at runtime has no effect on compiled code."
  (declare (indent 0) (debug t))
  ;; Split BODY on :on-error at macro-expansion time (pure Elisp, no cl-lib).
  (let* ((on-error-tail (memq :on-error body))
         (success-forms (if on-error-tail
                            (butlast body (length on-error-tail))
                          body))
         (error-forms (if on-error-tail
                          (or (cdr on-error-tail) '(nil))
                        '(nil))))
    (cond
     ;; -- Debug build: no protection, scan-errors propagate;
     ;;    :on-error forms are not emitted (error surfaces to caller).
     (seed7--debug-sexp-scan
      `(progn (forward-sexp) ,@success-forms))
     ;; -- Logging build: catch scan-error, report it, then run error forms.
     ;;    On error, point may move; remember it.
     (seed7--log-sexp-scan-errors
      `(let ((scan-start-pos (point)))
         (condition-case err
             (progn (forward-sexp) ,@success-forms)
           (scan-error
            (message "seed7-mode: forward-sexp scan-error at pos %d: %s"
                     scan-start-pos
                     (error-message-string err))
            ,@error-forms))))
     ;; -- Production build: catch scan-error silently, run error forms.
     (t
      `(condition-case nil
           (progn (forward-sexp) ,@success-forms)
         (scan-error ,@error-forms))))))

(defmacro seed7--with-backward-sexp (&rest body)
  "Call `backward-sexp' then evaluate BODY, returning the last value.

Expansion rules are identical to `seed7--with-forward-sexp': the
guard/log/debug mode is selected once at byte-compile time, and BODY
may contain an `:on-error' keyword to separate success forms from
error forms."
  (declare (indent 0) (debug t))
  (let* ((on-error-tail (memq :on-error body))
         (success-forms (if on-error-tail
                            (butlast body (length on-error-tail))
                          body))
         (error-forms (if on-error-tail
                          (or (cdr on-error-tail) '(nil))
                        '(nil))))
    (cond
     (seed7--debug-sexp-scan
      `(progn (backward-sexp) ,@success-forms))
     (seed7--log-sexp-scan-errors
      `(let ((scan-start-pos (point)))
         (condition-case err
             (progn (backward-sexp) ,@success-forms)
           (scan-error
            (message "seed7-mode: backward-sexp scan-error at pos %d: %s"
                     scan-start-pos
                     (error-message-string err))
            ,@error-forms))))
     (t
      `(condition-case nil
           (progn (backward-sexp) ,@success-forms)
         (scan-error ,@error-forms))))))

;; --

(defun seed7--type-regexp (keyword)
  "Return a regexp to search for the start/end of KEYWORD type block.

The regexp has 2 capture groups:
- group1 for the starting expression,
- group2 for the end part."
  (declare (side-effect-free t))
  ;; Note hard tab is supported after keyword and not between 2 adjacent keywords as inside 'end block'.
  (format "^\\(?:[[:space:]]*?\\(const +?type:.+?[[:space:]]%s\\)\\|[[:space:]]*?\\(end %s;\\)\\)"
          keyword keyword))

(defun seed7--end-regexp-for (word1 word2 last-word)
  "Return regexps for end and start of block for specified arguments.
Used when searching forward.
Where WORD1 is the first word, WORD2 the second word and LAST-WORD the last.
The regexp has 2 or 3 groups:
- group 1: block start text,
- group 2: the block end text,
- group 3: optional, a peer level clause."
  ;; Note hard tab is supported *after* keyword and *not* between 2 adjacent
  ;; keywords as inside "end block".
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
           ((string= word1 "repeat")    "^[[:blank:]]*?\\(?:\\(repeat\\>\\)\\|\\(until\\>\\)[[:blank:]]\\)" )
           ((string= word1 "block")     "^[[:blank:]]*?\\(?:\\(block\\>\\)\\|\\(end block\\)\\)" )
           ((string= word1 "global")    "^[[:blank:]]*?\\(?:\\(global\\>\\)\\|\\(end global;\\)\\)" )
           ((string= word1 "when")      "^[[:blank:]]*?\\(?:\\(case[[:blank:]]\\)\\|\\(end case;?\\)\\|\\(when[[:blank:]]\\|otherwise\\)\\)")
           ((string= word1 "otherwise") "^[[:blank:]]*?\\(?:\\(case[[:blank:]]\\)\\|\\(end case;?\\)\\)")
           ((string= word1 "elsif")     "^[[:blank:]]*?\\(?:\\(if[[:blank:]]\\)\\|\\(end if;?\\)\\|\\((elsif[[:blank:]]\\|else\\)\\)")
           ((string= word1 "else")      "^[[:blank:]]*?\\(?:\\(if[[:blank:]]\\)\\|\\(end if;?\\)\\)")
           ((member word1 '("local" "begin")) seed7--inner-callables-triplets-re)
           ((string= word1 "const")
            (cond
             ((member word2 '("varfunc" "func" "proc"))
              nil)    ; use `seed7-end-of-defun' for functions and procedures.
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
            (format "^[[:blank:]]*?\\(?:\\(%s[[:blank:]]\\)\\|\\(end %s;?\\)\\)" word1 word1))
           (t nil)))
    (cons regexp start-pos)))

(defun seed7-to-block-forward (&optional dont-push-mark)
  "Move forward from the block beginning to its end.

Handle comments, array/set/type definition blocks, procedure/function
declarations, and generic Seed7 statement blocks.

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
            (seed7--with-forward-sexp
              (seed7-re-search-forward ";")
              (setq found-position (point))))
           ;; handle set
           ((and (string= word2 "set")
                 (member word1 '("const" "var")))
            (seed7-re-search-forward "{")
            (backward-char)
            (seed7--with-forward-sexp
              (seed7-re-search-forward ";")
              (setq found-position (point))))
           ;; handle type
           ((and (string= word2 "type")
                 (member word1 '("const" "var"))
                 (not (member (seed7--current-line-nth-word -1) '("struct" "enum"))))
            (seed7-re-search-forward ";")
            (setq found-position (point)))
           ;; handle everything else
           (t
            (let* ((regexp.start-pos (seed7--end-regexp-for
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
                               ((match-beginning 1)
                                (setq nesting (1+ nesting)))
                               ;; found block end: exiting one nesting level.
                               ((match-beginning 2)
                                (if (eq nesting 0)
                                    (progn
                                      (setq searching nil)
                                      (setq found-position (point)))
                                  (setq nesting (1- nesting))))
                               ;; Found a peer level clause: stop if at
                               ;; nesting level 0
                               ((match-beginning 3)
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


(defun seed7--start-regexp-for (word1 word2)
  "Return a regexp to search the starting string block specified by the arguments.
Used when searching backward.

- WORD1 : the first word of the end statement.
- WORD2 : the second word of the end statement.

Return a regexp that searches for the start or end of the block.
The regexp has 2 or 3 groups:
- group 1: block start text,
- group 2: the block end text,
- group 3: optional, a peer level clause."
  ;; Note hard tab is supported *after* keyword and *not* between 2 adjacent
  ;; keywords as inside "end block".
  (let ((regexp nil)
        (same-line nil))
    (setq regexp
          (cond
           ;; Deal with special cases first
           ;;  The following regexp allow no white-space before the keywords even
           ;;  though the space is always required in Seed7 code.  This is done this
           ;;  way to allow a match when trying to verify a matching block for the
           ;;  purpose of calculating the indentation required.
           ((not word1) "^\\(?:[[:space:]]*?\\(const +?array[[:space:]]+?.+?:\\)\\|[[:space:]]+?\\();\\)\\)")
           ((string= word1 "until")               "^[[:blank:]]*?\\(?:\\(repeat\\>\\)\\|\\(until\\>\\)[[:blank:]]\\)")
           ((member  word1 '("when" "otherwise")) "^[[:blank:]]*?\\(?:\\(case[[:blank:]]\\)\\|\\(end case;?\\)\\|\\(when[[:blank:]]\\)\\)")
           ((member  word1 '("elsif" "else"))     "^[[:blank:]]*?\\(?:\\(if[[:blank:]]\\)\\|\\(end if;?\\)\\|\\(elsif[[:blank:]]\\)\\)")
           ((string= word1 "return")              "\\(^[[:blank:]]*?const func\\>\\)\\|\\(^;INVALID-MAKE-IT-NEVER-MATCH;\\)")
           ((string= word1 "end")
            (cond
             ((string= word2 "func") seed7--inner-callables-triplets-re)
             ((string= word2 "global")            "^[[:blank:]]*?\\(?:\\(global\\>\\)\\|\\(end global;\\)\\)")
             ((string= word2 "block")             "^[[:space:]]*?\\(block\\>\\(?:[[:space:]]*?#.*?\\)?$\\)\\|\\(end block;\\)")
             ((member word2 '("case"
                              "for"
                              "if"
                              "while"))
              (format"^[[:space:]]*?\\(?:\\(%s[[:blank:]]\\)\\|\\(end %s;?\\)\\)" word2 word2))
             ((member word2 '("enum"
                              "struct"))
              (seed7--type-regexp word2))
             (t nil)))
           ((and (string= word1 "const")
                 (string= word2 "type"))
            (setq same-line t)
            ;; return a regexp where the group 2 never matches
            "\\(\\(?:^[[:space:]]*?const +?type:.+?[[:space:]]\\)\\)\\|\\(^;INVALID-MAKE-IT-NEVER-MATCH;\\)")
           (t nil)))
    (cons regexp same-line)))

(defun seed7-to-block-backward (&optional at-beginning-of-line dont-push-mark)
  "Move backward from block end or array/set definition delimiter to its beginning.

Move point to the beginning of the block keyword, definition header, or comment.

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
               (regexp.same-line (seed7--start-regexp-for first-word
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
                       ((match-beginning 1)
                        (if (eq nesting 0)
                            (progn
                              (setq searching nil)
                              (setq found-position (point)))
                          (setq nesting (1- nesting))))
                       ;; Found a block end text: entering a deeper nesting
                       ;; level.
                       ((match-beginning 2)
                        (setq nesting (1+ nesting)))
                       ;; Found peer level clause: stop if at nesting level 0
                       ((match-beginning 3)
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
            ;; Check for end of parens pair, used for array and set, but also
            ;; possible at the end of a return statement of a short function.
            (let ((pos nil))
              (cond
               ((seed7--set (seed7-line-code-ends-with 0 ");") pos)
                (goto-char (1+ pos))
                (seed7--with-backward-sexp
                  (seed7-to-indent)
                  ;; in case we were at the return statement of a short function
                  ;; check if we land before a const keyword.  If not, we're not
                  ;; at the beginning of the block and most probably inside a
                  ;; function return statement so use `seed7-beg-of-defun' to
                  ;; find it.
                  (unless (looking-at-p seed7--array-definition-start-regexp)
                    (seed7-beg-of-defun nil dont-push-mark dont-push-mark))
                  (setq found-position (point))))
               ;;
               ;; same logic for {} hash blocks
               ((seed7--set (seed7-line-code-ends-with 0 "};") pos)
                (goto-char (1+ pos))
                (seed7--with-backward-sexp
                  (seed7-to-indent)
                  (unless (looking-at-p seed7--set-definition-start-regexp)
                    (seed7-beg-of-defun nil dont-push-mark dont-push-mark))
                  (setq found-position (point))))
               ;;
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

(defvar-local seed7--sexp-dispatch-active nil
  "Non-nil while `seed7--forward-sexp-function' is dispatching to a Seed7 helper.
This prevents re-entry when those helpers internally invoke `backward-sexp' or
`forward-sexp' (e.g. via `seed7--with-backward-sexp') which would otherwise
cause unbounded recursion back into the dispatcher.")

(defun seed7--at-array-definition-end-line-p ()
  "Return non-nil if current line ends a Seed7 array definition block."
  (save-excursion
    (let ((pos (seed7-line-code-ends-with 0 ");")))
      (when (and pos
                 (not (eq (char-before) ?\))))
        (goto-char (1+ pos))
        (let ((seed7--sexp-dispatch-active t))
          (when (ignore-errors
                  (backward-sexp)
                  t)
            (seed7-to-indent)
            (looking-at-p seed7--array-definition-start-regexp)))))))

(defun seed7--at-set-definition-end-line-p ()
  "Return non-nil if current line ends a Seed7 set definition block."
  (save-excursion
    (let ((pos (seed7-line-code-ends-with 0 "};")))
      (when (and pos
                 (not (eq (char-before) ?\})))
        (goto-char (1+ pos))
        (let ((seed7--sexp-dispatch-active t))
          (when (ignore-errors
                  (backward-sexp)
                  t)
            (seed7-to-indent)
            (looking-at-p seed7--set-definition-start-regexp)))))))

(defun seed7--at-multiline-short-func-end-p ()
  "Return non-nil if point at end a multi-line short-function return.

That is: return non-nil when point is after the `;' ending a multi-line
short-function return.

Handles the case where the `return' keyword is on a line preceding the one
that holds the terminating `;'.  Example:
  return [indexRange.minIdx] (tupleType conv
      someExpression);"
  (when (eq (char-before) ?\;)
    (save-excursion
      (let ((found nil)
            (stop nil))
        (while (and (not found) (not stop))
          (when (/= (forward-line -1) 0)
            (setq stop t))
          (unless stop
            (cond
             ;; Found the line that begins the return statement.
             ((looking-at-p "[[:blank:]]+return[[:blank:]]")
              (setq found t))
             ;; Another statement ended on this line: stop scanning backward.
             ((seed7-line-code-ends-with 0 ";")
              (setq stop t)))))
        found))))

(defun seed7--forward-sexp-function (&optional arg)
  "Seed7-aware `forward-sexp-function'.
Handles moving forward, or backward with negative ARG, from start/end of:
- nested `(* ... *)' block comments,
- single and consecutive `#' line-end comments,
- procedure/function declaration lines and their end lines,
- Seed7 block start/end lines,
- const/var array and const/var set definition block start/end lines,
- (), [] and {} delimiter pairs.

Falls through to `scan-sexps' for all other sexp forms to provide a uniform
navigation command."
  (if seed7--sexp-dispatch-active
      ;; Re-entry: a Seed7 helper is calling (backward/forward)-sexp internally.
      ;; Delegate straight to the built-in scanner to avoid infinite recursion.
      (goto-char (or (scan-sexps (point) (if (> (or arg 1) 0) 1 -1))
                     (buffer-end (or arg 1))))
    (let* ((seed7--sexp-dispatch-active t)
           (arg (or arg 1)))
      (dotimes (_ (abs arg))
        (cond
         ((> arg 0)
          ;; -- Forward ---------------------
          (cond
           ;; Forward: point is at (* opener
           ((looking-at-p "(\\*")
            (seed7--forward-block-comment 1))
           ;;
           ;; Forward: point is at # line-comment start
           ((seed7--at-line-comment-start-p)
            (seed7--forward-line-comments 1))
           ;;
           ;; Forward: current line is the start of a proc/func declaration.
           ;; Must be tested before the generic block case below because
           ;; `seed7-block-start-regexp' also matches "const proc:" and "const func".
           ((and (not (looking-at-p seed7--open-paren-regexp))
                 (save-excursion
                   (seed7-to-indent)
                   (looking-at-p seed7-procfunc-beg-of-decl-nc-re)))
            (seed7-end-of-defun 1 :silent :dont-push-mark))
           ;;
           ;; Forward: current line is the start of any other Seed7 block
           ;; (if, while, for, repeat, case, block, global, local, begin, …).
           ((and (not (looking-at-p seed7--open-paren-regexp))
                 (save-excursion
                   (seed7-to-indent)
                   (or
                    (looking-at-p seed7-block-start-regexp)
                    ;; If current line is the start of a const/var array or set
                    ;; definition block.  These are not in
                    ;; `seed7-block-start-regexp' so they need their own clause
                    ;; `seed7-to-block-forward' handles both.
                    (looking-at-p seed7--array-definition-start-regexp)
                    (looking-at-p seed7--set-definition-start-regexp))))
            (seed7-to-block-forward :dont-push-mark))
           ;;
           ;; Forward default: delegate to built-in scanner
           (t
            (goto-char (or (scan-sexps (point) 1)
                           (buffer-end arg))))))
         ;; -- Backward ----------------------
         ((< arg 0)
          (cond
           ;; Backward: point is just after *) closer
           ((and (>= (- (point) 2) (point-min))
                 (string= (buffer-substring (- (point) 2) (point)) "*)"))
            (seed7--forward-block-comment -1))
           ;;
           ;; Backward: current line has a # line comment
           ((let ((hash-pos (seed7--line-comment-hash)))
              (and hash-pos
                   (>= (point) hash-pos)))
            (seed7--forward-line-comments -1))
           ;;
           ;; Backward: current line is "end func;" (end of proc/func).
           ;; Must be tested before the generic block-end case because
           ;; `seed7-block-end-regexp' also matches "end func;".
           ((and (not (memq (char-before) seed7--close-paren-chars))
                 (save-excursion
                   (seed7-to-indent)
                   (looking-at-p seed7-procfunc-end-regexp)))
            (seed7-beg-of-defun 1 :silent :dont-push-mark))
           ;;
           ;; Backward: current line is any other block end
           ;; (end if;, end while;, end for;, end case;, end block, until …).
           ((and (not (memq (char-before) seed7--close-paren-chars))
                 (save-excursion
                   (seed7-to-indent)
                   (looking-at-p seed7-block-end-regexp)))
            (seed7-to-block-backward nil :dont-push-mark))
           ;;
           ;; Backward: current line ends with ); — may be end of a
           ;; const/var array definition block.
           ;; `seed7-to-block-backward' already disambiguates between an array
           ;; definition and a function return statement via the
           ;; `seed7--array-definition-start-regexp' at the destination indent
           ;; position.
           ((seed7--at-array-definition-end-line-p)
            (seed7-to-block-backward nil :dont-push-mark))
           ;;
           ;; Backward: current line ends with }; — may be end of a
           ;; const/var set definition block.
           ((seed7--at-set-definition-end-line-p)
            (seed7-to-block-backward nil :dont-push-mark))
           ;;
           ;; Backward: current line is the return statement of a short
           ;; function (matches `seed7-short-func-end-regexp'), or point is
           ;; after the `;' ending a multi-line short-function return where the
           ;; `return' keyword sits on a preceding line.
           ;; Jump to the beginning of the enclosing declaration.
           ((and (not (memq (char-before) seed7--close-paren-chars))
                 (or
                  ;; Single-line return: the current line starts with "return … ;"
                  (save-excursion
                    (forward-line 0)
                    (looking-at-p seed7-short-func-end-regexp))
                  ;; Multi-line return: "return" is on a preceding line and the
                  ;; terminating ";" is on the current line.
                  (seed7--at-multiline-short-func-end-p)))
            (seed7-beg-of-defun 1 :silent :dont-push-mark))
           ;;
           ;; Backward default: delegate to built-in scanner
           (t
            (goto-char (or (scan-sexps (point) -1)
                           (buffer-end arg)))))))))))

;; ---------------------------------------------------------------------------
;;* Nested function/procedure lineage naming
;;  ========================================

(defun seed7--pos-after-decl-header ()
  "Return end-of-line of the `is func'/`is' terminator if point is inside or
at the start of a multi-line callable declaration header, else return nil.

When `re-search-backward' is used on a multi-line G1 pattern (see
`seed7---inner-callables-1'), the match ends on the `... is func' or `... is'
line.  If point lies on or before that line (but before its end), the match-end
is AFTER point and `re-search-backward' cannot find the declaration.

This helper detects that situation — including the case where point is on the
very first line of the header (`const proc/func ...') — and returns a position
past the terminator line so the caller can advance point before searching.

Returns nil when no adjustment is needed:
  - point is on a body/end line (`begin', `local', `end ...'), i.e. already
    inside or past the callable body; or
  - the current line itself ends with `is func' or `is' (single-line
    terminator), which `re-search-backward' can already find."
  (save-excursion
    (forward-line 0)                    ; move to BOL
    (cond
     ;; Already inside or past the callable body: no adjustment needed.
     ((looking-at-p
       "^[[:blank:]]*\\(?:begin\\b\\|local\\b\\|end \\)")
      nil)
     ;; The current line IS the `is func'/`is' terminator.  Returning
     ;; line-end-position ensures the backward scan can always find the
     ;; declaration, regardless of where on the line point sits.
     ;; (Only when point is exactly at line-end-position is re-search-backward
     ;; already able to find the match; for any earlier position it cannot.)
     ((looking-at
       "^[[:blank:]]*[^;]*?is\\(?: +func\\)?[[:blank:]]*$")
      (line-end-position))
     ;; Otherwise — whether this is the opening `const proc/func' line of a
     ;; multi-line header or a continuation line — scan forward for the
     ;; terminator.
     (t
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (forward-line 1)
          (cond
           ;; Found the `is func' / `is' terminator.
           ((looking-at
             "^[[:blank:]]*[^;]*?is\\(?: +func\\)?[[:blank:]]*$")
            (setq found (line-end-position)))
           ;; Hit a body start, a new declaration, or a complete statement
           ;; (ends with `;'): no terminator in this direction.
           ((or (looking-at-p
                 (concat "^[[:blank:]]*\\(?:begin\\b\\|local\\b\\|end \\|"
                         "const[[:blank:]]+\\(?:func\\|proc\\)\\)"))
                (looking-at-p "^.*?;[[:blank:]]*$"))
            (setq found 'stop))))
        (when (numberp found) found))))))

(defun seed7--callable-ancestors ()
  "Return a list of callable names enclosing the current position.
The list is ordered from outermost to innermost; each element is a string.
For example, if point is inside `grand_child' which is defined inside `child',
itself defined inside `parent', the returned list is
  (\"parent\" \"child\" \"grand_child\")
Returns nil when point is not inside any callable.

Algorithm
---------
The outer while loop iterates once per nesting level.  Each iteration
performs a nesting-aware backward scan using
`seed7--inner-callables-triplets-re':

  Group 1 — proc/func declaration start:
    nesting = 0, long-body (`is func'): this is the enclosing callable;
      record and stop.
    nesting = 0, short-body (`is' only): scan forward for the matching
      `return …;'.  If its end is AFTER the iteration's scan-start, point
      is inside this short function — record and stop.  Otherwise point is
      past the function (it is a preceding sibling) — skip and continue.
    nesting > 0, long-body: a previously entered nested long-body scope has
      been fully traversed; decrement depth and continue.
    nesting > 0, short-body: leave depth unchanged and continue.

  Group 2 — `end func;' / `end proc;' or `return …;':
    Only text starting with `end ' increments the nesting counter.
    Short-function return lines do not open a long-body scope.

  Group 3 — forward / action declaration: not a nesting boundary; ignored."
  (save-excursion
    (let ((ancestors  nil)
          (keep-going t)
          (first-iter t))        ; track whether this is the first iteration
      (while keep-going
        (let ((found-pos  nil)
              (found-name nil))
          ;; ------------------------------------------------------------------
          ;; PRE-ADJUSTMENT (first iteration only)
          ;; If point falls inside a multi-line callable declaration header
          ;; (e.g. on the first line `const proc: for … do', or on a
          ;; continuation line), re-search-backward cannot find the declaration
          ;; because its multi-line match ends AFTER point.  Advance point past
          ;; the header terminator so the backward scan can find the match.
          ;;
          ;; This adjustment is applied ONLY on the first iteration.  On
          ;; subsequent iterations point has been placed one character before
          ;; the previously found declaration (see PROCESS RESULT below), so
          ;; no adjustment is needed — and applying it would incorrectly
          ;; re-advance past the same declaration, causing the loop to find
          ;; the same callable repeatedly.
          ;; ------------------------------------------------------------------
          (when first-iter
            (let ((adj (seed7--pos-after-decl-header)))
              (when adj (goto-char adj)))
            (setq first-iter nil))
          ;; ------------------------------------------------------------------
          ;; NESTING-AWARE BACKWARD SCAN
          ;; ------------------------------------------------------------------
          (let ((scan-start (point))   ; remember where this iteration begins
                (nesting    0)
                (searching  t))
             (while (and searching (not (bobp)))
              (if (seed7-re-search-backward seed7--inner-callables-triplets-re)
                  (cond
                   ;; ---- Group 1: proc/func declaration start ---------------
                   ((match-beginning 1)
                    (let* ((match-str    (match-string-no-properties 1))
                           (is-long-body
                            (string-match-p
                             "\\bis[[:blank:]]+\\(?:func\\|proc\\)[[:blank:]]*\\'"
                             match-str)))
                      (if (eq nesting 0)
                          (if is-long-body
                              ;; Long-body at nesting 0: definitely enclosing.
                              (progn
                                (setq searching nil)
                                (when (looking-at seed7-procfunc-regexp)
                                  (setq found-pos  (point)
                                        found-name
                                        (substring-no-properties
                                         (or (match-string
                                              seed7-procfunc-regexp-item-name-group)
                                             "?")))))
                            ;; Short-body at nesting 0: determine whether
                            ;; scan-start is INSIDE or PAST this function.
                            (let* ((decl-pos (point))
                                   (ret-end
                                    (save-excursion
                                      (goto-char decl-pos)
                                      (seed7-re-search-forward
                                       seed7--callable-return-re))))
                              (if (and ret-end (> ret-end scan-start))
                                  ;; Inside the short function: record it.
                                  (progn
                                    (setq searching nil)
                                    (when (looking-at seed7-procfunc-regexp)
                                      (setq found-pos  decl-pos
                                            found-name
                                            (substring-no-properties
                                             (or (match-string
                                                  seed7-procfunc-regexp-item-name-group)
                                                 "?")))))
                                ;; Past the short function: it is a sibling —
                                ;; continue scanning backward.
                                )))
                        ;; nesting > 0: only long-body declarations have a
                        ;; matching `end func;'.  Short-body declarations do
                        ;; not change depth.
                        (when is-long-body
                          (setq nesting (1- nesting))))))
                   ;; ---- Group 2: callable end marker ----------------------
                   ((match-beginning 2)
                    (let ((matched-text (match-string 2)))
                      (when (and matched-text
                                 (string-match-p "\\`end " matched-text))
                        (setq nesting (1+ nesting)))))
                   ;; ---- Group 3: forward/action declaration ---------------
                   (t nil))
                ;; No match found: stop the inner scan.
                (setq searching nil))))
          ;; ------------------------------------------------------------------
          ;; PROCESS RESULT
          ;; ------------------------------------------------------------------
          (if found-pos
              (progn
                ;; Prepend so the final list is outermost-first.
                (setq ancestors (cons found-name ancestors))
                ;; Move to ONE CHARACTER BEFORE the found declaration's BOL.
                ;; This places point at EOL of the preceding line, ensuring
                ;; the next iteration's backward scan searches for what
                ;; ENCLOSES the just-found callable — not the callable itself.
                ;;
                ;; Using `found-pos' directly (BOL of the declaration) would
                ;; cause `seed7--pos-after-decl-header' to forward-scan past
                ;; the same declaration again and re-find the same callable.
                (goto-char (max (point-min) (1- found-pos))))
            (setq keep-going nil))))
      ancestors)))

(defun seed7--qualified-name-at-pos ()
  "Return the fully qualified callable name enclosing point.
Nesting levels are joined with \":\" as the separator, producing names such as
  \"parent:child:grand_child\"
for a procedure `grand_child' defined inside `child', itself inside `parent'.
Returns nil when point is not inside any callable."
  (let ((ancestors (seed7--callable-ancestors)))
    (when ancestors
      (string-join ancestors ":"))))

;; ---------------------------------------------------------------------------
;;* Seed7 iMenu Support
;;  ===================

(defvar-local seed7--menu-list-functions-and-procedures-together
    seed7-menu-list-functions-and-procedures-together
  "When on, list functions and procedures together, otherwise separately.

Dynamic value that affects the way the callables are displayed in imenu
commands, in the top menu and inside the Speedbar.
Initialized to `seed7-menu-list-functions-and-procedures-together' user-option
value which can then be dynamically modified by the
`seed7-toggle-menu-callable-list' command.")

(defun seed7--imenu-index-for-regexp (regexp group)
  "Scan the buffer for REGEXP and return an (name . pos) alist.
GROUP identifies the sub-expression whose text becomes the name.
Results are in document order."
  (let ((items '()))
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward regexp)
        (push (cons (match-string-no-properties group)
                    (match-beginning 0))
              items)))
    (nreverse items)))

(defun seed7--imenu-create-index ()
  "Build an imenu index for the current Seed7 buffer using qualified names.

Non-callable entries (Enum, Interface, Struct) are listed under their own
category sub-menu using their simple names.

Callable entries (proc/func) use fully-qualified names so nested callables
appear as \"parent:child:grand_child\".  The nesting stack is maintained by
a single O(n) forward pass: only long-body callables (tail-type \"func\")
push a frame; `end func;' pops one frame.  Short-body callables
\(tail-type \"return\"), forward declarations, and action declarations are
added to the index as leaves without pushing to the stack.

The flag `seed7--menu-list-functions-and-procedures-together' controls
whether procedures and functions share a single \"Callable\" sub-menu or are
split into separate \"Procedure\" / \"Function\" sub-menus."
  (let ((procs      '())
        (funcs      '())
        ;; Nesting stack: each element is (name . decl-pos).
        ;; Only long-body callables (tail-type "func") are pushed here;
        ;; short-body, forward, and action declarations are NOT pushed.
        (stack      '())
        (enums      (seed7--imenu-index-for-regexp seed7-enum-regexp-4imenu      1))
        (interfaces (seed7--imenu-index-for-regexp seed7-interface-regexp-4imenu 1))
        (structs    (seed7--imenu-index-for-regexp seed7-struct-regexp-4imenu    1)))
    (save-excursion
      (goto-char (point-min))
      ;; Combine both patterns in one search.  When `end func;' matches,
      ;; the callable name group (G3) is nil — use that to dispatch.
      (while (seed7-re-search-forward
              (concat seed7-procfunc-regexp "\\|" seed7-procfunc-end-regexp))
        (if (not (match-string seed7-procfunc-regexp-item-name-group))
            ;; `end func;' matched — pop one level.
            ;; Both proc and func long bodies close with `end func;'.
            (when stack (pop stack))
          ;; Callable declaration matched.
          (let* ((name      (match-string-no-properties
                             seed7-procfunc-regexp-item-name-group))
                 (item-type (match-string-no-properties
                             seed7-procfunc-regexp-item-type-group))
                 (tail-type (match-string-no-properties
                             seed7-procfunc-regexp-tail-type-group))
                 (is-proc      (string= item-type "proc"))
                 ;; Only "func" tail introduces a long body closed by `end func;'.
                 ;; "return", "forward;", "action ...", "DYNAMIC;" are all leaves.
                 (is-long-body (string= tail-type "func"))
                 (decl-pos  (match-beginning 0))
                 ;; Build qualified name from the current nesting stack.
                 (qname     (if stack
                                (concat (mapconcat #'car (reverse stack) ":") ":" name)
                              name)))
            ;; Only long-body callables open a new nesting level.
            (when is-long-body
              (push (cons name decl-pos) stack))
            (if is-proc
                (push (cons qname decl-pos) procs)
              (push (cons qname decl-pos) funcs))))))
    ;; Restore document order (push reverses the lists).
    (setq procs (nreverse procs)
          funcs (nreverse funcs))
    ;; Assemble the final index alist.
    (let ((index '()))
      (when structs    (push (cons "Struct"    structs)    index))
      (when interfaces (push (cons "Interface" interfaces) index))
      (when enums      (push (cons "Enum"      enums)      index))
      (if seed7--menu-list-functions-and-procedures-together
          ;; Merge procs + funcs into one list sorted by document position.
          (let ((callables (sort (append procs funcs)
                                 (lambda (a b) (< (cdr a) (cdr b))))))
            (when callables
              (push (cons "Callable" callables) index)))
        ;; Keep them in separate sub-menus.
        (when funcs (push (cons "Function"  funcs) index))
        (when procs (push (cons "Procedure" procs) index)))
      (nreverse index))))

(defun seed7--setup-imenu ()
  "Configure the way imenu lists its items.
Uses `seed7--imenu-create-index' as the index function so that nested
callables appear with their fully-qualified \"parent:child\" names in imenu
and in Speedbar.  The `imenu-generic-expression' mechanism is not used."
  ;; Use our custom index builder instead of the regexp-table approach.
  ;; imenu-create-index-function takes priority over imenu-generic-expression.
  (setq-local imenu-create-index-function #'seed7--imenu-create-index)
  (setq-local imenu-generic-expression    nil))

(defun seed7--refresh-imenu ()
  "Force re-display of the imenu."
  (if (fboundp 'imenu--menubar-select)
      (progn
        (imenu--menubar-select imenu--rescan-item)
        (imenu-update-menubar))
    (display-warning
     'seed7-mode
     "Cannot refresh menu; `imenu--menubar-select' is not bound."
     :warning)))

(defun seed7-toggle-menu-callable-list ()
  "Change the way callables are listed inside the current buffer menu.
Toggles listing them together or separately.
  When listed separately the functions and procedures are listed inside
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
  "Set to non-nil to list menu entries in sorted order.")

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

;; [ TODO 2025-06-21, by Pierre Rouleau: Perhaps this can use
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
  "Number of columns used for Seed7 code indentation.

Only values in the range 2 to 8, inclusive, are used.
If the value is smaller than 2, then 2 is used.
If the value is larger than 8, 8 is used."
  :group 'seed7
  :type 'integer
  :safe (lambda (value)
        (and (integerp value) (<= 2 value 8))))

(defcustom seed7-auto-indent t
  "Set to t to activate automatic indentation control.
Automatic indentation is invoked when the return or tab key is typed.
Setting this user-option to nil disables automatic indentation for
buffers using the `seed7-mode'."
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
  (declare (side-effect-free t))
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun seed7-indent-step-for-column (column)
  "Return indentation step for COLUMN number."
  (declare (pure t) (side-effect-free t))
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

(defun seed7-current-line-indent ()
  "Return indentation column number of current line, 0 if line not indented.
Does not move point."
  (save-excursion
    (seed7-to-indent)
    (current-column)))


(defun seed7-current-line-start-inside-comment-p ()
  "Return non-nil if beginning of current line is in a comment.
Checks the first indented character.  Does not move point."
  (save-excursion
    (seed7-to-indent)
    (seed7-inside-comment-p (point))))

(defun seed7-to-line-last-non-whitespace ()
  "Move point to the last non-whitespace character of the line.
Return its position if found, nil if the line is empty."
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
  "Return the position of the # starting a line-end comment at point.
Return nil if point is not inside a line-end comment, including when point
is inside a block comment."
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
CHAR is a string of one character.
If BOUND is specified it bounds the search; it is a buffer position:
the match found must not begin before that position.
Do not move point."
  (save-excursion
    (seed7-re-search-backward (regexp-quote char) bound)))

(defun seed7-forward-char-pos (char &optional bound point-after)
  "Forward search for CHAR in code, return its position or nil.
Return the position of the CHAR unless POINT-AFTER is non-nil, in which case
it returns the point after CHAR.
CHAR is a string of one character.
If BOUND is specified it bounds the search; it is a buffer position:
the match found must not begin after that position.
Does not move point."
  (save-excursion
    (when (seed7-re-search-forward (regexp-quote char) bound)
      (if point-after
          (match-end 0)
        (match-beginning 0)))))


;;*** Seed7 Indentation Base Position Detection Utilities

(defun seed7-assign-op-pos (&optional bound)
  "Return the position after the previous Seed7 assignment operator.
If BOUND is non-nil, do not search before that buffer position.
Return nil if none is found.  Do not move point."
  (save-excursion
    (when (seed7-re-search-backward seed7-predef-assignment-operator-regexp
                                    bound)
      (match-end 0))))

(defun seed7-statement-end-pos (&optional start-pos bound)
  "Return the position after the next Seed7 statement terminator.
Start searching at START-POS when non-nil, otherwise at point.
If BOUND is non-nil, do not search past that buffer position.
Return nil if no terminator is found.  Do not move point."
  (save-excursion
    (when start-pos
      (goto-char start-pos))
    (seed7-forward-char-pos ";" bound 'point-after)))


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
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A positive integer for next lines, a negative integer for previous
        lines: 1: next line, -1: previous line, etc.
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
N is: - :previous-non-empty for the previous non-empty line,
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
the search.  The match found must not begin before that position.  A
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
                                 &optional dont-skip-comment-start
                                 end-pos)
  "Return indent column when line N begins with REGEXP.
If REGEXP starts with \"^\" the comparison is done from the beginning of the
line, otherwise the string comparison is done after the indent whitespace.
Return nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
If END-POS is non-nil, it identifies the limit for the string."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (if (string=  (substring regexp 0 1) "^")
          (forward-line 0)
        (skip-chars-forward " \t"))
      (when (and (looking-at-p regexp)
                 (or (not end-pos)
                     (save-excursion
                       (re-search-forward regexp end-pos :noerror))))
        (current-column)))))

(defun seed7-line-starts-with-any (n regexps
                                     &optional dont-skip-comment-start end-pos)
  "Return indent column when line N non-white space begins with any of REGEXPS.
Return nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
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
If something found within these lines, return its indentation column,
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
N is: - :previous-non-empty for the previous non-empty line,
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
        (end-of-line)
        (when (seed7-re-search-backward "[^ \t]" line-start-pos)
          (forward-char)
          (setq line-code-end-pos (point))
          (when (seed7--set (seed7-re-search-backward regexp line-start-pos)
                          found-pos)
            (when (= (match-end 0) line-code-end-pos)
              found-pos)))))))

(defun seed7-line-ends-with (n regexp
                               &optional dont-skip-comment-start
                               start-pos)
  "Return column when line N ends with REGEXP.
Trailing spaces and tabs are ignored before checking the end of the line.
Return nil otherwise.

N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...

If START-POS is non-nil, it identifies the lower search limit."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((line-start-pos (save-excursion
                               (forward-line 0)
                               (point)))
             (limit-pos (if start-pos
                            (max line-start-pos start-pos)
                          line-start-pos))
             (line-end-pos nil))
        ;; Move to the logical end of the line, ignoring trailing whitespace.
        (end-of-line)
        (skip-chars-backward " \t" line-start-pos)
        (setq line-end-pos (point))
        ;; Search backward for REGEXP on this line and verify that the match
        ;; reaches the logical end of the line.
        (when (seed7-re-search-backward regexp limit-pos)
          (when (= (match-end 0) line-end-pos)
            (goto-char (match-beginning 0))
            (current-column)))))))

;;** Seed7 Indentation Line Type Checking Functions
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-line-isa-string (n)
  "Return the indentation column if line N is a string, nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (seed7-line-starts-with n "\""))

(defun seed7-line-is-block-end (n &optional dont-skip-comment-start)
  "Return t if line N is a block end, nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
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
                     "case "
                     ;; ... also support tabs when caller did not normalize them.
                     "const proc:\t"
                     "const func\t"
                     "const type:\t"
                     "if\t"
                     "elsif\t"
                     "while\t"
                     "for\t"
                     "case\t"))
    (seed7-to-block-forward :dont-push-mark)
    (point))
   ;;
   ((string= header "exception")
    (seed7-to-next-line-starts-with "end block")
    (point))
   ;;
   ((member header '("catch " "catch\t"))
    (seed7-to-next-line-starts-with "end block")
    (point))
   ;;
   (t (error "Unsupported block header: %s" header))))

(defun seed7--indent-offset-for (header first-text)
  "Return indentation offset (in columns) for the inside of a block.

- HEADER: string: identifies the block start header.
  This is normalized: hard tabs replaced by spaces.
- FIRST-TEXT: string: the text at the indented start of the currently
  inspected line (pre-extracted by the caller to avoid repeated buffer
  scanning)."
  (cond

   ;;-- local - begin - end func;
   ((string= header "local")
    (if (string-prefix-p "begin" first-text)
        ;; The begin keyword lines up with the local keyword
        0
      ;; Other lines are indented
      seed7-indent-width))
   ((string= header "begin")
    (if (string-prefix-p "end func;" first-text)
        ;; The end func; lines up with begin
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- if - elsif - else - end if
   ((member header '("if "
                     "elsif "
                     "else"))
    (if (or
         (string-prefix-p "elsif"  first-text)
         (string-prefix-p "else"   first-text)
         (string-prefix-p "end if" first-text))
        ;; The elsif/else/end if keywords line up with the if keyword
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- block - exception - catch - end block;
   ((string= header "block")
    (if (string-prefix-p "exception" first-text)
        ;; exception lines up with block
        0
      ;; Other lines are indented
      seed7-indent-width))
   ((string= header "exception")
    (if (or (string-match-p "\\`catch[[:blank:]]" first-text)
            (string-match-p "\\`error[[:blank:]]" first-text))
        ;; catch is indented one level from exception
        seed7-indent-width
      ;; Other lines are indented by 2 levels
      (* 2 seed7-indent-width)))

   ((string= header "catch ")
    (cond
     ((string-prefix-p "end block" first-text)
      ;; end block lines up with block, 1 indentation level less than catch
      (- seed7-indent-width))
     ;; A sibling catch or error clause lines up with the previous one (offset 0)
     ((string-match-p "\\`catch[[:blank:]]" first-text)
      0)
     ((string-match-p "\\`error[[:blank:]]" first-text)
      0)
     (t
      ;; Other lines are indented by 1 level relative to catch
      seed7-indent-width)))

   ;;-- repeat - until
   ((string= header "repeat")
    (if (string-match-p "\\`until[[:blank:]]" first-text)
        ;; until lines up with repeat
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- global - end global
   ((string= header "global")
    (if (string-prefix-p "end global;" first-text)
        ;; 'end global' lines up with 'global'
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- for - end for
   ((string= header "for ")
    (if (string-prefix-p "end for" first-text)
        ;; 'end for' lines up with 'for'
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- while - end while
   ((string= header "while ")
    (if (string-prefix-p "end while" first-text)
        ;; 'end while' lines up with 'while'
        0
      ;; Other lines are indented
      seed7-indent-width))

   ;;-- case - end case;
   ((string= header "case ")
    (if (string-prefix-p "end case" first-text)
        ;; 'end case' lines up with 'case'
        0
      ;; Other lines are indented
      (if (or (string-match-p "\\`when[[:blank:]]" first-text)
              (string-prefix-p "otherwise" first-text))
          ;; the when and otherwise keywords are indented once
          seed7-indent-width
        ;; the statements inside when or otherwise blocks are indented twice
        (* 2 seed7-indent-width))))

   ;;-- const func - return/result-begin
   ((string= header "const func ")
    (if (or (string-match-p "\\`return[[:blank:]]" first-text)
            (string-prefix-p "result"  first-text))
        seed7-indent-width
      ;; otherwise indent below 'return '. BUT other code should handle this.
      (+ seed7-indent-width 7)))
   ((string= header "result")
    (if (or (string-prefix-p "begin" first-text)
            (string-prefix-p "local" first-text))
        ;; begin/local lines up with result
        0
      ;; other lines are indented
      seed7-indent-width))

   ;;-- const proc
   ((string= header "const proc: ")
    seed7-indent-width)

   ((string= header "const type: ")
    (if (or (string-prefix-p "end struct;" first-text)
            (string-prefix-p "end enum;"   first-text))
        ;; end struct/enum is indented once relative to the const type
        seed7-indent-width
      ;; the definitions are indented twice.
      (* 2 seed7-indent-width)))

   ;; (error "Unsupported header %s" header)
   (t 0)))


(defun seed7--on-lineof  (start-pos &optional pos)
  "Return t if point or POS is on first line of block starting at START-POS."
  (<= (save-excursion (goto-char start-pos)
                      (forward-line 0)
                      (point))
      (or pos (point))
      (save-excursion (goto-char start-pos)
                      (line-end-position))))

(defun seed7-line-inside-a-block (n &optional dont-skip-comment-start)
  "Check if line N is inside a Seed7 block.
N is: - :previous-non-empty for the previous non-empty line,
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
      (let* ((current-pos (point))
             ;; Pre-extract the text at the indented start of line N once so
             ;; that seed7--indent-offset-for can use string-prefix-p instead
             ;; of repeated save-excursion + goto-char + looking-at-p calls.
             (line-n-first-text
              (save-excursion
                (skip-chars-forward " \t")
                (buffer-substring-no-properties (point) (line-end-position))))
             (block-start-pos nil)
             (enclosing-block-start-pos nil)
             (enclosing-block-end-pos nil)
             (match-text nil)
             (block-start-indent-column nil)
             (keep-searching t)
             (result nil))
        ;; all block line start regexp fit 1 line: search from end of line
        ;; to detect if the point is on the block start we find.
        (end-of-line)
        (while (and keep-searching
                    (not (bobp)))
          (when (seed7-re-search-backward seed7-block-line-start-regexp)
            ;; get match text, normalize matching hard tabs to spaces.
            (setq match-text (replace-regexp-in-string
                              "[[:blank:]]+$" " "
                              (substring-no-properties (match-string 1))))
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
                  ;; Allow modification of a read-only buffer and ensure that
                  ;; the undo history is not modified by the insertion and
                  ;; removal operations.
                  (let ((inhibit-read-only t))
                    (with-silent-modifications
                      (setq keep-searching nil
                            result (list
                                    (save-excursion
                                      (forward-line 0)
                                      (let ((noop-start (point))
                                            (noop-end nil))
                                        (insert " noop;\n")
                                        (setq noop-end (point))
                                        (forward-line -1)
                                        (unwind-protect
                                            ;; compute indentation with noop
                                            (seed7-calc-indent)
                                          ;; always delete inserted noop
                                          (delete-region noop-start noop-end))))
                                    match-text
                                    enclosing-block-start-pos
                                    enclosing-block-end-pos
                                    block-start-indent-column))))))
               ;;
               ;; Case 2: point is on an internal block start line.
               ((seed7--on-lineof block-start-pos current-pos)
                ;; Look for the previous block and use info from it.
                (goto-char block-start-pos)
                (when (seed7-re-search-backward
                       seed7-block-line-start-regexp)
                  (setq match-text (replace-regexp-in-string
                                    "[[:blank:]]+$" " "
                                    (substring-no-properties (match-string 1))))
                  (setq block-start-pos (point))
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
                                          (seed7--indent-offset-for
                                           match-text
                                           line-n-first-text))
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
                                      (seed7--indent-offset-for
                                       match-text
                                       line-n-first-text))
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

;; --

(defvar-local seed7--indent-last-block-spec nil
  "Buffer-local marker-backed block spec cache for indentation.

Holds the result of the last successful `seed7-line-inside-a-block' call,
converted to a marker-backed list by `seed7--cache-block-spec'.
Used by `seed7--cached-block-spec-current-line' to avoid recomputation
when the current line is still inside the same block.

During normal indentation this variable is dynamically rebound by
`seed7-indent-line', so the cache lifetime is one indentation command
or one region-indentation pass.  Direct callers of `seed7-calc-indent'
may use the buffer-local value.")

(defvar-local seed7--indent-block-bounds nil
  "Buffer-local lightweight block-boundary cache for indentation.

Cons cell (BEGIN-MARKER . END-MARKER) holding the start and end positions
of the last block identified by `seed7-line-inside-a-block-cached'.
Updated whenever `seed7--indent-last-block-spec' is updated.
Used by `seed7--cached-block-bounds' to provide O(1) begin/end lookup
without returning or allocating the full block spec.

During normal indentation this variable is dynamically rebound by
`seed7-indent-line', so the cache lifetime is one indentation command
or one region-indentation pass.  Direct callers of `seed7-calc-indent'
may use the buffer-local value.")

(defun seed7--cache-block-spec (spec)
  "Return a marker-backed copy of SPEC."
  (when spec
    (list (nth 0 spec)
          (nth 1 spec)
          (copy-marker (nth 2 spec))
          (copy-marker (nth 3 spec) t)
          (nth 4 spec))))

(defun seed7--cached-block-bounds ()
  "Return (BEGIN-POS . END-POS) for the current block if cache is valid.
Return nil otherwise.
Uses `(point)' directly — no `seed7-to-indent', no `save-excursion'.
Cost: O(1) — two `marker-position' calls and two integer comparisons.

Returns nil when no cache is present or point has moved outside the
cached block.  Only the begin and end buffer positions of the current
block are returned; the full block spec (keyword, match-text, indent
column) is not exposed."
  (when seed7--indent-block-bounds
    (let ((begin-pos (marker-position (car seed7--indent-block-bounds)))
          (end-pos   (marker-position (cdr seed7--indent-block-bounds))))
      (when (and begin-pos end-pos
                 (<= begin-pos (point) end-pos))
        (cons begin-pos end-pos)))))

(defun seed7--cached-block-spec-current-line ()
  "Return cached block spec for current line when still applicable."
  (when seed7--indent-last-block-spec
    (let* ((current-pos (save-excursion
                          (seed7-to-indent)
                          (point)))
           (start-pos (marker-position (nth 2 seed7--indent-last-block-spec)))
           (end-pos   (marker-position (nth 3 seed7--indent-last-block-spec))))
      (when (and start-pos end-pos
                 (<= start-pos current-pos end-pos))
        (list (nth 0 seed7--indent-last-block-spec)
              (nth 1 seed7--indent-last-block-spec)
              start-pos
              end-pos
              (nth 4 seed7--indent-last-block-spec))))))

(defun seed7-line-inside-a-block-cached (n &optional dont-skip-comment-start)
  "Cached variant of `seed7-line-inside-a-block' for indentation.

N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...

When N is 0 and DONT-SKIP-COMMENT-START is nil, the function first
checks `seed7--cached-block-spec-current-line'; if the cache is still
valid it returns the cached spec immediately without re-scanning (O(1)
fast path).

On a cache miss the function delegates to `seed7-line-inside-a-block'.
When N is 0 and a block is found, two caches are updated as side effects:
- `seed7--indent-last-block-spec' is set to a marker-backed copy of the
  returned spec (via `seed7--cache-block-spec').
- `seed7--indent-block-bounds' is set to a cons cell whose car is the
  marker for the block start position (element 2 of the spec) and whose
  cdr is the marker for the enclosing block end position (element 3).
  This lightweight cons is used by `seed7-calc-indent' for O(1) early
  bound checks on subsequent lines.

These side effects do NOT occur when N is non-zero or when no block is
found.

Return nil if line N is not inside any Seed7 block.
If a block is found, return a list of 5 elements:
- 0: indent column     : indentation column line N should use,
- 1: match string      : string describing the type of block found,
- 2: block start position (the beginning of the start keyword line),
- 3: enclosing block end position,
- 4: indent column of the block start line."
  (or (and (eq n 0)
           (not dont-skip-comment-start)
           (seed7--cached-block-spec-current-line))
      (let ((spec (seed7-line-inside-a-block n dont-skip-comment-start)))
        (when (and (eq n 0) spec)
          (setq seed7--indent-last-block-spec
                (seed7--cache-block-spec spec))
          ;; Also update the lightweight bounds cache used for early-bound
          ;; lookups at the top of `seed7-calc-indent'.
          (setq seed7--indent-block-bounds
                (cons (nth 2 seed7--indent-last-block-spec)
                      (nth 3 seed7--indent-last-block-spec))))
        spec)))

;; ---------------------------------------------------------------------------

(defun seed7-line-inside-until-logic-expression (n &optional
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)
  "Check if line N is inside a Seed7 until logic expression.
N is: - :previous-non-empty for the previous non-empty line,
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
          (if (seed7-re-search-backward "^[[:blank:]]+until[[:blank:]]" scope-begin-pos)
              (progn
                (setq keep-searching nil)
                (unless (seed7-line-code-ends-with 0 ";")
                  ;; found an incomplete until statement remember position
                  (setq block-start-pos (point))
                  ;; Now search the end, which ends on a ';'.
                  (setq enclosing-block-end-pos
                        (seed7-statement-end-pos nil scope-end-pos))
                  (when (and enclosing-block-end-pos
                             (< block-start-pos current-pos enclosing-block-end-pos)
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
N is: - :previous-non-empty for the previous non-empty line,
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
          (if (seed7-re-search-backward "^[[:blank:]]+return[[:blank:]]"
                                        scope-begin-pos)
              (progn
                (setq keep-searching nil)
                (seed7-to-indent)
                (setq start-pos (point))
                (setq found-column (+  (current-column) 5))
                (setq end-pos (seed7-forward-char-pos ";" scope-end-pos
                                                      'point-after))
                (unless (and end-pos
                             (< start-pos current-pos end-pos)
                             (or (not scope-end-pos)
                                 (< end-pos scope-end-pos)))
                  (setq found-column nil)))
            ;; no return found
            (setq found-column nil)
            (setq keep-searching nil)))
        found-column))))

(defun seed7-line-inside-argument-list-section (&optional
                                                scope-begin-pos
                                                scope-end-pos)
  "Check if point is inside a Seed7 action argument list section.
A Seed7 action is a function or a procedure.

SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If it finds that the line is inside the procedure or function argument list
section, it returns the indentation column of the proc/func declaration.
If it detects that it is outside, it returns nil."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-argument-list-section: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (let ((current-pos (point))
          (found-column nil))
      (when (seed7-move-to-line 0)
        ;; STEP 1: Locate the owning proc/func declaration.
        ;;
        ;; If the current line IS a proc/func declaration start, use it
        ;; directly.  Searching backward from here would skip past the
        ;; current declaration and land on the previous one.
        (if (seed7-line-is-procfunc-beg-of-decl 0)
            (progn
              (seed7-to-indent)
              (setq found-column (current-column)))
          ;; Current line is a continuation line (or unrelated);  search
          ;; backward for the nearest proc/func declaration start.
          (when (seed7-re-search-backward seed7-procfunc-beg-of-decl-nc-re
                                          scope-begin-pos)
            ;; store the function/procedure indented column
            (seed7-to-indent)
            (setq found-column (current-column))))
        ;;
        ;; STEP 2: Validate that current-pos is strictly inside the '(...)'
        ;; argument list of the declaration we just found.
        ;;
        ;; This also correctly returns nil when line N is the declaration
        ;; line itself (current-pos is before '(', so the range check fails).
        (when found-column
          (let ((open-paren-pos (seed7-forward-char-pos "(" scope-end-pos)))
            (if open-paren-pos
                (progn
                  (goto-char open-paren-pos)
                  (seed7--with-forward-sexp
                    ;; Point is now one past ')'.
                    (let ((close-paren-pos (1- (point))))
                      (unless (< open-paren-pos current-pos close-paren-pos)
                        (setq found-column nil)))
                    :on-error
                    ;; forward-sexp failed - closing paren not found in scope.
                    (setq found-column nil)))
              ;; No opening '(' within scope.
              (setq found-column nil))))
        found-column))))

(defun seed7-line-inside-array-definition-block (n &optional
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)
  "Check if line N is inside an array definition block.
N is: - :previous-non-empty for the previous non-empty line,
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
- 3: block end position: position of the character immediately after
     the closing delimiter (one past the `)').

If SCOPE-END-POS is non-nil, it is treated as an inclusive upper bound:
the position one past the closing delimiter must be less than or equal
to SCOPE-END-POS."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-array-definition-block: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-indent-column nil))
        (when (seed7-re-search-backward
               seed7--line-array-definition-start-regexp
               scope-begin-pos)
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (goto-char (1- (match-end 0))) ; position at "("
          (seed7--with-forward-sexp
            ;; with point now one past the closing delimiter
            (when (and (< block-start-pos original-pos (point))
                       (or (not scope-end-pos)
                           (<= (point) scope-end-pos)))
              (list block-indent-column
                    "array"
                    block-start-pos
                    (point)))))))))

(defun seed7-line-at-endof-array-definition-block (n &optional
                                                     dont-skip-comment-start)
  "Check if line N is the end of an array definition block.
Return the indentation column of the array definition block statement
if line N is the end of an array block, nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
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
             (line-end-pos (line-end-position))
             (block-start-pos nil)
             (enclosing-block-end-pos nil))
        ;; Fast reject: only a code line ending with ");" can close an
        ;; array definition block.  This avoids an unnecessary backward
        ;; search on almost all ordinary previous lines.
        (when (seed7-line-code-ends-with 0 ");")
          (goto-char line-end-pos)
          ;; Bound the search to the current line.  The old code searched
          ;; backward without a bound, which could scan far above line N.
          (when (seed7-re-search-backward ");" line-start-pos)
            (setq enclosing-block-end-pos (point))
            (seed7--with-backward-sexp
              (seed7-to-indent)
              (when (looking-at-p seed7--array-definition-start-regexp)
                (setq block-start-pos (point))
                (when (< block-start-pos line-start-pos enclosing-block-end-pos line-end-pos)
                  block-indent-column)))))))))

(defun seed7-line-inside-set-definition-block (n &optional
                                                 scope-begin-pos
                                                 scope-end-pos
                                                 dont-skip-comment-start)
  "Check if line N is inside a set definition block.
N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...

If nothing found it returns nil.
If line N is inside a set definition block, it returns a list with the
following information:
- 0: indent column : indentation column the line N should use,
- 1: string: \"set\"
- 2: block start position,
- 3: block end position: position of the character immediately after
     the closing delimiter (one past the `}').

If SCOPE-END-POS is non-nil, it is treated as an inclusive upper bound:
the position one past the closing delimiter must be less than or equal
to SCOPE-END-POS."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-set-definition-block: \
Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((original-pos (point))
            (block-start-pos nil)
            (block-indent-column nil))
        (when (seed7-re-search-backward
               seed7--line-set-definition-start-regexp
               scope-begin-pos)
          (setq block-start-pos (point))
          (skip-chars-forward " \t")
          (setq block-indent-column (current-column))
          (goto-char (1- (match-end 0))) ; position at "{"
          (seed7--with-forward-sexp
            ;; with point now one past the closing delimiter
            (when (and (< block-start-pos original-pos (point))
                       (or (not scope-end-pos)
                           (<= (point) scope-end-pos)))
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
if line N is inside a logic check expression, nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
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


(defun seed7-line-inside-assign-statement-continuation (n
                                                        &optional
                                                        scope-begin-pos
                                                        scope-end-pos
                                                        dont-skip-comment-start)
  "Check if line N is inside a Seed7 assignment statement.
Return the indentation column of the code following the statement
operator on the assignment statement if line N is inside a statement
continuation line, nil otherwise.

N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
        is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...

If SCOPE-BEGIN-POS and SCOPE-END-POS are non-nil, they identify the
search boundaries for the assignment operator and statement-end searches."
  ;; Note: if point is inside a comment embedded inside a assign statement
  ;;       line continuation, the function does return the column as if it
  ;;       was code; it's OK because we want the comment to be indented like
  ;;       the code.
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((current-pos (point))
             (assignment-pos (seed7-assign-op-pos scope-begin-pos))
             (statement-end-pos (when assignment-pos
                                  (seed7-statement-end-pos assignment-pos scope-end-pos))))
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
if line N is the end of a set definition block, nil otherwise.
N is: - :previous-non-empty for the previous non-empty line,
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
             (line-end-pos (line-end-position))
             (block-start-pos nil)
             (enclosing-block-end-pos nil))
        ;; Fast reject: only a code line ending with "};" can close a set
        ;; definition block.  This avoids an unnecessary backward
        ;; search on almost all ordinary previous lines.
        (when (seed7-line-code-ends-with 0 "};")
          (goto-char line-end-pos)
          ;; Bound the search to the current line.  The old code searched
          ;; backward without a bound, which could scan far above line N.
          (when (seed7-re-search-backward "};" line-start-pos)
            (setq enclosing-block-end-pos (point))
            (seed7--with-backward-sexp
              (seed7-to-indent)
              (when (looking-at-p seed7--set-definition-start-regexp)
                (setq block-start-pos (point))
                (when (< block-start-pos line-start-pos enclosing-block-end-pos line-end-pos)
                  block-indent-column)))))))))

;; --

(defun seed7--paren-pair-string (open-char)
  "Return delimiter-pair string corresponding to OPEN-CHAR.
Return nil if OPEN-CHAR is none of the supported opening parens characters."
  (cond
   ((eq open-char ?\() "()")
   ((eq open-char ?\[) "[]")
   ((eq open-char ?\{) "{}")
   (t nil)))

(defun seed7-line-inside-syntax-parens-pair
    (n &optional scope-begin-pos scope-end-pos dont-skip-comment-start)
  "Fast syntax-table check for a parens pair containing line N.

N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A positive integer for next lines, a negative integer for previous
        lines: 1: next line, -1: previous line, etc.

If SCOPE-BEGIN-POS is non-nil, bound the backward search to that position.
If SCOPE-END-POS is non-nil, it is treated as an exclusive upper bound: the
closing paren must be at a position strictly less than SCOPE-END-POS.

Return nil if syntax state does not identify a usable pair.
If an appropriate parens pair is found, return a list of 4 elements:
- 0: indentation column of the character after the opening paren
- 1: string identifying the parens pair found
- 2: position of the opening paren
- 3: position of the closing paren."
  ;; Note: This is only called by `seed7-line-inside-parens-pair' and
  ;; that function checks the validity of the scope-begin-pos and
  ;; scope-end-pos arguments; therefore they are not checked here.
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let* ((line-start (point))
             (open-pos (nth 1 (syntax-ppss line-start))))
        (when (and open-pos
                   (or (not scope-begin-pos)
                       (< scope-begin-pos open-pos))
                   (or (not scope-end-pos)
                       (< open-pos scope-end-pos)))
          (goto-char open-pos)
          (let* ((open-char (char-after open-pos))
                 (op (seed7--paren-pair-string open-char))
                 (indent-column (1+ (current-column))))
            (when op
              (seed7--with-forward-sexp
                ;; with point now one past the closing delimiter
                (let ((end-pos (1- (point))))
                  (when (and (< open-pos line-start end-pos)
                             (or (not scope-end-pos)
                                 (< end-pos scope-end-pos)))
                    (list indent-column op open-pos end-pos)))
                :on-error
                nil))))))))

(defun seed7-line-inside-parens-pair (n &optional
                                        scope-begin-pos
                                        scope-end-pos
                                        dont-skip-comment-start)
  "Check if line N is inside a parens pair.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A positive integer for next lines, a negative integer for previous
        lines: 1: next line, -1: previous line, etc.

If SCOPE-BEGIN-POS is non-nil, bound the backward search to that position.
If SCOPE-END-POS is non-nil, it is treated as an exclusive upper bound: the
closing paren must be at a position strictly less than SCOPE-END-POS.

Return nil if nothing is found.
If an appropriate parens pair is found, return a list of 4 elements:
- 0: indentation column of the character after the opening paren
- 1: string identifying the parens pair found
- 2: position of the opening paren
- 3: position of the closing paren."
  (unless (or (not scope-end-pos)
              (< (or scope-begin-pos 0) scope-end-pos))
    (error "seed7-line-inside-parens-pair: Invalid boundaries: begin=%S, end=%S"
           scope-begin-pos scope-end-pos))
  (or (seed7-line-inside-syntax-parens-pair
       n scope-begin-pos scope-end-pos dont-skip-comment-start)
      (save-excursion
        (when (seed7-move-to-line n dont-skip-comment-start)
          (let ((line-start (point))
                (found-spec nil))
            ;; Search backward once for any opener.  Since we search from the
            ;; current line toward the beginning, the first valid enclosing pair
            ;; is the innermost one.
            (while (and (not found-spec)
                        (seed7-re-search-backward seed7--open-paren-regexp
                                                  scope-begin-pos))
              (let* ((start-pos (match-beginning 0))
                     (open-char (char-after start-pos))
                     (op (seed7--paren-pair-string open-char))
                     (indent-column nil))
                (goto-char start-pos)
                (setq indent-column (1+ (current-column)))
                (seed7--with-forward-sexp
                  ;; with point now one past the closing delimiter
                  (let ((end-pos (1- (point))))
                    (when (and op
                               (< (or scope-begin-pos 0)
                                  start-pos line-start end-pos)
                               (or (not scope-end-pos)
                                   (< end-pos scope-end-pos)))
                      (setq found-spec
                            (list indent-column op start-pos end-pos))))
                  :on-error
                  ;; Malformed/incomplete delimiter pair.  Ignore this opener
                  ;; and continue searching farther backward.
                  nil)
                ;; Continue before this opener if it did not produce a result.
                (goto-char start-pos)))
            found-spec)))))

;; --

(defun seed7-line-inside-parens-pair-column (n &optional
                                               scope-begin-pos
                                               scope-end-pos
                                               dont-skip-comment-start)
  "Check if line N is inside a parens pair.
N is: - :dont-move to keep point at current position
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A positive integer for next lines, a negative integer for previous
        lines: 1: next line, ...
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
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A positive integer for next lines, a negative integer for previous
        lines: 1: next line, -1: previous line, etc.
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If nothing is found it returns nil.
If the appropriate parens pair is found it returns a list of 4 elements:
- 0: indentation column of the character after the opening parens of
     the inner-most nesting.
- 1: string: parens pair found.
- 2: position of the opening paren of the inner-most nesting.
- 3: position of the end paren of the inner-most nesting."
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
      - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
SCOPE-BEGIN-POS and SCOPE-END-POS are the search begin and end boundaries.
If nothing is found it returns nil.
If the appropriate parens pair is found it returns the indentation column of
the character after the opening parens of the inner-most nesting."
  (car-safe (seed7-line-inside-nested-parens-pairs n nested-depth
                                                   scope-begin-pos
                                                   scope-end-pos
                                                   dont-skip-comment-start)))

(defun seed7-indentation-of-previous-non-string-line ()
  "Return indentation of the previous non-blank, non-comment, non-string line.
Return nil when no such previous line exists."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (not (bobp)))
        (forward-line -1)
        (when (and (not (seed7-line-starts-with 0 "\""))
                   (not (seed7-inside-comment-p (point)))
                   (not (seed7-inside-string-p (point)))
                   (save-excursion
                     (skip-chars-forward " \t")
                     (not (looking-at-p "\n"))))
          (setq found t)))
      (when found
        (skip-chars-forward " \t")
        (current-column)))))


(defun seed7-line-is-procfunc-beg-of-decl (n &optional dont-skip-comment-start)
  "Return indent column when line N is a procedure or function declaration.
Skip comment start unless DONT-SKIP-COMMENT-START is non-nil."
  (seed7-line-starts-with n seed7-procfunc-beg-of-decl-nc-re
                          dont-skip-comment-start))

(defun seed7-line-is-defun-end (n &optional dont-skip-comment-start)
  "Return the defining-line indentation when line N ends a definition.
Recognizes func, proc, struct, enum, forward, and native/action declaration
forms. Return nil when line N is not such an end/declaration line.

N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT-START
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before..."
  (save-excursion
    (when (seed7-move-to-line n dont-skip-comment-start)
      (let ((previous-defun-column nil)
            (first-word-on-line (seed7--current-line-nth-word 1))
            (end-pos (line-end-position)))
        (cond
         ;; Fast reject: ordinary code lines cannot be definition-ending lines.
         ((not (member first-word-on-line '("end" "const")))
          nil)

         ;; Handle line that is an end func, struct or enum.
         ((seed7-line-starts-with-any
           0
           (list
            "end[[:blank:]]+?func[[:blank:]]*?;"
            "end[[:blank:]]+?struct[[:blank:]]*?;"
            "end[[:blank:]]+?enum[[:blank:]]*?;"))
          (seed7-to-block-backward nil :dont-push-mark)
          (seed7-current-line-indent))

         ;; Handle forward and native/action declarations of func and proc.
         ((seed7--set
           (seed7-line-starts-with-any
            0
            (list
             seed7-func-forward-or-action-declaration-nc-re
             seed7---inner-callables-4
             seed7-proc-forward-or-action-declaration-re)
            nil
            end-pos)
           previous-defun-column)
          previous-defun-column)

         ;; Fallback only for plausible lines, not for every ordinary code line.
         (t
          (let ((endline-pos nil)
                (endline-pos-2 nil))
            (end-of-line)
            (setq endline-pos (point))
            (when (ignore-errors
                    (seed7-beg-of-defun nil :silent :dont-push-mark)
                    (setq previous-defun-column (current-column)))
              (setq endline-pos-2
                    (ignore-errors
                      (seed7-end-of-defun nil :silent :dont-push-mark)
                      (point)))
              (when (eq endline-pos endline-pos-2)
                previous-defun-column)))))))))

;; ---------------------------------------------------------------------------
;;** Seed7 Gradual Block Selection Support for expand-region
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-er-mark-enclosing-block ()
  "Mark the innermost Seed7 block strictly enclosing the current region.

As a first step if the exiting block does not include the end of the line,
return a marked area that includes the end of the line, otherwise return
the are corresponding to the innermost block that includes the currently
selected region.

If the region already covers a block, mark the next outer (parent) block.
Intended for use with the `expand-region' library.

This provides block-by-block expansion between line selection and full
defun (function/procedure) selection."
  (let* ((orig-beg (if (use-region-p) (region-beginning) (point)))
         (orig-end (if (use-region-p) (region-end) (point)))
         (found-begin.end nil))
    ;; Search outward until we find a block strictly larger than the region.
    (save-excursion
      (goto-char orig-beg)
      (let ((done nil))
        (while (not done)
          (let ((info (seed7-line-inside-a-block 0)))
            (if info
                (let ((blk-beg (nth 2 info))
                      (blk-end (nth 3 info)))
                  (cond
                   ;; Found a block strictly larger: use it.
                   ((and blk-beg blk-end
                         (or (< blk-beg orig-beg)
                             (> blk-end orig-end)))
                    ;; Check if the orig-end is at the end of line.
                    ;; If not, return a "block" that reach the end of line.
                    (let ((real-orig-end (save-excursion
                                           (goto-char orig-end)
                                           (line-end-position))))
                      (if (> real-orig-end orig-end)
                          (setq found-begin.end (cons orig-beg real-orig-end))
                        (setq found-begin.end (cons blk-beg blk-end)))
                      (setq done t)))
                   ;; Block matches the region exactly (or is smaller):
                   ;; step above its start line to find the parent.
                   ((and blk-beg (> blk-beg (point-min)))
                    (goto-char blk-beg)
                    (forward-line -1))
                   ;; At buffer top with no larger block found.
                   (t (setq done t))))
              ;; seed7-line-inside-a-block returned nil: no block found.
              (setq done t))))))
    ;; Apply region only when a strictly larger block was found.
    (when found-begin.end
      (goto-char (car found-begin.end))
      (set-mark (cdr found-begin.end)))))

(defun seed7--setup-expand-region ()
  "Insert `seed7-er-mark-enclosing-block' into `er/try-expand-list'.
Places block expansion just before `er/mark-defun' so that expand-region
cycles: word → symbol → line → [block …] → defun."
  ;; Only allowed to proceed in a seed7-mode buffer.
  (if (and (eq major-mode 'seed7-mode)
           (boundp 'er/try-expand-list))
      (progn
        ;; All is OK: proceed.
        (make-local-variable 'er/try-expand-list)
        (let ((pos (cl-position 'er/mark-defun er/try-expand-list)))
          (if pos
              ;; Insert before er/mark-defun
              (setq er/try-expand-list
                    (append (cl-subseq er/try-expand-list 0 pos)
                            '(seed7-er-mark-enclosing-block)
                            (cl-subseq er/try-expand-list pos)))
            ;; Fallback: append at the end
            (setq er/try-expand-list
                  (append er/try-expand-list
                          '(seed7-er-mark-enclosing-block))))))
    ;; invalid call - should never happen
    (error "seed7--setup-expand-region called from a non Seed7 buffer: %S"
           (current-buffer))))

(defun seed7--activate-expand-region ()
  "Activate Seed7 enhanced expansion for all existing Seed7 buffers."
  ;; The expand-region feature was loaded but not on a command
  ;; issued from a seed7-mode buffer: we know see7-mode is loaded
  ;; and now that seed7-mode is loaded too.
  ;; Activate the enhanced expansion in all opened seed7-mode.
  ;; Do like `er/enable-mode-expansions' does it but without creating a hook
  ;; since seed7-mode is already aware.
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'seed7-mode)
          (seed7--setup-expand-region))))))

;; ---------------------------------------------------------------------------
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

;;** Seed7 Position Value Identification
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-position-of-end-of-statement (n &optional dont-skip-comment)
  "Return position of the end of statement semicolon for line N statement.
Issue error if not found.
In either case do not move point.
N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
Skip comment unless DONT-SKIP-COMMENT is non-nil."
  (save-excursion
    (let ((found-pos nil))
      (when (seed7-move-to-line n dont-skip-comment)
        (while (and (not (seed7-line-code-ends-with 0 ";"))
                    (not (eobp)))
          (forward-line 1))
        (setq found-pos (seed7-line-code-ends-with 0 ";")))
      (or found-pos (error "No statement end found for N: %S" n)))))

(defun seed7-bol-position (n &optional dont-skip-comment)
  "Return position of the beginning of line N.
Issue error if not found.
Do not move point.
N is: - :previous-non-empty for the previous non-empty line,
        skipping lines with starting comments unless DONT-SKIP-COMMENT
         is non-nil,
      - 0 for the current line,
      - A negative number for previous lines: -1 previous, -2 line before...
Skip comment unless DONT-SKIP-COMMENT is non-nil."
  (save-excursion
    (if (seed7-move-to-line n dont-skip-comment)
        (progn
          (forward-line 0)
          (point))
      (error "seed7-bol-position: No line N (%S) found" n))))

;;** Seed7 Indentation Calculator Function
;;   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun seed7-calc-indent (&optional treat-comment-line-as-code recurse-count)
  "Calculate and return indentation (in columns) of current line of code.
When TREAT-COMMENT-LINE-AS-CODE is non-nil a comment line is processed as if
  it was a code line, allowing the indentation logic to handle comments.
The RECURSE-COUNT should be nil on the first call, 1 on the first recursive
  call.  Only one recursion is allowed."
  (let* ((recurse-count (or recurse-count 0))
         ;; Eagerly probe the previous call's block boundaries.
         ;; Cost: O(1) — two marker-position comparisons, uses (point)
         ;;              directly; no `seed7-to-indent' overhead.
         ;; Returns (begin-pos . end-pos) or nil; never recomputes.
         ;; Only begin and end positions are used here; the full block spec
         ;; is left for the `seed7-line-inside-a-block-cached' call below.
         ;; The previously-computed begin/end bounds can be passed to
         ;; to expensive backward-search helpers below, bounding their
         ;; scan to the current block rather than the whole file.
         ;; The later `seed7-line-inside-a-block-cached' call only recomputes
         ;; when point moves out of previous block.
         (cached-bounds   (seed7--cached-block-bounds))
         ;; Widen by 1 on each side so backward searches that land exactly
         ;; on the block keyword boundary still succeed.
         (early-begin-pos (when cached-bounds
                            (max (point-min) (1- (car cached-bounds)))))
         (early-end-pos   (when cached-bounds
                            (min (point-max) (1+ (cdr cached-bounds)))))
         ;;
         (indent-step nil) ; will be initialized later only if needed
         (first-word-on-line (seed7--current-line-nth-word 1))
         (indent-column nil)
         (indent-column2 nil)
         (spec-list nil))
    ;; don't indent blank lines
    (unless (and (not first-word-on-line)
                 (seed7-blank-line-p))
      (cond
       ((> recurse-count 1)
        (error "Recursion done more than once: implementation logic error!"))

       ;; in comment
       ((and (seed7-current-line-start-inside-comment-p)
             (not treat-comment-line-as-code))
        (setq indent-column (seed7-comment-column recurse-count)))

       ;; In an array or set definition, indent 1 level unless the line is
       ;; inside 2 nested parens.  In that case align with the inside of
       ;; the inner-most parens.
       ((or (seed7--set (seed7-line-inside-array-definition-block
                         0 early-begin-pos early-end-pos)
                        spec-list)
            (seed7--set (seed7-line-inside-set-definition-block
                         0 early-begin-pos early-end-pos)
                        spec-list))
        (if (seed7--set (seed7-line-inside-nested-parens-pairs-column
                         0 2
                         (nth 2 spec-list)
                         (nth 3 spec-list))
                        indent-column2)
            (setq indent-column indent-column2)
          (setq indent-column (+ (nth 0 spec-list)
                                 seed7-indent-width))))

       ;; -- in string -------------------------
       ((seed7-line-isa-string 0)
        (save-excursion
          (cond
           ;; If previous line starts with an assignment operator,
           ;; indent this line.  Handle this Seed7 code:
           ;;         +--- hit return here
           ;;         |
           ;;         v
           ;; stri := "-" & randDigitStri(length);
           ;;
           ((seed7--set (seed7-line-ends-with
                         :previous-non-empty
                         seed7-predef-assignment-operator-regexp)
                        indent-column)
            (setq indent-column (+ indent-column seed7-indent-width)))
           ;;
           ;; If inside parens pairs
           ((seed7--set (seed7-line-inside-parens-pair-column
                         0 early-begin-pos early-end-pos)
                        indent-column))
           ;;
           ;; if previous line starts with a string, align the string to it.
           ((seed7-line-isa-string :previous-non-empty)
            (forward-line -1)
            (search-forward "\"")
            (setq indent-column (1- (current-column))))
           ;;
           ;; If the previous non-empty line ends with the Seed7 string-
           ;; concatenation operator `<&' AND that line itself contains a `"',
           ;; align the current string's opening `"' to the column of that `"'.
           ;;
           ;; Handles multi-line string-concatenation expressions such as:
           ;;
           ;;   return "(" <& bitfield.mask radix 2 lpad0 32 <&
           ;;         ", " <& bitfield.rShift lpad 2 <&     ← aligned to "("
           ;;         ", " <& bitfield.scale <& ")";
           ;;
           ;; When the previous line ends with `<&' but has no `"' (e.g. a
           ;; non-string expression like `header.fileSize rpad 10 <&'), this
           ;; case produces nil and the `t' fallback below handles it correctly.
           ((seed7--set
             (save-excursion
               (when (seed7-move-to-line :previous-non-empty)
                 (let* ((lbeg (line-beginning-position))
                        (lend (line-end-position)))
                   (goto-char lend)
                   (skip-chars-backward " \t" lbeg)
                   (when (and (>= (- (point) lbeg) 2)
                              (string= "<&"
                                       (buffer-substring-no-properties
                                        (- (point) 2) (point))))
                     (goto-char lbeg)
                     (when (search-forward "\"" lend t)
                       (1- (current-column)))))))
             indent-column))
           ;;
           ;; If the previous non-empty line ends with `&' or `<&' AND that
           ;; line also contains an assignment operator (`:=', `&:=', etc.),
           ;; align the current string to the column of the RHS of that
           ;; assignment — i.e. the first non-space character after the
           ;; operator on that line.
           ;;
           ;; Handles: (bmp.s7i line 888)
           ;;   stri &:= bytes(rawDataSize + 54, UNSIGNED, LE, 4) &
           ;;            "\0;" mult 4  &   ← aligns to "bytes(" = column 13
           ;;
           ;; This clause fires only when the `<&'+`"' clause above did NOT
           ;; fire (the previous line either ends with plain `&' rather than
           ;; `<&', or ends with `<&' but contains no `"').
           ((seed7--set
             (save-excursion
               (when (seed7-move-to-line :previous-non-empty)
                 (let* ((lbeg (line-beginning-position))
                        (lend (line-end-position)))
                   ;; The previous line must end with `&' (covers both plain
                   ;; `&' and `<&', since both have `&' as the last char).
                   (goto-char lend)
                   (skip-chars-backward " \t" lbeg)
                   (when (and (> (point) lbeg)
                              (= (char-before (point)) ?&))
                     ;; Look for an assignment operator anywhere on this line.
                     (goto-char lbeg)
                     (when (seed7-re-search-forward
                            seed7-predef-assignment-operator-regexp lend)
                       ;; Column of the first non-whitespace char after the
                       ;; operator — that is where the RHS expression begins.
                       (skip-chars-forward " \t")
                       (current-column))))))
             indent-column))
           ;;
           ;; Fallthrough: use the indentation of the nearest preceding
           ;; non-string line.  Only warn when that heuristic also fails
           ;; (i.e. when `seed7-indentation-of-previous-non-string-line'
           ;; returns nil), meaning the indentation is truly unresolvable.
           (t
            (let ((col (seed7-indentation-of-previous-non-string-line)))
              ;; [ TODO 2026-06-24, by Pierre Rouleau: replace if by when for testing?]
              (if col
                  (setq indent-column col)
                (message
                 "At line %d: string line syntax not yet supported! Recurse count=%d Please report."
                 (seed7-current-line-number)
                 recurse-count)))))))

       ;; -- Special rule: if a line starts with a Seed7 assignment operator,
       ;; consider that line manually indented and keep it where it is.
       ;; This allows a long multi-line statement to be lined up on the
       ;; assignment operator placed on the line after its rvalue for the
       ;; explicit purpose of allowing that manual alignment mechanism.
       ((seed7--set (seed7-line-starts-with
                     0
                     seed7-predef-assignment-operator-regexp)
                    indent-column))
       ;;
       ;; Assignment statement continuation
       ((and (seed7--set (seed7-line-inside-assign-statement-continuation
                          0 early-begin-pos early-end-pos)
                         indent-column2)
             (not (seed7-line-inside-parens-pair-column
                   0
                   (seed7-bol-position -1)
                   (seed7-position-of-end-of-statement -1))))
        (setq indent-column indent-column2))

       ;; Handle special cases before checking if line is inside a block
       ;; --------------------------------------------------------------
       ;; Check if line is below end of func|struct|enum before checking if it
       ;; is inside a block and is not itself a 'end func|struct|enum;' line.
       ;; This ensures it handles the next line properly.
       ((and (or (not (seed7-line-is-defun-end 0))
                 (seed7-line-is-procfunc-beg-of-decl 0))
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

       ((seed7--set (seed7-line-inside-a-block-cached 0) spec-list)
        ;; Inside a block.  Check if inside any special zones first.
        ;; For all of those extra checks limit the zone to the scope of the
        ;; current block to improve efficiency. Extend the boundary by 1
        ;; character to allow searches to succeed if they match at the edges.
        (let ((begin-pos (max (point-min) (1- (nth 2 spec-list))))
              (end-pos   (min (point-max) (1+ (nth 3 spec-list)))))
          (cond
           ((seed7--set                 ; inside parens pair?
             (seed7-line-inside-parens-pair-column 0 begin-pos end-pos)
             indent-column))
           ((seed7--set                 ; inside logic expression?
             (seed7-line-inside-logic-check-expression 0 begin-pos end-pos)
             indent-column))
           ((seed7--set                 ; inside argument list?
             (seed7-line-inside-argument-list-section begin-pos end-pos)
             indent-column)
            (setq indent-column (+ indent-column seed7-indent-width)))
           ((seed7--set                 ; inside until  ...; area?
             (seed7-line-inside-until-logic-expression 0 begin-pos end-pos)
             indent-column))
           ((seed7--set                 ; inside func return?
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
       ;; following the open paren; any of: ( { [
       ((seed7--set (seed7-line-inside-parens-pair-column 0)
                    indent-column))
       ((and (string= first-word-on-line "end")
             (string= (seed7--current-line-nth-word 2) "block"))
        (setq indent-step (- (or indent-step
                                 (seed7-line-indent-step :previous-non-empty))
                             2)))
       ;;
       ;;
       ((string= first-word-on-line "until")
        (setq indent-step (1- (or indent-step
                                  (seed7-line-indent-step :previous-non-empty)))))
       ;;
       ;;
       ((and (string= first-word-on-line "var")
             (seed7-line-starts-with :previous-non-empty "include "))
        (setq indent-step 0))

       ((seed7-line-starts-with 0 "(")
        ;; for block comment comments or code within parentheses,
        ;; if did not find a rule report it.  For comment it will be
        ;; caught by `seed7-comment-column' and that will force indent to 0,
        ;; inside code it will leave the line unchanged and will print the
        ;; error.
        (error "No rule yet to indent line %d" (seed7-current-line-number)))))
    (if indent-column
        indent-column
      (* (or indent-step
             (seed7-line-indent-step :previous-non-empty))
         seed7-indent-width))))


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
If point was inside the indentation space move it to first non-whitespace,
otherwise leave point over the same character.
If a region is marked, use it to identify the lines that must be indented,
then deactivates it (to prevent the area to limit searches)."
  (interactive "*")
  (let ((move-point (seed7-inside-line-indent-p))
        ;; clear both caches; code below repopulates them per line via the
        ;; call to `seed7--indent-one-line' --> `seed7-calc-indent'.
        (seed7--indent-last-block-spec nil)
        (seed7--indent-block-bounds    nil))
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

(defun seed7-indent-block ()
  "Indent the block enclosing point.  Do not move point."
  (interactive)
  (save-excursion
    (seed7-to-block-forward :dont-push-mark)
    (set-mark (point))
    (seed7-to-block-backward :at-beginning-of-line :dont-push-mark)
    (seed7-indent-line)))

(defun seed7-fill ()
  "Refill/justify comment or string paragraph, or re-indent current code block."
  (interactive)
  (if (or (seed7-inside-comment-p)
          (seed7-inside-string-p))
      (fill-paragraph)
    (seed7-indent-block)))

;; ---------------------------------------------------------------------------
;;* Seed7 Code Template Expansion
;;  =============================

(defun seed7--delete-char-and-mark ()
  "Delete one character and put a tempo marker at its position."
  (delete-char 1)
  (tempo-insert-mark (point-marker)))

(defun seed7--delete-char-and-mark-at-column (indented-column)
  "Delete one character at specified INDENTED-COLUMN number.
Also add a tempo marker at that location."
  (seed7-to-indent)
  (when (> indented-column 0)
    (forward-char indented-column))
  (seed7--delete-char-and-mark))

(defun seed7--delete-char-and-mark-at (indented-column)
  "Delete one character at INDENTED-COLUMN number or each one in the list.
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
  (insert "include \".s7i\";")
  (seed7-to-indent)
  (forward-char 9)
  (save-excursion
    (indent-for-tab-command)))

(defun seed7-insert-procedure-declaration ()
  "Insert the code template for a procedure declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
  "Insert the code template for an enumeration declaration.
Leave point at first position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
  "Insert an if statement.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
  "Insert an if statement with an else clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
  "Insert an if statement with an elsif clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
  "Insert an if statement with an elsif and an else clause.
Leave point at condition position to fill.
Use \\[tempo-forward-mark] to move to next position to fill,
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
  (interactive "*")
  (insert "for key I range L until C do\n E\n end for;")
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
and \\[tempo-backward-mark] to move to previous one."
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
;; This section provides the `seed7-check-or-compile' command which either
;; checks the validity of the Seed7 code (static check, no executable
;; generated) or compiles it, using the Seed7 tool identified by the
;; `seed7-checker' user-option by default or `seed7-compiler' if its optional
;; compile argument is non-nil.
;;
;; The usage hierarchy is:
;;
;;  * `seed7-check-or-compile'
;;    - `seed7-check-file'
;;      D `seed7--checker-diagnostic-regexp'   (D := data)
;;      D `seed7--compiler-diagnostic-regexp'  (D := data)
;;
;; The `seed7-check-file' function static checks or compiles a file and
;; returns a cons cell `(EXIT-CODE . DIAGNOSTICS)`.  It does not use the shell
;; to execute the Seed7 commands specified by the `seed7-checker' or
;; `seed7-compiler' user options.  It uses `seed7--checker-diagnostic-regexp'
;; or `seed7--compiler-diagnostic-regexp' depending on which tool is invoked,
;; to parse the messages output of either Seed7 tool to extract the
;; information.

(defconst seed7--checker-diagnostic-regexp
  "^\\([^(]+\\)(\\([0-9]+\\)):[ \t]+\\([A-Z0-9_]+\\):[ \t]+\\(.*\\)"
  "Regexp matching a Seed7 static checker (s7check) diagnostic line.
Expected format produced by s7check:
  FILENAME(LINE): ERROR_CODE: message text
Groups:
  1 - absolute filename
  2 - line number (integer string)
  3 - symbolic error code (e.g. NO_MATCH, DECL_FAILED,
      BASE2TO36ALLOWED, UTF16_SURROGATE_CHAR_FOUND)
  4 - message text
Context lines, source-excerpt lines, and ^ pointer lines do NOT match
this pattern.")

(defconst seed7--compiler-diagnostic-regexp
  "^\\*\\*\\* \\([^(]+\\)(\\([0-9]+\\)):\\([0-9]+\\):[ \t]+\\(.*\\)"
  "Regexp matching a Seed7 compiler (s7c) diagnostic line.
Expected format produced by s7c:
  *** FILENAME(LINE):COLUMN: message text
Groups:
  1 - absolute filename
  2 - line number (integer string)
  3 - column number (integer string, 1-based)
  4 - message text
Unlike s7check, s7c does not emit a symbolic error code.
Header lines (\"SEED7 COMPILER Version ...\", \"Source: ...\",
\"Compiling the program ...\") and the footer (\"N errors found\")
do NOT match this pattern and are therefore skipped during parsing.")


(defun seed7--parse-diagnostics (compile out-buf)
  "Parse s7c/s7check diagnostics from text in OUT-BUF.

If COMPILE is non-nil, parse `seed7-compiler' (s7c) output;
otherwise parse `seed7-checker' (s7check) output.

Return a list (in source order) of plists, each with the keys:
    :file    - absolute source filename string
    :line    - line number as an integer
    :column  - column number as an integer, 1-based (s7c only; nil for s7check)
    :code    - symbolic error code string (s7check only, e.g. \"NO_MATCH\");
               nil when the compiler (s7c) is used
    :message - diagnostic message text string
    :context - list of continuation/context strings that followed the
               diagnostic line in the tool's output (may be nil)."
  (with-current-buffer out-buf
    (goto-char (point-min))
    (let ((diag-re    (if compile
                          seed7--compiler-diagnostic-regexp
                        seed7--checker-diagnostic-regexp))
          ;; other local variables that are all initialized to nil
          diagnostics cur-file cur-line cur-col cur-code cur-message
          context-lines in-error)
      (cl-flet ((flush-current ()
                  "Push the accumulated diagnostic (if any) onto results."
                  (when in-error
                    (push (list :file    cur-file
                                :line    cur-line
                                :column  cur-col
                                :code    cur-code
                                :message cur-message
                                :context (nreverse context-lines))
                          diagnostics)
                    (setq in-error      nil
                          context-lines nil))))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (cond
             ;;
             ((string-match diag-re line)
              ;; New diagnostic - flush previous, start fresh.
              (flush-current)
              (setq in-error      t
                    cur-file      (match-string 1 line)
                    cur-line      (string-to-number (match-string 2 line))
                    context-lines nil)
              (if compile
                  ;; s7c: group 3 = column, group 4 = message, no code
                  (setq cur-col     (string-to-number (match-string 3 line))
                        cur-code    nil
                        cur-message (match-string 4 line))
                ;; s7check: group 3 = code, group 4 = message, no column
                (setq cur-col     nil
                      cur-code    (match-string 3 line)
                      cur-message (match-string 4 line))))
             ;;
             ;; Regexp to recognise (and discard) the s7c summary footer line.
             ;; e.g. "64 errors found" - must not be accumulated as context.
             ;; Only relevant for s7c output; s7check produces no such footer.
             ((and compile (string-match "^[0-9]+ errors? found$"  line))
              ;; s7c summary footer ("N errors found") - skip entirely.
              nil)
             ;;
             (in-error
              ;; Continuation / context line - accumulate non-blank
              (unless (string-blank-p line)
                (push line context-lines)))))
          (forward-line 1))
        ;; Flush the final diagnostic (if any).
        (flush-current))
      (nreverse diagnostics))))

(defun seed7--expand-args (args)
  "Return a copy of ARGS with any element whose name starts with `~' expanded.
Other elements are returned unchanged."
  (mapcar (lambda (arg)
            (if (string-prefix-p "~" arg)
                (expand-file-name arg)
              arg))
          args))

(defun seed7--run-and-parse (program args cmd-string compile file-name)
  "Run PROGRAM with ARGS and return (EXIT-CODE DIAGNOSTICS STDERR-TEXT).

CMD-STRING is the original command string (used verbatim in error messages).
COMPILE non-nil identifies a compilation operation (s7c); nil identifies
a static check (s7check).  When COMPILE is non-nil, STDERR-TEXT in the
result is always \"\", because s7c emits diagnostics on stderr and they
are parsed directly from the combined stdout+stderr stream.  When COMPILE
is nil, STDERR-TEXT holds any text the checker emitted on stderr.
FILE-NAME is the path of the source file passed to PROGRAM; it is also
used to set `default-directory' for the subprocess."
  (let* ((stdout-buf        (generate-new-buffer " *seed7-check-output*"))
         (stderr-buf        (unless compile
                              (generate-new-buffer " *seed7-check-stderr*")))
         (default-directory (file-name-directory (expand-file-name file-name)))
         stderr-text diagnostics exit-code)
    (unwind-protect
        ;; Run compiler/checker
        (progn
          (setq exit-code
                (seed7--run program args stdout-buf stderr-buf
                            (format "(from `%s' = %S)"
                                    (if compile "seed7-compiler" "seed7-checker")
                                    cmd-string)))
          (if compile
              (setq stderr-text "")
            (with-current-buffer stderr-buf
              (setq stderr-text (buffer-string))))
          (setq diagnostics
                (seed7--parse-diagnostics compile stdout-buf)))
      ;; Cleanup: always kill scratch buffers.
      (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf)))
    (list exit-code diagnostics stderr-text)))

(defun seed7--cmd-specs-for (user-option)
  "Return a (PROGRAM CMD-STRING CMD-PARTS) list for the specified USER-OPTION.

USER-OPTION is a symbol; the name of the user-option.

The returned PROGRAM is the absolute path of the executable file to execute.
Honors absolute, relative, and PATH names identified in the user-option,
including a leading ~ symbol to identify the home directory.

The returned CMD-PARTS is a list of strings of all arguments that follow the
program name.

Signals a user error if the absolute path of the program is not found."
  (let* ((user-option-name (symbol-name user-option))
         (cmd-string       (symbol-value user-option))
         (cmd-parts        (split-string-and-unquote cmd-string))
         (pgm-name         (car-safe cmd-parts))
         ;; Resolve the executable: when a directory component is present
         ;; (e.g. "~/bin/s7check"), expand ~ before searching exec-path
         ;; so tilde paths are honoured.
         (program-path  (and pgm-name
                             (if (file-name-directory pgm-name)
                                 (executable-find (expand-file-name pgm-name))
                               (executable-find pgm-name)))))
    (if program-path
        (list program-path cmd-string (cdr-safe cmd-parts))
      (user-error "Program specified by `%s' not found or not executable: %s"
                  user-option-name (or pgm-name "")))))

(defun seed7--end-msg-for (pgm operation n-diags stderr-text exit-code)
  "Return message describing result of execution of PGM.

OPERATION  : a descriptive name for the purpose of the PGM.
N-DIAGS    : the number of diagnostics/errors detected by PGM.
STDERR-TEXT: text printed by PGM on its stderr stream.
EXIT-CODE  : the exit code of PGM.
"
  (let ((err-msg     (when (and stderr-text
                                (not (string-blank-p stderr-text)))
                       (format "\nSTDERR: %s" stderr-text))))
    (format "%s: %s"
            pgm
            (cond
             ;; 1 or more error found.
             ((> n-diags 0)
              (format "%s: %d error%s found%s"
                      operation
                      n-diags
                      (seed7--plural-s n-diags)
                      (or err-msg "")))
             ;;
             ;; stderr message
             (err-msg (format "%s with exit code %d%s"
                              operation
                              exit-code
                              err-msg))
             ;;
             ;; no stderr message
             ((not (zerop exit-code))
              (format "%s with exit code %d"
                      operation
                      exit-code))
             ;;
             ;; no diags, no stderr, exit-code 0
             (t "no errors found")))))

(defun seed7-check-file (file-name &optional compile)
  "Run static check or compilation on FILE-NAME without using the shell.

If COMPILE is non-nil, use `seed7-compiler' (s7c) to compile the file;
otherwise use `seed7-checker' (s7check) to perform a static check.

Invokes the tool directly via `call-process' (no /bin/sh involved).
Returns a list (EXIT-CODE DIAGNOSTICS STDERR) where:
  EXIT-CODE   - integer exit status returned by the tool, or 1 when the
                process was terminated by a signal.
  DIAGNOSTICS - list (in source order) of plists, each with the keys:
    :file    - absolute source filename string
    :line    - line number as an integer
    :column  - column number as an integer, 1-based (s7c only; nil for s7check)
    :code    - symbolic error code string (s7check only, e.g. \"NO_MATCH\");
               nil when the compiler (s7c) is used
    :message - diagnostic message text string
    :context - list of continuation/context strings that followed the
               diagnostic line in the tool's output (may be nil).
  STDERR     - a string corresponding to the program stderr, or an empty
               string when COMPILE is non-nil (the compiler's stderr is
               merged into the parsed diagnostic stream instead).

The DIAGNOSTICS list is nil when no diagnostics are found.

The regexp used for matching depends on the tool:
  s7check: `seed7--checker-diagnostic-regexp'
  s7c:     `seed7--compiler-diagnostic-regexp'

Signals a `user-error' with an informative message if the executable
identified by `seed7-checker' (or `seed7-compiler' when COMPILE is
non-nil) cannot be found or is not executable.  In that case verify the
value of the corresponding user-option.

See also: `seed7-check-or-compile'."
  (let* ((cmd-specs  (seed7--cmd-specs-for (if compile
                                               'seed7-compiler
                                             'seed7-checker)))
         (program    (nth 0 cmd-specs))
         (cmd-string (nth 1 cmd-specs))
         (args       (nth 2 cmd-specs)))
    ;; Run compiler or checker and return results
    (seed7--run-and-parse program
                          (append (seed7--expand-args args)
                                  (list (expand-file-name file-name)))
                          cmd-string compile file-name)))

(defun seed7-check-or-compile (&optional compile)
  "Check or compile the current Seed7 buffer's file and show diagnostics.

When COMPILE is nil (the default), uses `seed7-checker' (s7check) for a
static check; no executable is generated.  When COMPILE is non-nil (or
called with \\[universal-argument]), uses `seed7-compiler' (s7c) to compile
and generate an executable if there are no errors.

Calls `seed7-check-file' internally (no shell, no /bin/sh).
Creates or reuses a `*seed7-errors*' buffer using `compilation-mode'.

The buffer is formatted to match a standard `*compilation*' buffer:
- A `-*- mode: compilation; default-directory: DIR -*-' file-variable header.
- GNU error lines: FILENAME:LINE: error: CODE: MESSAGE  (s7check; no column)
                   FILENAME:LINE:COL: error: MESSAGE    (s7c; no symbolic code)
  Both forms are navigable with \\[next-error] / \\[previous-error].
- Verbatim continuation lines between navigable entries.
- A `Compilation finished at DATE, duration N s' footer.
- Mode-line showing exit code and error/warning/info counts.

Returns the DIAGNOSTICS list from `seed7-check-file' (a list of plists;
see that function's docstring for the plist keys),
or nil when no diagnostics are found."
  (interactive "P")
  (let ((file (or buffer-file-truename buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (unless (eq major-mode 'seed7-mode)
      (user-error "%s is not a Seed7 buffer" (buffer-name)))
    ;;
    ;; -- Prompted save
    ;; The checker/compiler reads from file system; the buffer must be saved
    ;; before invoking it.  Ask the user rather than saving silently.
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Buffer %s is modified.  Save before %s? "
                            (buffer-name)
                            (if compile "compiling" "checking")))
          (save-buffer)
        (user-error "Aborted: buffer not saved")))
    ;;
    ;; -- Build command, execute and extract result information
    (let* ((cmd        (if compile seed7-compiler seed7-checker))
           (pgm        (file-name-nondirectory
                        (car (split-string-and-unquote cmd))))
           (operation  (if compile "compilation" "check"))
           (dir        (file-name-directory file))
           ;; Record time *around* the checker invocation
           (t0          (current-time))
           (result      (seed7-check-file file compile))
           (exit-code   (nth 0 result))
           (diags       (nth 1 result))
           (stderr-text (nth 2 result))
           (t1          (current-time))
           (duration    (float-time (time-subtract t1 t0)))
           (n-diags     (length diags))
           (out-buf     (get-buffer-create "*seed7-errors*"))
           (end-msg     (seed7--end-msg-for pgm operation
                                            n-diags stderr-text exit-code)))
      ;;
      ;; -- Format the output -----------------------------------------------
      (with-current-buffer out-buf
        (let ((inhibit-read-only t))
          (setq-local default-directory dir)
          (erase-buffer)
          ;; -- Header ------------------------------
          ;; Matches what compilation-start inserts; tells Emacs the
          ;; default-directory for resolving relative filenames.
          (insert (format "-*- mode: compilation; default-directory: %S -*-\n"
                          dir))
          (insert (format "%s %s\n\n" cmd file))
          ;; -- Diagnostics -------------------------
          (if diags
              (dolist (e diags)
                ;; Format the GNU-style anchor line according to the tool used.
                ;; compilation-mode recognises both FILENAME:LINE: and
                ;; FILENAME:LINE:COL: forms via its built-in `gnu' regexp entry.
                (insert
                 (if compile
                     ;; s7c: include column, no symbolic code
                     (format "%s:%d:%d: error: %s\n"
                             (plist-get e :file)
                             (plist-get e :line)
                             (or (plist-get e :column) 0) ; :column always non-nil for s7c; guard is defensive
                             (plist-get e :message))
                   ;; s7check: include symbolic code, no column
                   (format "%s:%d: error: %s: %s\n"
                           (plist-get e :file)
                           (plist-get e :line)
                           (plist-get e :code)
                           (plist-get e :message))))
                ;; Continuation / context lines verbatim.
                (dolist (ctx (plist-get e :context))
                  (insert ctx "\n"))
                ;; Blank line between diagnostics for readability.
                (insert "\n"))
            (insert end-msg))
          ;; -- Footer ------------------------------
          (insert (format "\nCompilation finished at %s, duration %.2f s\n"
                          (format-time-string "%a %b %e %H:%M:%S" t1)
                          duration)))
        ;; -- Activate compilation-mode AFTER inserting ---
        ;; This lets mode font-lock and regexp scanning run on the final text.
        (compilation-mode)
        ;; -- Mode-line: replicate what compilation-handle-exit produces if
        ;;   possible.
        ;;   NOTE: compilation-num-*-found are compile.el internals and older
        ;;   versions of Emacs may not have them.  If they are defined, then
        ;;   update the modeline, otherwise leave it alone.
        (when (boundp 'compilation-num-errors-found)
          (setq-local compilation-num-errors-found n-diags)
          ;; The current Seed7 tools emit no warning and no information hints.
          ;; If future versions of Seed7 emit them the following should be
          ;; extracted.
          (when (boundp 'compilation-num-warnings-found)
            (setq-local compilation-num-warnings-found 0))
          (when (boundp 'compilation-num-infos-found)
            (setq-local compilation-num-infos-found 0))
          (setq-local mode-line-process
                      (list
                       ;; ":exit [N]" - green when 0, red when non-zero
                       (propertize (format ":exit [%d]" exit-code)
                                   'face (if (zerop exit-code)
                                             'compilation-mode-line-exit
                                           'compilation-mode-line-fail))
                       " ["
                       ;; error count - red bold (compilation-mode-line-fail)
                       (propertize (number-to-string n-diags)
                                   'face 'compilation-mode-line-fail)
                       ;; warning count
                       " "
                       (propertize "0" 'face 'compilation-warning)
                       ;; info count - green bold (compilation-mode-line-exit)
                       " "
                       (propertize "0" 'face 'compilation-mode-line-exit)
                       "]")))
        (goto-char (point-min)))
      (display-buffer out-buf)
      ;; -- Show and return diagnostics count ----------------------
      (message "%s" end-msg)
      diags)))

;; ---------------------------------------------------------------------------
;;* Seed7 Run Program
;;  =================
;;
;; The `seed7-run-program' command first performs a static check using
;; `seed7-checker' (s7check).  If the code is clean it then launches the
;; program using `seed7-interpreter' (s7).
;;
;; stdout is displayed in real time in a `*seed7-run: BASENAME*' buffer.  The
;; buffer accepts input.
;;
;; In buffered mode, text is typed and edited normally at the end of the
;; buffer and sent to the Seed7 program by pressing the RET key.
;;
;; In raw mode every key (with the exception of C-c C-c and C-c C-j)
;; is sent directly to the Seed7 program.
;;
;; In both modes, C-c C-c interrupts the Seed7 program.
;;
;; stderr is captured in real time in a separate
;; `*seed7-run-stderr: BASENAME*' buffer.
;;
;;  * `seed7-run-program'
;;    . `seed7--run-program-filter'
;;    . `seed7--run-sentinel'
;;    . `seed7-run-send-input'
;;    . `seed7-run-interrupt'
;;    . `seed7-run-raw-send-key'
;;    * `seed7-run-mode'
;;  * `seed7-run-enter-raw-mode'
;;  * `seed7-run-exit-raw-mode'

;;** Seed7 Run – process filters and sentinel

(defun seed7--run-program-filter (proc string)
  "Process filter for `seed7-run-program': insert STRING into PROC's buffer.
Text is inserted at the process mark so that any user input typed at the
end of the buffer is not disturbed."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

(defun seed7--run-sentinel (proc event)
  "Sentinel for the Seed7 run process PROC; appends EVENT to its buffer."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n\nProcess %s %s"
                        (process-name proc)
                        (string-trim-right event)))))))

;;** Seed7 Run – interactive input commands

(defvar-local seed7-run--raw-mode nil
  "Non-nil when the `*seed7-run: BASENAME*' buffer is in raw-input mode.
In raw mode every character key press is forwarded directly to the
running Seed7 process without local buffering.
Switch modes with `seed7-run-enter-raw-mode' / `seed7-run-exit-raw-mode'.")

(defun seed7-run-send-input ()
  "Send the text typed after the last program output to the Seed7 process.
The text is taken from the current process mark to the end of the buffer.
A newline is appended automatically.
Bound to RET in `*seed7-run: BASENAME*' buffers."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (and proc (process-live-p proc))
      (user-error "No running Seed7 process in this buffer"))
    (let* ((input (buffer-substring-no-properties
                   (process-mark proc) (point-max)))
           (inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (set-marker (process-mark proc) (point))
      (process-send-string proc (concat input "\n")))))

(defun seed7-run-interrupt ()
  "Interrupt the Seed7 program running in the current buffer.
Sends SIGINT to the associated process.  Bound to C-c C-c in
`*seed7-run: BASENAME*' buffers."
  (interactive)
  (if-let ((proc (get-buffer-process (current-buffer))))
      (if (process-live-p proc)
          (progn
            (interrupt-process proc)
            (message "seed7: process interrupted"))
        (message "seed7: process is not running"))
    (message "seed7: no process associated with this buffer")))

(defun seed7-run-raw-send-key ()
  "Forward the key just pressed directly to the running Seed7 process.
This is the catch-all binding used in raw-input mode.
Non-character events (e.g. function keys, mouse events) are silently
ignored because there is no universal byte encoding for them."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (and proc (process-live-p proc))
      (user-error "No running Seed7 process in this buffer"))
    (if (characterp last-input-event)
        (process-send-string proc (char-to-string last-input-event))
      ;; Non-character events (arrows, F-keys, mouse, ...) cannot be
      ;; forwarded as raw bytes without a terminal-encoding layer.
      (message "seed7-run[RAW]: cannot forward non-character event: %s"
               (key-description (vector last-input-event))))))

;;** Seed7 Run – run-buffer major mode

(defvar seed7-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'seed7-run-send-input)
    (define-key map (kbd "C-c C-c") #'seed7-run-interrupt)
    (define-key map (kbd "C-c C-k") #'seed7-run-enter-raw-mode)
    map)
  "Keymap used in `*seed7-run: BASENAME*' output/input buffers.")

(defvar seed7-run-raw-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Catch-all: forward every key that is a character to the process.
    (define-key map [t]             #'seed7-run-raw-send-key)
    ;; Keep SIGINT reachable even in raw mode.
    (define-key map (kbd "C-c C-c") #'seed7-run-interrupt)
    ;; C-c C-j exits raw mode and returns to buffered mode.
    (define-key map (kbd "C-c C-j") #'seed7-run-exit-raw-mode)
    map)
  "Keymap used in `*seed7-run: BASENAME*' buffers while in raw-input mode.
Every key that is a character is forwarded directly to the running
Seed7 process.  Use \\[seed7-run-exit-raw-mode] (C-c C-j) to return
to buffered mode, or \\[seed7-run-interrupt] (C-c C-c) to send SIGINT.")

;; --

(defun seed7-run-enter-raw-mode ()
  "Switch the current `*seed7-run: BASENAME*' buffer to raw-input mode.
In raw mode every character key is sent immediately to the running
Seed7 process, bypassing Emacs editing.

  \\[seed7-run-interrupt] (C-c C-c) still sends SIGINT.
  \\[seed7-run-exit-raw-mode] (C-c C-j) returns to buffered mode."
  (interactive)
  (unless (derived-mode-p 'seed7-run-mode)
    (user-error "Not in a seed7-run buffer"))
  (setq seed7-run--raw-mode t)
  (setq mode-name "seed7-run[RAW]")
  (force-mode-line-update)
  (use-local-map seed7-run-raw-mode-map)
  (message "seed7-run: raw-input mode  (C-c C-j → buffered,  C-c C-c → SIGINT)"))

(defun seed7-run-exit-raw-mode ()
  "Return the current `*seed7-run: BASENAME*' buffer to buffered-input mode.
Type your input at the end of the buffer and press \\[seed7-run-send-input]
(RET) to send it.  Use \\[seed7-run-enter-raw-mode] (C-c C-k) to switch
back to raw mode."
  (interactive)
  (setq seed7-run--raw-mode nil)
  (setq mode-name "seed7-run")
  (force-mode-line-update)
  (use-local-map seed7-run-mode-map)
  (message "seed7-run: buffered-input mode  (RET → send,  C-c C-k → raw)"))

(define-derived-mode seed7-run-mode fundamental-mode "seed7-run"
  "Major mode for Seed7 program stdout/stdin buffers.

Program output is inserted at the process mark.

Buffered-input mode (default)
  Type input after the last output line and press
  \\[seed7-run-send-input] (RET) to send it to the running program.
  Press \\[seed7-run-enter-raw-mode] (C-c C-k) to switch to raw mode.

Raw-input mode
  Every character key is forwarded immediately to the running program.
  Press \\[seed7-run-exit-raw-mode] (C-c C-j) to return to buffered mode.

In both modes, \\[seed7-run-interrupt] (C-c C-c) sends SIGINT.

stderr output appears in the companion `*seed7-run-stderr: BASENAME*' buffer."
  (use-local-map seed7-run-mode-map)
  (setq-local scroll-conservatively 1000))

;; ---------------------------------------------------------------------------
;;** Seed7 Run – main command

(defun seed7-run-program (args)
  "Statically check, then run, the Seed7 program in the current buffer.

Prompts for ARGS: a string of space-separated arguments to pass to the
program (leave empty if none are required).

Step 1 – Static check
  Runs `seed7-checker' (s7check) on the visited file, exactly as
  `seed7-check-or-compile' does.  If any diagnostics are found they are
  displayed in `*seed7-errors*' and the run is aborted.

Step 2 – Interpreter launch
  If the code is clean, runs the program with `seed7-interpreter' (s7).

  stdout is shown in real time in `*seed7-run: BASENAME*'.
  The buffer accepts stdin: type at the end of the buffer and press
  \\[seed7-run-send-input] (RET) to send a line to the program.  Press
  \\[seed7-run-interrupt] (C-c C-c) to send SIGINT.

  stderr is captured in real time in `*seed7-run-stderr: BASENAME*'.

If a previous run process is still alive in the stdout buffer the
command asks whether to kill it before launching a new one.

See also: `seed7-check-or-compile', `seed7-interpreter'."
  (interactive
   (list (read-string "Program arguments (empty for none): ")))
  ;;
  ;; -- Validate current buffer
  (let ((file (or buffer-file-truename buffer-file-name)))
    (if file
        (setq file (expand-file-name file))
      (user-error "Buffer is not visiting a file"))
    (unless (eq major-mode 'seed7-mode)
      (user-error "%s is not a Seed7 buffer" (buffer-name)))
    ;;
    ;; -- Prompted save (checker reads from disk)
    (when (buffer-modified-p)
      (if (y-or-n-p (format "Buffer %s is modified.  Save before running? "
                            (buffer-name)))
          (save-buffer)
        (user-error "Aborted: buffer not saved")))
    ;;
    ;; -- Step 1: static check -----------------------------------------------
    (message "seed7: checking %s..." (file-name-nondirectory file))
    (let* ((check-result (seed7-check-file file nil)) ; nil = s7check, not s7c
           (exit-code    (nth 0 check-result))
           (diags        (nth 1 check-result))
           (stderr-text  (nth 2 check-result))
           (n-diags      (length diags))
           (dir          (file-name-directory file))
           (pgm          (file-name-nondirectory
                          (car (split-string-and-unquote seed7-checker)))))
      ;;
      (when (or diags (not (zerop exit-code)))
        ;; Build the errors buffer the same way seed7-check-or-compile does.
        (let ((end-msg (seed7--end-msg-for
                        pgm
                        (format "run %s" (file-name-nondirectory file))
                        n-diags stderr-text exit-code))
               (out-buf  (get-buffer-create "*seed7-errors*")))
          (with-current-buffer out-buf
            (let ((inhibit-read-only t))
              (setq-local default-directory dir)
              (erase-buffer)
              (insert (format "-*- mode: compilation; default-directory: %S -*-\n"
                              dir))
              (insert (format "%s %s\n\n" seed7-checker file))
              (if diags
                  (dolist (e diags)
                    (insert (format "%s:%d: error: %s: %s\n"
                                    (plist-get e :file)
                                    (plist-get e :line)
                                    (plist-get e :code)
                                    (plist-get e :message)))
                    (dolist (ctx (plist-get e :context))
                      (insert ctx "\n"))
                    (insert "\n"))
                (insert end-msg))
              (insert "\nCompilation finished\n"))
            (compilation-mode)
            (goto-char (point-min)))
          (display-buffer out-buf)
          (user-error "%s" end-msg)))
      ;;
      ;; -- Step 2: launch interpreter -----------------------------------------
      (let* ((basename   (file-name-nondirectory file))
             (buf-name   (format "*seed7-run: %s*" basename))
             (err-name   (format "*seed7-run-stderr: %s*" basename))
             (stdout-buf (get-buffer-create buf-name))
             (stderr-buf (get-buffer-create err-name))
             (cmd-specs (seed7--cmd-specs-for 'seed7-interpreter))
             (interp-pgm  (nth 0 cmd-specs))
             (interp-opts (nth 2 cmd-specs))
             (prog-args  (unless (string-blank-p args)
                           (split-string-and-unquote args)))
             (cmd-list   (append (list interp-pgm)
                                 interp-opts
                                 (list file)
                                 prog-args)))
        ;;
        ;; Kill any leftover process in the stdout buffer
        (let ((old (get-buffer-process stdout-buf)))
          (when old
            (when (process-live-p old)
              (if (y-or-n-p
                   (format "A Seed7 process is already running in %s.  Kill it? "
                           buf-name))
                  (delete-process old)
                (user-error "Aborted: existing process still running")))))
        ;;
        ;; Prepare stdout buffer
        (with-current-buffer stdout-buf
          (let ((inhibit-read-only t))
            (erase-buffer))
          (seed7-run-mode)
          (setq-local default-directory dir)
          (insert (format "Running: %s\n\n"
                          (mapconcat #'identity cmd-list " "))))
        ;;
        ;; Prepare stderr buffer
        (with-current-buffer stderr-buf
          (let ((inhibit-read-only t))
            (erase-buffer))
          (special-mode)
          (setq-local default-directory dir))
        ;;
        ;; Launch with separate stderr pipe
        (make-process
         :name     "seed7-run"
         :buffer   stdout-buf
         :command  cmd-list
         :filter   #'seed7--run-program-filter
         :sentinel #'seed7--run-sentinel
         :stderr   stderr-buf          ; Emacs routes stderr here in real time
         :noquery  t)
        ;;
        ;; Always show stdout and stderr in separate windows.
        ;; Display stdout first; then split its window to show stderr below,
        ;; unless stderr is already visible somewhere (avoids duplicate splits
        ;; on re-runs).
        (let ((stdout-win (display-buffer stdout-buf)))
          (unless (get-buffer-window stderr-buf)
            (when stdout-win
              (set-window-buffer
               (split-window stdout-win nil 'below)
               stderr-buf))))
        (message "seed7: running %s %s %s"
                 (file-name-nondirectory interp-pgm)
                 basename
                 (if prog-args (mapconcat #'identity prog-args " ") ""))))))

;; ---------------------------------------------------------------------------
;;* Seed7 Cross Reference
;;  =====================
;;
;; The `seed7-mode' supports the xref framework introduced in Emacs 25: the
;; code implements an xref-framework-compliant backend using the s7xref Seed7
;; program that parses Seed7 program files and library files to extract all
;; required information about global variables, functions and procedures which
;; includes all operators (both word and special operators).
;;
;; Therefore to use this feature you must have the Seed7 compiler and
;; interpreter installed on your system and the s7xref.sd7 file available in
;; the seed7-mode/tools directory installed.  You can use it as is with the s7
;; interpreter or compile it with the s7c compiler.  The `seed7-xref' user
;; option must identify the command used to execute the program.
;;
;; Once this is done, you will be able to use the various xref commands with
;; the xref front-end of your choice, the default being xref's own front-end,
;; but also with the helm, ivy or other xref front-ends.

(defvar-local seed7---xref-buffer nil
  "Internal/hidden buffer holding cross-reference info for visited Seed7 file.")

(defconst seed7--xref-line-re-fmt
  "^\\(%s\\)\t\\(.+?\\)\t\\([0-9]+\\)$"
  "Regexp format extracting the 3 elements of the s7xref output line.
Requires 1 format %s argument for the identifier.
- Group 1: identifier,
- Group 2: file name,
- Group 3: line number.")

(defconst seed7--xref-line-re
  (format seed7--xref-line-re-fmt
          seed7-name-identifier-nc-re)
  "Regexp to extract identifier for xref.")

(defun seed7--build-xref ()
  "Build a cross reference buffer for the current Seed7 file.
The buffer holds 1 line per object referenced.
Each line holds 3 tab-separated elements:
- object name,
- name of file where object is defined,
- line number in file where the object is defined.

This uses the Seed7 cross reference tool identified by the `seed7-xref'
user-option."
  (unless (or buffer-file-truename buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((xref-uo-list (split-string-and-unquote seed7-xref))
         (xref-cmd (car-safe xref-uo-list))
         (xref-executable-name (and
                                xref-cmd
                                (if (file-name-directory xref-cmd)
                                    (executable-find (expand-file-name xref-cmd))
                                  (executable-find xref-cmd)))))
    (if (and xref-executable-name
             (file-executable-p xref-executable-name))
        (let* ((sd7-source-fname-with-path
                (expand-file-name buffer-file-truename))
               (fbasename (file-name-sans-extension
                           (file-name-nondirectory
                            buffer-file-truename)))
               (outbuf (or (and seed7---xref-buffer
                                (buffer-live-p seed7---xref-buffer)
                                seed7---xref-buffer)
                           (setq-local
                            seed7---xref-buffer
                            (get-buffer-create
                             ;; Create a hidden buffer by using a leading
                             ;; space in its name.
                             (format " *s7xref-for-%s*" fbasename))))))
          ;; In case the command was executed before, erase prior content
          (with-current-buffer outbuf
            (erase-buffer))
          (let
              ((exit-code
                (if (eq (length xref-uo-list) 1)
                    ;; seed7-xref is just 1 word, the name of the xref program
                    (call-process xref-executable-name
                                  nil outbuf nil
                                  sd7-source-fname-with-path)
                  ;; seed7-xref has more than 1 word.  The first word is the
                  ;; program, and the following words are its arguments.  For
                  ;; instance the program name could be s7 and the next word a
                  ;; Seed7 source file to interpret.  And there could be other
                  ;; options.  Pass them all to the `call-process' as args.
                  ;;
                  ;; Since we do not use the shell, the file paths *must* be
                  ;; expanded.
                  (let ((args (list sd7-source-fname-with-path)))
                    (if (string= (file-name-nondirectory
                                  (car xref-uo-list))
                                 "s7")
                        ;; If Seed7 interpreter is used, ensure that the
                        ;; second element is a fully expanded file name that
                        ;; exists.  If it exists prepend the fully expanded
                        ;; file name to args.
                        (progn
                          (setq xref-uo-list (cdr xref-uo-list))
                          (let ((script (car-safe xref-uo-list)))
                            (unless script
                              (user-error "\
Invalid seed7-xref: No Seed7 file identified after s7 interpreter.
seed7-xref = %s"
                                          seed7-xref))
                            (let ((sd7-script-fn
                                   (expand-file-name script)))
                              (if (and sd7-script-fn
                                       (file-exists-p sd7-script-fn))
                                  (progn
                                    ;; remove script filename from the list
                                    (setq xref-uo-list (cdr-safe xref-uo-list))
                                    ;; prepend scripts args if any
                                    (when xref-uo-list
                                      (setq args (append xref-uo-list args)))
                                    ;; then prepend script file-name
                                    (setq args
                                          (append (list sd7-script-fn) args)))
                                (user-error
                                 "Invalid seed7-xref: %s\nseed7-xref = %s"
                                 (format "Can't find script: %s"
                                         sd7-script-fn)
                                 seed7-xref)))))
                      ;; The command is not a s7 command but something else.
                      ;; We know the first word is a valid executable, just
                      ;; proceed by prepending the cdr of xref-uo-list.
                      (when (> (length xref-uo-list) 1)
                        (setq args (append (cdr xref-uo-list) args))))
                    ;; Execute the command with appropriate arguments.
                    (apply #'call-process
                           xref-executable-name
                           nil outbuf nil
                           args)))))
            (cond
             ((not (integerp exit-code))
              (user-error
               "seed7-xref tool was terminated: %s.\nCommand: %s\nOutput:\n%s"
               exit-code
               seed7-xref
               (with-current-buffer outbuf (buffer-string))))
             ((not (zerop exit-code))
              (user-error
               "seed7-xref tool exited with status %d.\nCommand: %s\nOutput:\n%s"
               exit-code
               seed7-xref
               (with-current-buffer outbuf (buffer-string))))
             (t
              (when (buffer-modified-p)
                (message "\
seed7-xref: buffer has unsaved changes; xref results may be stale"))))))
      (user-error "\
The seed7-xref user-option does not identify an executable file: %s
Please update!"
                  seed7-xref))))


;; [ TODO 2025-06-20, by Pierre Rouleau: Add support for multi-line signatures]
(defun seed7--signature-at (&optional pos)
  "Return Seed7 element signature for element at point or POS."
  (when pos (goto-char pos))
  (substring-no-properties
   (buffer-substring (line-beginning-position) (line-end-position)))

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
  (condition-case err
      (if (and (stringp filename)
               (integerp line)
               (> line 0)
               (integerp column)
               (>= column 0))
          (with-temp-buffer
            (insert-file-contents filename)
            (goto-char (point-min))
            (forward-line (1- line))
            (forward-char column)
            (seed7--signature-at))
        (format "<xref: invalid location %S:%S:%S>"
                filename line column))
    (error
     (format "<xref: cannot read signature at %s:%S:%S: %s>"
             filename line column (error-message-string err)))))

(defun seed7--xref-in-list (entries filename lineno)
  "Return t if FILENAME @ LINENO is inside list of ENTRIES.
Return nil otherwise."
  (declare (pure t) (side-effect-free t))
  (let ((found nil)
        (entry (car-safe entries)))
    (while (and entry
                (not found))
      (when (and (string= (nth 0 entry) filename)
                 (eq (nth 1 entry) lineno))
        (setq found t))
      (setq entries (cdr-safe entries))
      (setq entry (car-safe entries)))
    found))

(defun seed7--symbol-definition-areas-for-block (block-spec)
  "Return a list of definition area (START-POS . END-POS) cons cells.

Each position pair identifies an area inside the block where a Seed7 variable
or parameter can be defined.
BLOCK-SPEC identifies the function/procedure block.  It is a list returned
by `seed7-line-inside-a-block'."
  (save-excursion
    (let ((block-start-pos          (nth 2 block-spec))
          (enclosing-block-end-pos  (nth 3 block-spec))
          (area-start nil)
          (area-end   nil)
          (areas nil))
      ;;
      ;; identify parameter area
      (goto-char block-start-pos)
      (when (seed7-re-search-forward "(" enclosing-block-end-pos)
        (setq area-start (point))
        (backward-char)
        (seed7--with-forward-sexp
          (setq area-end (1- (point)))
          (push (cons area-start area-end) areas)))
      ;;
      ;; Identify variable definition area: local-begin or result-begin block
      (goto-char block-start-pos)
      ;; look for local-begin or result-begin block
      (when (seed7-re-search-forward
             "^[[:blank:]]+\\(?:local\\>\\|result\\>\\)"
             enclosing-block-end-pos)
        (forward-line 1)
        (setq area-start (point))
        (when (seed7-re-search-forward "^[[:blank:]]+begin\\>"
                                       enclosing-block-end-pos)
          (forward-line 0)
          (setq area-end (point))
          (push (cons area-start area-end) areas)))
      ;;
      ;; return the list of cons
      areas)))


(defun seed7--find-identifier-in (identifier from-line
                                             &optional block-spec
                                             start-pos end-pos)
  "Return position info list of IDENTIFIER inside specified constraints.
Start looking at line identified by FROM-LINE.
The constraints are optional: BLOCK-SPEC, START-POS and END-POS.
If these are not specified, perform a file global scope search.

Return a list of 4-elements:
- The file name where this identifier entry was found.
- The line number integer,
- The column number integer
- A description string (the signature, if found)."
  (save-excursion
    (goto-char (or start-pos (point-min)))
    ;; search for a isolated identifier (don't match partial identifiers)
    (when (seed7-re-search-forward
           (format "\\<%s\\>" (regexp-quote identifier)) end-pos)
      (let ((specs nil)
            (found-lineno (seed7-current-line-number)))
        (when (and (not (eq from-line found-lineno))
                   (or (seed7--set (seed7-line-inside-a-block 0)
                                   specs)
                       (not block-spec)))
          (list (expand-file-name (or buffer-file-truename
                                      buffer-file-name
                                      ""))
                found-lineno
                (- (current-column) (length identifier))
                (seed7--signature-at
                 (if block-spec
                     ;; inside-block definition
                     (nth 2 specs)
                   ;; global definition
                   (progn (forward-line 0) (point))))))))))


(defun seed7--find-candidates-for (identifier from-line
                                              &optional block-spec)
  "Find information about IDENTIFIER, globally or inside BLOCK-SPEC.
Start looking at line specified by FROM-LINE.
Return a list of 4-element lists, where each 4-element list has:
- The file name where this identifier entry was found.
- The line number integer,
- The column number integer
- A description string (the signature, if found).

If nothing is found it returns nil.

This function is used only when the IDENTIFIER is not identified in the output
of s7xref program."
  (save-excursion
    (let ((candidate nil)
          (candidates nil))
      (if block-spec
          ;; with a block spec: check into all possible areas for the block
          (dolist (start.end (seed7--symbol-definition-areas-for-block
                              block-spec)
                             (seq-filter #'identity candidates))
            (when (seed7--set (seed7--find-identifier-in identifier from-line
                                                         block-spec
                                                         (car start.end)
                                                         (cdr start.end))
                              candidate)
              (push candidate candidates)))
        ;; without a block spec: check in the entire file
        (seq-filter #'identity
                    (list (seed7--find-identifier-in identifier from-line)))))))


(defun seed7--xref-get-from-s7xref (identifier from-line)
  "Get a list of all entries matching IDENTIFIER literally using the s7xref.
Start looking at line specified by FROM-LINE.
Return a list of 4-element lists, where each 4-element list has:
- 0: The file name where this identifier entry was found.
- 1: The line number integer,
- 2: The column number integer
- 3: A description string (the signature, if found, otherwise a replacement)."
  ;; Build the xref list for this Seed7 file if it does not already exist.
  ;; Store it inside an internal buffer `seed7---xref-buffer'.
  (unless (and seed7---xref-buffer
               (buffer-live-p seed7---xref-buffer))
    (seed7--build-xref))
  ;; Then search the identifier references in the `seed7---xref-buffer'.
  ;; There may be several entries for a specific identifier, and some of them
  ;; may be duplicated.  Filter duplicates and return all candidates.
  ;; Each line of the xref buffer holds a tab-separated set of 3 values: the
  ;; identifier string, the file name and the line number.
  (let* ((entries nil)
         (keep-searching t)
         ;; Capture buffer-file-truename and default-directory from the source
         ;; buffer now, before `with-current-buffer' switches context to the
         ;; internal xref buffer.
         (source-file-truename buffer-file-truename)
         (expanded-source-fname (expand-file-name source-file-truename))
         (source-default-directory default-directory)
         (text-re (format seed7--xref-line-re-fmt (regexp-quote identifier))))
    (with-current-buffer seed7---xref-buffer
      (goto-char (point-min))
      (while (and keep-searching
                  (not (eobp)))
        (if (re-search-forward text-re nil :noerror)
            (let* ((xref-identifier (match-string 1))
                   (raw-filename    (match-string 2))
                   (filename (expand-file-name raw-filename source-default-directory))
                   (lineno (string-to-number (match-string 3))))
              (unless (or (and source-file-truename ; guard: nil if buffer is unsaved
                               (string= filename expanded-source-fname)
                               (= lineno from-line))
                          (seed7--xref-in-list entries filename lineno))
                (push (list
                       filename
                       lineno
                       0   ; column not identified by s7xref: use 0
                       (or (seed7--signature-from filename lineno 0)
                           (format "No signature for: %s" xref-identifier)))
                      entries)))
          (setq keep-searching nil))))
    entries))

(defun seed7-point-on-defined-identifier-p (&optional pos)
  "Return non-nil if identifier at point or POS is being defined, nil otherwise."
  (save-excursion
    (when pos
      (goto-char pos))
    (forward-word-strictly 2)
    (backward-word-strictly)
    (looking-at-p "is\\>")))

(defun seed7--xref-get (identifier)
  "Get a list of all entries matching IDENTIFIER literally.
Return a list of 4-element lists, where each 4-element list has:
- 0: The file name where this identifier entry was found.
- 1: The line number integer,
- 2: The column number integer
- 3: A description string (the signature, if found, otherwise a replacement)."
  (let ((current-lineno (seed7-current-line-number))
        (point-face (get-char-property (point) 'face))
        (local-block-spec (save-excursion (seed7-to-top-of-block)
                                          (seed7-line-inside-a-block 0)))
        (candidates nil)
        ;; prevent case fold searching: Seed7 is case sensitive.
        (case-fold-search nil))
    (cond
     ((seed7-inside-comment-p)
      (user-error "Comments cross reference is not supported!"))
     ((seed7-inside-string-p)
      (user-error "This is a string: no reference available"))
     ((memq point-face '(seed7-float-face
                         seed7-integer-face
                         seed7-big-integer-face))
      (user-error "This is a number: no reference available!"))

     ;; For identifiers, look in local block first, then in the s7xref built
     ;; table and then in the global scope of the current file.
     ((eq point-face 'seed7-name-identifier-face)
      (if (seed7-point-on-defined-identifier-p)
          (user-error "%S is defined here!" identifier)
        (if (seed7--set
             (seed7--find-candidates-for identifier
                                         current-lineno
                                         local-block-spec)
             candidates)
            candidates
          ;; if nothing found in current block, search at program scope
          ;; using the cross reference list of object created by s7xref
          (or (seed7--xref-get-from-s7xref identifier current-lineno)
              ;; if that also fails to find something, then look into the global
              ;; scope of the current file.
              (seed7--find-candidates-for identifier current-lineno)))))
     ;; for other keywords only look into the xref extracted by s7xref
     (t (seed7--xref-get-from-s7xref identifier current-lineno)))))

;; [ TODO 2025-06-13, by Pierre Rouleau: Should we also skip parens?.]
(defun seed7-operator-at-point ()
  "Return the Seed7 operator at point as a string."
  (save-excursion
    (let ((c nil)
          (start-pos nil))
      ;; move point to previous whitespace
      (while (and (not (bobp))
                  (not (eq (setq c (preceding-char)) ?\s))
                  (not (eq c ?\t))
                  (not (eq c ?\n)))
        (backward-char))
      (setq start-pos (point))
      ;; move point to next whitespace
      (while (and (not (eobp))
                  (not (eq (setq c (following-char)) ?\s))
                  (not (eq c ?\t))
                  (not (eq c ?\n)))
        (forward-char))
      (substring-no-properties (buffer-substring start-pos (point))))))

(defun seed7-symbol-at-point ()
  "Return the Seed7 token at point as a string.

Return value is determined by matching the character at point in
priority order:

1. A predefined assignment operator (e.g. `:='), if one starts at point.
2. An arithmetic operator string (capture group 1), if one starts at point.
3. A single-character string, if point is at a \"very special\" punctuation
   character (specified in `seed7--very-special-char-re').
4. The operator token spanning point, via `seed7-operator-at-point', if point
   is at a character matched by `seed7--special-char-re'.
5. The word symbol at point, as returned by `thing-at-point' with DEREF t.
6. If `thing-at-point' returns nil (e.g. point is in whitespace or at a buffer
   boundary), falls back to `seed7-operator-at-point'.

This implements `xref-backend-identifier-at-point' for Seed7 buffers."
  (cond
   ;;
   ((looking-at seed7-predef-assignment-operator-regexp)
    (substring-no-properties (match-string 0)))
   ;;
   ((looking-at seed7-arithmetic-operator-regexp)
    (substring-no-properties (match-string 1)))
   ;;
   ((looking-at-p seed7--very-special-char-re)
    (string (char-after)))
   ;;
   ((looking-at-p seed7--special-char-re)
    (seed7-operator-at-point))
   ;;
   (t (or (thing-at-point 'symbol t)
          (seed7-operator-at-point)))))

(defun seed7--make-xref-from-file-loc (elems)
  "Create an xref object pointing to the given file location.

ELEMS is a list (FILE LINE COLUMN DESC), where DESC describes the xref
target."
  (declare (side-effect-free t))
  (xref-make (nth 3 elems)              ; desc
             (xref-make-file-location (nth 0 elems)
                                      (nth 1 elems)
                                      (nth 2 elems))))

(defun seed7--find-symbol (symbol)
  "Get list of xref locations objects for Seed7 SYMBOL."
  ;; First get a list of candidates using the s7xref mechanism for symbol
  ;; regardless of what symbol is.  We may search something that is hard
  ;; coded, but Seed7 may also evolve over time, so don't take any chance and
  ;; check if the symbol is referred somewhere first.
  (let ((candidates (seq-filter #'identity (seed7--xref-get symbol))))
    (if candidates
        ;; return a list of xref location objects for those candidates.
        (mapcar (function seed7--make-xref-from-file-loc)
                candidates)
      ;; If nothing is found then check if symbol is something we expect to
      ;; not have a reference and issue an appropriate error.
      (save-match-data
        (cond
         ((or (string= symbol "")
              (string-match "[[:blank:]\n]" symbol))
          (user-error "Point is at white space character!"))
         ;;
         ((member symbol seed7--compile-time-symbols)
          (user-error "%S is a Seed7 compile-time symbol" symbol))
         ;;
         ((string-match seed7--very-special-char-re symbol)
          (user-error
           "%S is part of another Seed7 statement, hard-coded\
 or a Seed7 compile time symbol.?" symbol))
         ;;
         (t
          (user-error "Nothing matching %S here.
Is point at its definition? Is this file compiling?"
                      symbol)))))))

;;** Seed7 Cross Reference Xref Backend Framework

(defun seed7--xref-backend ()
  "Use the Seed7 backend for Xref in `seed7-mode' files."
  'seed7)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql seed7)))
  "Return symbol at point."
  (seed7-symbol-at-point))


(cl-defmethod xref-backend-definitions ((_backend (eql seed7)) symbol)
  "Return definition of SYMBOL."
  (seed7--find-symbol symbol))

;; [ TODO 2025-06-16, by Pierre Rouleau: Complete xref support]
;; (cl-defmethod xref-backend-references ((_backend (eql seed7)) symbol)
;;   (seed7--find-uses-of symbol symbol))
;;
;; (cl-defmethod xref-backend-apropos ((_backend (eql seed7)) symbol)
;;   (seed7-find-symbol-apropos symbol))
;;

(defun seed7--invalidate-xref-cache ()
  "Invalidate the buffer-local Seed7 xref cache for the current buffer.
Called from `after-save-hook' and `after-revert-hook' to ensure stale
data is not used for the next xref lookup."
  (when (buffer-live-p seed7---xref-buffer)
    (kill-buffer seed7---xref-buffer))
  (setq-local seed7---xref-buffer nil))

;; ---------------------------------------------------------------------------
;;* Seed7 Completion Support
;;  ========================
;; [ TODO 2025-06-07, by Pierre Rouleau: add completion support]
;; (defun seed7-completions-at-point)


(defun seed7--xref-identifiers ()
  "Return list of all identifiers extracted by s7xref for current file.
The list has no duplicate and is unsorted."
  ;; Build the xref list for this Seed7 file if it does not already exist.
  ;; Store it inside an internal buffer `seed7---xref-buffer'.
  (unless (and seed7---xref-buffer
               (buffer-live-p seed7---xref-buffer))
    (seed7--build-xref))
  (let ((identifiers nil)
        (identifier nil))
    (with-current-buffer seed7---xref-buffer
      (goto-char (point-min))
      (while (re-search-forward seed7--xref-line-re nil :noerror)
        (setq identifier (match-string 1))
        (unless (member identifier identifiers)
          (push identifier identifiers))))
    identifiers))

(defun seed7--procfun-identifiers ()
  "Return (unsorted) list of all identifiers of current function or procedure."
  (save-excursion
    (let ((identifiers nil)
          (callable-start nil)
          (callable-end nil)
          (param-var-end nil))
      (seed7--to-top)
      (setq callable-start (point))
      (seed7-end-of-defun 1 :silent :dont-push-mark)
      (setq callable-end (point))
      (goto-char callable-start)
      (setq param-var-end
            (if (seed7-re-search-forward "^[[:blank:]]+begin\\>" callable-end)
                (point)
              callable-end))
      (dolist (start.end (seed7--symbol-definition-areas-for-block
                          (list 0 "" callable-start param-var-end))
                         identifiers)
        (goto-char (car start.end))
        (while (seed7-re-search-forward seed7-parameter-var-declaration-regexp
                                        (cdr start.end))
          (push (substring-no-properties (match-string 1)) identifiers))))))

(defun seed7--list-of-terms ()
  "Return sorted list of identifiers available from the current location."
  (sort (append (seed7--xref-identifiers) (seed7--procfun-identifiers))
        (function string<)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql seed7)))
  "Return a list of terms for completions taken from the current buffer."
  (seed7--list-of-terms))

;; ---------------------------------------------------------------------------
;;* Seed7 Abbreviation Support
;;  ==========================

(defcustom seed7-support-abbrev-mode t
  "When non-nil, install Seed7's local abbrev table in seed7-mode buffers.

This does not enable `abbrev-mode'; enable `abbrev-mode' separately to expand
abbreviations.

NOTE: After changing in a session execute `seed7-rebuild-abbrev-table'
to activate the change."
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
                                 (";l"    "listener")
                                 (";ob"   "object")
                                 (";pd"   "pollData")
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
  "List of Seed7-specific abbrev/expansion pairs.

These abbreviations are made available to the `abbrev-mode' when the
`seed7-support-abbrev-mode' user option is on.

The list included here corresponds to what is documented.
Each entry shows the `abbrev' and its expanded text.

You can add, delete or modify any of these.

All built-in abbreviations use \";\" as a prefix (e.g. \";ra\" for \"raise\").
This prefix prevents accidental expansion of bare Seed7 keywords while you
type: an abbreviation like \"and\" would expand every time you type that word.
If you add custom entries, it is strongly recommended to use the \";\" prefix
for the same reason.  The commented-out operator-symbol entries at the end of
the default list are intentionally left without the prefix to show what they
would look like, but they are disabled because bare-word abbreviations expand
too aggressively in normal editing.

Make sure you have no duplication of abbreviation keys if you edit the list.

NOTE: After changing in a session execute `seed7-rebuild-abbrev-table'
to activate the change."
  :group 'seed7
  :type '(repeat
          (list
           (string :tag "abbrev")
           (string :tag "expand"))))

(defvar seed7-mode-abbrev-table nil
  "Abbrev table in use in Seed7 mode buffers.")

(defun seed7-rebuild-abbrev-table (&optional quietly)
  "Rebuild the Seed7 abbrev table from the current option values.

Print a completion message at the end unless optional QUIETLY argument
is non-nil.

Validates `seed7-abbreviations' before building the table:
- Entries that are not a list of exactly 2 strings are skipped with a warning.
- Entries whose abbrev key is an empty string are skipped with a warning.
- Duplicate abbrev keys are skipped (first occurrence wins) with a warning.

Warnings are emitted via `display-warning' with category \\='seed7 and
can be reviewed in the *Warnings* buffer.

Call this command after customizing `seed7-support-abbrev-mode' or
`seed7-abbreviations' if you want the changes to take effect in the
current Emacs session without restarting Emacs."
  (interactive)
  (let ((abbrevs-changed abbrevs-changed)) ; prevent spurious "Save abbrevs?" prompt
    (when (abbrev-table-p seed7-mode-abbrev-table)
      (clear-abbrev-table seed7-mode-abbrev-table))
    (when seed7-support-abbrev-mode
      (unless (abbrev-table-p seed7-mode-abbrev-table)
        (define-abbrev-table 'seed7-mode-abbrev-table nil
          "Abbrev table for Seed7 mode."
          :regexp "\\(?:[^[:word:]_;]\\|^\\)\\(;?[[:word:]_]+\\)[^[:word:]_]*"))
      ;; --- Validate entries and collect clean ones -------------------------
      (let ((seen-abbrevs (make-hash-table :test #'equal))
            (entry-number 0)
            (invalid-count 0)
            (duplicate-count 0))
        (dolist (entry seed7-abbreviations)
          (setq entry-number (1+ entry-number))
          (cond
           ;; Invalid: not a list of exactly 2 elements, both strings.
           ((not (and (listp entry)
                      (= (length entry) 2)
                      (stringp (car entry))
                      (stringp (cadr entry))))
            (setq invalid-count (1+ invalid-count))
            (display-warning
             'seed7
             (format "seed7-abbreviations entry #%d is invalid and was skipped: %S\n\
  Expected a list of 2 strings: (\"abbrev\" \"expansion\")."
                     entry-number entry)
             :warning))
           ;; Invalid: empty abbrev key.
           ((string-empty-p (car entry))
            (setq invalid-count (1+ invalid-count))
            (display-warning
             'seed7
             (format "seed7-abbreviations entry #%d has an empty abbrev key and was skipped: %S."
                     entry-number entry)
             :warning))
           ;; Duplicate: same abbrev key was already registered.
           ((gethash (car entry) seen-abbrevs)
            (setq duplicate-count (1+ duplicate-count))
            (display-warning
             'seed7
             (format "seed7-abbreviations entry #%d is a duplicate abbrev \"%s\" and was skipped.\n\
  First occurrence expands to: \"%s\"."
                     entry-number
                     (car entry)
                     (gethash (car entry) seen-abbrevs))
             :warning))
           ;; Valid: register the abbrev.
           (t
            (puthash (car entry) (cadr entry) seen-abbrevs)
            (define-abbrev seed7-mode-abbrev-table
              (car entry) (cadr entry) nil :system t))))
        ;; --- Summary message -----------------------------------------------
        (let ((registered (hash-table-count seen-abbrevs)))
          (if (or (> invalid-count 0) (> duplicate-count 0))
              (display-warning
               'seed7
               (format "seed7-mode abbrev table rebuilt: %d registered, %d invalid, %d duplicate%s skipped.\n\
  See individual warnings above for details. Run M-x seed7-rebuild-abbrev-table after fixing."
                       registered
                       invalid-count
                       duplicate-count
                       (seed7--plural-s (+ invalid-count duplicate-count)))
               :warning)
            (unless quietly
              (message "seed7-mode: abbrev table rebuilt: %d abbreviation%s registered."
                       registered
                       (seed7--plural-s registered)))))))))

;; Build the table once at package load time using the initial values.
(seed7-rebuild-abbrev-table 'quietly)

;; ---------------------------------------------------------------------------
;;* Seed7 Entity Browser Mode
;;  =========================
;;
;; A special mode that lists all Seed7 elements defined in a Seed7 buffer and
;; allows moving to anyone by typing RET on its line.

(defun seed7-buffer-entities ()
  "Return all named entities declared in the current Seed7 buffer.

Each element of the returned list is a list (TYPE NAME LINE-NUMBER) where:
- TYPE        is a symbol: `function', `procedure', `structure',
              `enumeration', `array', or `set'.
- NAME        is the entity name as a string.
- LINE-NUMBER is the line number (integer ≥ 1) of the declaration.

The list is sorted by LINE-NUMBER in ascending order.
Only code outside comments and strings is examined
\(via `seed7-re-search-forward').

This function does not move point (all searches use `save-excursion')."
  (let ((entities '()))
    ;; -- Functions and procedures ------------------------------
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward seed7-procfunc-regexp)
        (let ((raw-type (match-string-no-properties
                         seed7-procfunc-regexp-item-type-group))
              (name     (match-string-no-properties
                         seed7-procfunc-regexp-item-name-group)))
          (when name
            (push (list (if (string= raw-type "proc") 'procedure 'function)
                        name
                        (line-number-at-pos
                         (match-beginning seed7-procfunc-regexp-item-name-group)))
                  entities)))))
    ;; -- Enumerations ------------------------------------------
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward seed7-enum-regexp-4imenu)
        (push (list 'enumeration
                    (match-string-no-properties 1)
                    (line-number-at-pos (match-beginning 1)))
              entities)))
    ;; -- Structures --------------------------------------------
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward seed7-struct-regexp-4imenu)
        (push (list 'structure
                    (match-string-no-properties 1)
                    (line-number-at-pos (match-beginning 1)))
              entities)))
    ;; -- Arrays ------------------------------------------------
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward seed7--array-definition-name-regexp)
        (push (list 'array
                    (match-string-no-properties 1)
                    (line-number-at-pos (match-beginning 1)))
              entities)))
    ;; -- Sets --------------------------------------------------
    (save-excursion
      (goto-char (point-min))
      (while (seed7-re-search-forward seed7--set-definition-name-regexp)
        (push (list 'set
                    (match-string-no-properties 1)
                    (line-number-at-pos (match-beginning 1)))
              entities)))
    ;; -- Sort by line number and return-------------------------
    (sort entities (lambda (a b) (< (nth 2 a) (nth 2 b))))))


;;** Seed7 entity browser (tabulated-list-mode)
;;   ------------------------------------------

(defvar-local sd7-ent--source-buffer nil
  "The Seed7 source buffer that this entity browser was built from.")

(defvar-local sd7-ent--source-file nil
  "File path of the Seed7 source buffer; used when the buffer is killed.")

(defvar seed7-entities-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'sd7-ent--visit-entity)
    (define-key map (kbd "o")   #'sd7-ent--visit-entity-other-window)
    (define-key map (kbd "g")   #'sd7-ent--refresh)
    map)
  "Keymap for `seed7-entities-mode'.

\\{seed7-entities-mode-map}")

(define-derived-mode seed7-entities-mode tabulated-list-mode "Seed7-Entities"
  "Major mode for browsing named entities declared in a Seed7 buffer.

Each row shows the line number, entity type, and name.
\\<seed7-entities-mode-map>
\\[sd7-ent--visit-entity] Go to the declaration in the source buffer.
\\[sd7-ent--visit-entity-other-window]   Go to the declaration in the other window.
\\[sd7-ent--refresh]   Re-scan the source buffer and redisplay.
\\[tabulated-list-sort]   Sort by the column at point.

The entry ID of each row is (SOURCE-BUFFER . LINE-NUMBER), so the browser
always navigates to the exact line regardless of how the table is sorted."
  (setq tabulated-list-format
        (vector
         (list "Line" 6
               (lambda (a b)
                 ;; Sort numerically on the LINE column (column 0).
                 (< (string-to-number (aref (cadr a) 0))
                    (string-to-number (aref (cadr b) 0)))))
         (list "Type" 13 t)
         (list "Name" 50 t)))
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;** Seed7 entity browser helpers
;;   ----------------------------

(defun sd7-ent--entry-id ()
  "Return the entry-id of the row at point, or signal an error."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No entity at point"))
    id))

(defun sd7-ent--resolve-source (entry-id)
  "Return a live buffer for ENTRY-ID = (SOURCE-BUFFER . LINE).
If the originally recorded buffer is no longer live, try to re-visit
`sd7-ent--source-file'.  Signals `user-error' if neither is possible."
  (let* ((src-buf  (car entry-id))
         (file     sd7-ent--source-file))
    (cond
     ;; Original buffer still live.
     ((buffer-live-p src-buf) src-buf)
     ;; Buffer killed but we have a file path — re-visit it.
     ((and file (file-readable-p file))
      (find-file-noselect file))
     (t
      (user-error "Source buffer is no longer available")))))

(defun sd7-ent--goto-line-in-buffer (buf line-no)
  "Switch to BUF and move point to LINE-NO."
  (switch-to-buffer buf)
  (goto-char (point-min))
  (forward-line (1- line-no))
  (recenter))

(defun sd7-ent--goto-line-in-buffer-other-window (buf line-no)
  "Display BUF in the other window and move point to LINE-NO."
  (switch-to-buffer-other-window buf)
  (goto-char (point-min))
  (forward-line (1- line-no))
  (recenter))

;;** Seed7 entity browser interactive commands

(defun sd7-ent--visit-entity ()
  "Go to the declaration of the entity on the current line.
Switches to the source Seed7 buffer and moves point to the declaration line."
  (interactive)
  (let* ((id      (sd7-ent--entry-id))
         (line-no (cdr id))
         (src-buf (sd7-ent--resolve-source id)))
    (sd7-ent--goto-line-in-buffer src-buf line-no)))

(defun sd7-ent--visit-entity-other-window ()
  "Go to the declaration of the entity, displayed in the other window."
  (interactive)
  (let* ((id      (sd7-ent--entry-id))
         (line-no (cdr id))
         (src-buf (sd7-ent--resolve-source id)))
    (sd7-ent--goto-line-in-buffer-other-window src-buf line-no)))

(defun sd7-ent--refresh ()
  "Re-scan the source buffer and redisplay the entity list."
  (interactive)
  (let ((src-buf (sd7-ent--resolve-source (cons sd7-ent--source-buffer 1))))
    (with-current-buffer src-buf
      (seed7-list-entities))))

;;** Seed7 entity browser helper - public entry point
;;   ------------------------------------------------

;;;###autoload
(defun seed7-list-entities ()
  "Display all named entities in the current Seed7 buffer in a browser window.

Builds or refreshes a `seed7-entities-mode' buffer with one row per entity
\(functions, procedures, structures, enumerations, arrays, sets).

Keys in the browser:
  RET   — go to the declaration (replaces current window).
  o     — go to the declaration in the other window.
  g     — refresh the browser from the source buffer.
  S     — sort by the column at point (`tabulated-list-sort').

The browser name is  *Seed7 Entities: BUFFER-NAME*."
  (interactive)
  (unless (derived-mode-p 'seed7-mode)
    (user-error "Current buffer is not in seed7-mode"))
  (let* ((source-buf  (current-buffer))
         (source-file (buffer-file-name))
         (entities    (seed7-buffer-entities))
         (bname       (format "*Seed7 Entities: %s*" (buffer-name source-buf)))
         (browser-buf (get-buffer-create bname)))
    (with-current-buffer browser-buf
      (seed7-entities-mode)
      (setq sd7-ent--source-buffer source-buf
            sd7-ent--source-file   source-file)
      (setq tabulated-list-entries
            (mapcar (lambda (e)
                      (let ((type (nth 0 e))
                            (name (nth 1 e))
                            (line (nth 2 e)))
                        ;; Entry ID is (source-buffer . line-no) so RET can
                        ;; always navigate even when the table is sorted by
                        ;; a column other than Line.
                        (list (cons source-buf line)
                              (vector (number-to-string line)
                                      (symbol-name type)
                                      name))))
                    entities))
      (tabulated-list-print :remember-pos))
    (pop-to-buffer browser-buf)))

;; ---------------------------------------------------------------------------
;;* Seed7 Key Map
;;  =============
;;

(defvar seed7-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'seed7-complete-statement-or-indent)
    (define-key map (kbd "<backtab>") 'tempo-forward-mark)
    (define-key map (kbd "C-c %") 'seed7-toggle-menu-callable-list)
    (define-key map (kbd "C-c =") 'seed7-toggle-menu-sorting)
    (define-key map (kbd "C-c C-a") 'seed7-to-block-backward)
    (define-key map (kbd "C-c C-e") 'seed7-to-block-forward)
    (define-key map (kbd "C-c C-l") 'seed7-list-entities)
    (define-key map (kbd "C-c C-n") 'seed7-beg-of-next-defun)
    (define-key map (kbd "C-c C-t") 'seed7-to-top-of-block)
    (define-key map "\M-\C-a"       'seed7-beg-of-defun)
    (define-key map "\M-\C-e"       'seed7-end-of-defun)
    (define-key map "\M-\C-h"       'seed7-mark-defun)
    (define-key map "\M-q"          'seed7-fill)
    (define-key map "\M-\C-q"       'seed7-indent-block)
    (define-key map (kbd "C-c ;")   'seed7-toggle-comment-style)
    (define-key map (kbd "C-c ?")   'seed7-mode-version)
    (define-key map (kbd "C-c <f3>") 'seed7-mode-customize)
    (define-key map (kbd "C-c C-c") 'seed7-check-or-compile)
    (define-key map (kbd "C-c C-r") 'seed7-run-program)
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
    ["Reload abbreviations table" seed7-rebuild-abbrev-table]
    "---"
    ("Align"
     ["Align current section" align-current]
     ["Align marked area"     align
      :help "Align the marked area, using distinct sections if necessary."]
     ["Align entire"         align-entire
      :help "Align marked region as 1 alignment section."]
     ["Align with regexp"     align-regexp
      :help "Align on text specified by regular expression."]
     "---"
     ["Align newline and indent" align-newline-and-indent]
     "---"
     ["Align highlight"       align-highlight-rule
      :help "Highlight the whitespace modified by a given align rule."]
     ["Remove highlight"      align-unhighlight-rule
      :help "Remove any highlighting added by `align-highlight-rule'."])
    "---"
    ("Code Template"
     ["Expand keyword/Indent"   seed7-complete-statement-or-indent
      :help "Hit <tab> after any keyword to expand it to code."  ]
     ["Move to next marker"     tempo-forward-mark
      :help "Move to next tempo marker identifying area to fill in code template."
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
    ["List Seed7 Entities" seed7-list-entities]
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
    ["Static check"  seed7-check-or-compile
     :help "Perform static analysis of Seed7 code in visited file."]
    ["Compile"       (seed7-check-or-compile t)
     :help "Compile Seed7 visited file. Key binding is: C-u C-c C-c"]
    ["Run"         seed7-run-program
     :help "Check and run the Seed7 program in the visited file. Key: C-c C-r"]
    "---"
    ["Customize Mode" (customize-group 'seed7)
     :help "Open the seed7 customization buffer"]))

;; ---------------------------------------------------------------------------
;;* Seed7 Major Mode
;;  ================

(defconst seed7--align-mode-rules-list
  (list
   (list
    'seed7-mode-initialization
    (cons 'regexp
          (format ":\\(\\s-+\\)%s+?\\(\\s-+\\)is\\>"
                  seed7-name-identifier-nc-re))
    (cons 'modes '(seed7-mode))
    (list 'group 1 2))
   (list
    'seed7-mode-assignment
    (cons 'regexp
          (format "\\(\\s-+\\)%s\\(\\s-+\\)"
                  seed7-predef-assignment-operator-regexp))
    (list 'group 1 2)
    (cons 'modes '(seed7-mode))))
  "Alignment rules for `seed7-mode'.")


;;;###autoload
(define-derived-mode seed7-mode prog-mode "seed7"
  "Major mode for editing Seed7 files.

See https://seed7.net/ for information on the Seed7 programming
language.

Use `seed7-mode-customize' to customize important elements;
this major mode takes advantage of Seed7's ability to analyze itself to provide
built-in cross reference support.  This, along with static analysis and
compilation requires a working installation of Seed7.

\\<seed7-mode-map>"

  ;; Seed7 Font Locking Control
  (setq-local font-lock-defaults '((seed7-font-lock-keywords)))

  ;; Seed7 Mode Syntax Propertize Function
  (setq-local syntax-propertize-function #'seed7-mode-syntax-propertize)

  ;; Allow forward-sexp/backward-sexp to navigate Seed7 comments,
  ;; procedure/function boundaries, block start/end lines, array/set
  ;; definition blocks, and delimiter pairs.
  (setq-local forward-sexp-function #'seed7--forward-sexp-function)

  ;; Seed7 iMenu Support
  (seed7--setup-imenu)

  ;; Seed7 which-function-mode support
  ;; `which-func-functions' is tried in turn; first non-nil result wins.
  (setq-local which-func-functions
              (list #'seed7--qualified-name-at-pos))
  ;; `add-log-current-defun-function' drives `C-x 4 a' and ChangeLog entries.
  (setq-local add-log-current-defun-function
              #'seed7--qualified-name-at-pos)

  ;; Seed7 Comments Control
  (seed7--set-comment-style seed7-uses-block-comment)

  ;; Drive all indent width control variables from seed7-indent-width:
  ;; although tab-width does not really identify the number of columns
  ;; used for indentation, Seed7 author does not recommend using hard tabs in
  ;; Seed7 code.  Therefore we can use a philosophy that an indentation step
  ;; corresponds to a tab-width and set the tab-width to `seed7-indent-width'.
  ;; This will allow users to force indent one indentation level by executing
  ;; the `indent-rigidly' command.
  ;; To ensure this is the case, the seed7-mode also forces `indent-tabs-mode'
  ;; to nil to prevent insertion of hard tabs.
  ;; Prevent using a value of `seed7-indent-width' user-option that would be
  ;; invalid.
  (cond
   ((not (integerp seed7-indent-width)) (setq-local seed7-indent-width 2))
   ((< seed7-indent-width 2) (setq-local seed7-indent-width 2))
   ((> seed7-indent-width 8) (setq-local seed7-indent-width 8)))
  ;; Adjust tab width to the indentation; that's sometimes useful to quickly
  ;; change the visual rendering of the indentation by converting to tabs then
  ;; changing the tab width.
  (setq-local tab-width seed7-indent-width)
  ;;
  (setq-local indent-tabs-mode nil)

  ;; Seed7 Code Navigation
  ;; Allow code familiar with the standard `beginning-of-defun' and
  ;; `end-of-defun' to work inside Seed7 buffers.  This includes iedit,
  ;; expand-region, etc...
  (setq-local beginning-of-defun-function
              #'seed7-nav-beginning-of-defun)
  (setq-local end-of-defun-function
              #'seed7-nav-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (when (and (>= emacs-major-version 29)
             (boundp 'end-of-defun-moves-to-eol))
    (setq-local end-of-defun-moves-to-eol nil))

  ;; Seed7 outline minor-mode support
  (setq-local outline-regexp
              "const \\(type: \\|proc: \\|func \\)")
  (setq-local outline-heading-end-regexp
              "\\( is\\(?:\\ new struct\\| func\\)?\\)")

  ;; Seed7 Indentation
  (when seed7-auto-indent
    (setq-local indent-line-function #'seed7-indent-line))

  (when seed7-support-abbrev-mode
    ;; Seed7 Abbreviation Support
    (setq-local local-abbrev-table seed7-mode-abbrev-table))

  ;; Seed7 Cross Reference
  ;; - Use the xref framework : implement a backend for Seed7 here. See above.
  (add-hook 'xref-backend-functions #'seed7--xref-backend nil t)
  ;; - Invalidate cache on file save/revert: force a rebuild on next xref lookup.
  (add-hook 'after-save-hook #'seed7--invalidate-xref-cache nil t)
  (add-hook 'after-revert-hook #'seed7--invalidate-xref-cache nil t)

  ;; Seed7 Completion [ TODO 2025-07-08, by Pierre Rouleau: find the proper way to hook it]
  ;; (add-hook 'completion-at-point-functions #'seed7--xref-backend nil t)
  ;; (add-hook 'completion-at-point-functions #'seed7--list-of-terms nil
  ;; 'local)

  ;; Seed7 Source Code Alignment rules
  (setq-local align-mode-rules-list
              (copy-tree seed7--align-mode-rules-list))
  (setq-local align-region-separate 'group)

  ;; Enhance the expand-region mode: allow expansion of enclosing block
  ;; one block level at a time.
  ;; Register when expand-region is available.
  (if (boundp 'er/try-expand-list)
      ;; The expand-region feature is already loaded: activate the
      ;; expansion enhancement right away.
      (seed7--setup-expand-region)
    ;; The expand-region is not loaded yet: schedule the activation
    ;; when it loads.
    (with-eval-after-load 'expand-region
      (seed7--activate-expand-region))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

;;; --------------------------------------------------------------------------
(provide 'seed7-mode)

;; Local variables:
;; time-stamp-format: "%Y%02m%02d.%02H%02M"
;; time-stamp-start: "Package-Version:[ \t]+\\\\?"
;; time-stamp-end: "\n"
;; time-stamp-line-limit: 15
;; End:

;;; seed7-mode.el ends here
