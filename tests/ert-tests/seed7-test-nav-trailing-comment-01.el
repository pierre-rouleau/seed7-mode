;;; seed7-test-nav-trailing-comment-01.el --- Regression: backward-sexp with trailing whitespace/comment after `is func'.  -*- lexical-binding: t; -*-

;;; --------------------------------------------------------------------------
;;; Commentary:
;;
;; Regression test for a `backward-sexp' failure when the declaration's
;; `is func' line ends with trailing whitespace followed by a `#' comment
;; (e.g. `is func #').  `seed7---func/proc/type-decl-start-re' anchors the
;; declaration-start match to `$' right after `is func', with no tolerance
;; for trailing whitespace/comment, so the nesting-aware backward scan in
;; `seed7-beg-of-defun' (invoked from `seed7--forward-sexp-function' when
;; `backward-sexp' is issued from `end func;') fails to recognize the
;; declaration line and cannot find it.
;;
;; NOTE: line 2 below intentionally ends with `is func <SPACE>#'.  This must
;; be preserved verbatim; the trailing `#' after the space prevents editors
;; from silently stripping the trailing whitespace on save.
;;
;; Fixture line layout (1-based):
;;
;;   1   blank
;;   2   const func string: gcmMult (in string: factor1, in factorHType: factorH) is func #
;;   3     result
;;   4       var string: product is "";
;;   5     local
;;   6       var integer: index is 0;
;;   7     begin
;;   8       index := 1;
;;   9     end func;
;;  10   blank

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Fixture
;;

(defconst seed7-test--trailing-comment-nav-code
  (concat
   "\n"
   "const func string: gcmMult (in string: factor1, in factorHType: factorH) is func #\n"
   "  result\n"
   "    var string: product is \"\";\n"
   "  local\n"
   "    var integer: index is 0;\n"
   "  begin\n"
   "    index := 1;\n"
   "  end func;\n"
   "\n"))

(defmacro seed7-test--with-trailing-comment-nav-buffer (&rest body)
  "Run BODY in a temp buffer holding `seed7-test--trailing-comment-nav-code'."
  `(with-temp-buffer
     (seed7-mode)
     (insert seed7-test--trailing-comment-nav-code)
     ,@body))

(defun seed7-test--trailing-comment-nav-goto-eol (line)
  "Move point to the end of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line))
  (end-of-line))

(defun seed7-test--trailing-comment-nav-current-line ()
  "Return the current line number (1-based)."
  (line-number-at-pos))

;;; --------------------------------------------------------------------------
;;; Tests
;;

(ert-deftest seed7-nav-trailing-comment/beginning-of-defun-reaches-decl ()
  "`beginning-of-defun' from `end func;' (line 9) must reach the declaration
start on line 2, even though line 2 ends with trailing whitespace + `#'."
  (seed7-test--with-trailing-comment-nav-buffer
    (seed7-test--trailing-comment-nav-goto-eol 9)
    (beginning-of-defun)
    (should (= (seed7-test--trailing-comment-nav-current-line) 2))))

(ert-deftest seed7-nav-trailing-comment/backward-sexp-from-end-func-reaches-decl ()
  "`seed7--forward-sexp-function' called with -1 from EOL of `end func;'
(line 9) must reach the declaration start on line 2, not fail or land at
some intermediate position, when line 2 ends with trailing whitespace + `#'."
  (seed7-test--with-trailing-comment-nav-buffer
    (seed7-test--trailing-comment-nav-goto-eol 9)
    (seed7--forward-sexp-function -1)
    (should (= (seed7-test--trailing-comment-nav-current-line) 2))))

(ert-deftest seed7-nav-trailing-comment/end-of-defun-reaches-end-func ()
  "`end-of-defun' from the declaration (line 2, with trailing whitespace +
`#') must reach `end func;' on line 9, confirming the forward direction is
also unaffected once the fix generalizes the regex."
  (seed7-test--with-trailing-comment-nav-buffer
    (goto-char (point-min))
    (forward-line 1)                    ; line 2
    (seed7-end-of-defun)
    (should (= (seed7-test--trailing-comment-nav-current-line) 9))))

;; ---------------------------------------------------------------------------
(provide 'seed7-test-nav-trailing-comment-01)

;;; seed7-test-nav-trailing-comment-01.el ends here
