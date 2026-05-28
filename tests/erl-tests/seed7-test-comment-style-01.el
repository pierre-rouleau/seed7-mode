;;; seed7-test-comment-style-01.el --- ERT-based test of Seed7 comment style toggling.  -*- lexical-binding: t; -*-

;; Created   : Thursday, May 28 2026.
;; Author    : Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the SEED7 package.
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
;; ERT-based tests for seed7-mode comment style toggling.
;;
;; Seed7 supports two comment styles:
;;   - Line comments: text following '#' to end of line
;;   - Block comments: text between '(*' and '*)'
;;
;; The mode defaults to line-comment style.  The command
;; `seed7-toggle-comment-style' and `seed7--set-comment-style' switch
;; between the two styles for the current buffer.
;;
;; These tests verify:
;;   1. Default comment style (line) after activating seed7-mode.
;;   2. Switch to block comment style.
;;   3. Switch back to line comment style.
;;   4. Force line style with negative argument.
;;   5. Force block style with positive argument.

;;; --------------------------------------------------------------------------
;;; Dependencies:
;;
(require 'ert)
(require 'seed7-mode)

;;; --------------------------------------------------------------------------
;;; Code:
;;

;; ---------------------------------------------------------------------------
;;** Tests: default comment style

(ert-deftest seed7-test-comment-default-is-line-style ()
  "Test that seed7-mode defaults to line comment style (using '#')."
  (with-temp-buffer
    (seed7-mode)
    ;; Default: seed7-uses-block-comment should be nil (line style is default)
    ;; comment-start should be "# "
    (should (string= comment-start "# "))
    ;; comment-end should be "" (no closing delimiter for line comments)
    (should (string= comment-end ""))
    ;; comment-start-skip should match "#"
    (should (string-match-p comment-start-skip "# "))))

;; ---------------------------------------------------------------------------
;;** Tests: switch to block comment style

(ert-deftest seed7-test-comment-switch-to-block ()
  "Test switching to block comment style ('(* ... *)')."
  (with-temp-buffer
    (seed7-mode)
    ;; Switch to block comment style
    (seed7--set-comment-style t)
    ;; comment-start should be "(* "
    (should (string= comment-start "(* "))
    ;; comment-end should be " *)"
    (should (string= comment-end " *)"))))

;; ---------------------------------------------------------------------------
;;** Tests: switch back to line comment style

(ert-deftest seed7-test-comment-switch-back-to-line ()
  "Test switching from block back to line comment style."
  (with-temp-buffer
    (seed7-mode)
    ;; First switch to block
    (seed7--set-comment-style t)
    (should (string= comment-start "(* "))
    ;; Then switch back to line
    (seed7--set-comment-style nil)
    (should (string= comment-start "# "))
    (should (string= comment-end ""))))

;; ---------------------------------------------------------------------------
;;** Tests: toggle comment style

(ert-deftest seed7-test-comment-toggle-to-block ()
  "Test that `seed7-toggle-comment-style' toggles from line to block style."
  (with-temp-buffer
    (seed7-mode)
    ;; Default is line style; toggling should give block style
    (should (string= comment-start "# "))
    (seed7-toggle-comment-style)
    (should (string= comment-start "(* "))
    (should (string= comment-end " *)"))))

(ert-deftest seed7-test-comment-toggle-twice-returns-to-line ()
  "Test that `seed7-toggle-comment-style' called twice returns to line style."
  (with-temp-buffer
    (seed7-mode)
    (seed7-toggle-comment-style)
    (seed7-toggle-comment-style)
    (should (string= comment-start "# "))
    (should (string= comment-end ""))))

;; ---------------------------------------------------------------------------
;;** Tests: set comment style with explicit argument

(ert-deftest seed7-test-comment-toggle-with-positive-arg ()
  "Test that `seed7-toggle-comment-style' with positive arg forces block style."
  (with-temp-buffer
    (seed7-mode)
    ;; Positive arg (1) should force block comment style
    (seed7-toggle-comment-style 1)
    (should (string= comment-start "(* "))
    (should (string= comment-end " *)"))))

(ert-deftest seed7-test-comment-toggle-with-negative-arg ()
  "Test that `seed7-toggle-comment-style' with negative arg forces line style."
  (with-temp-buffer
    (seed7-mode)
    ;; First switch to block, then use negative arg to force line
    (seed7--set-comment-style t)
    (should (string= comment-start "(* "))
    (seed7-toggle-comment-style -1)
    (should (string= comment-start "# "))))

;; ---------------------------------------------------------------------------
;;** Tests: comment-continue and comment-start-skip

(ert-deftest seed7-test-comment-continue-line-style ()
  "Test comment-continue is '#' in line comment style."
  (with-temp-buffer
    (seed7-mode)
    (seed7--set-comment-style nil)
    ;; In line style, comment-continue should use '#'
    (should (string= comment-continue "#"))))

(ert-deftest seed7-test-comment-continue-block-style ()
  "Test comment-continue uses '* ' in block comment style."
  (with-temp-buffer
    (seed7-mode)
    (seed7--set-comment-style t)
    ;; In block style, comment-continue should be " * " (with leading space)
    (should (string= comment-continue " * "))))

(ert-deftest seed7-test-comment-start-skip-line-style ()
  "Test comment-start-skip matches '#' in line comment style."
  (with-temp-buffer
    (seed7-mode)
    (seed7--set-comment-style nil)
    ;; comment-start-skip is a regexp; it should match "#"
    (should (string-match-p comment-start-skip "# "))))

(ert-deftest seed7-test-comment-start-skip-block-style ()
  "Test comment-start-skip matches '(*' in block comment style."
  (with-temp-buffer
    (seed7-mode)
    (seed7--set-comment-style t)
    ;; comment-start-skip is a regexp; it should match "(*"
    (should (string-match-p comment-start-skip "(* "))))

;;; --------------------------------------------------------------------------
(provide 'seed7-test-comment-style-01)

;;; seed7-test-comment-style-01.el ends here