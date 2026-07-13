;;; seed7-test-indent-02.el --- Comprehensive ERT tests for Seed7 indentation  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-07-13 07:15:25 EDT, updated by Pierre Rouleau>

;; This file is part of the SEED7-MODE package.
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
;; Comprehensive indentation tests covering as many Seed7 indentation schemes
;; as possible.  Each test group covers one structural pattern, verifying both
;; that `indent-region' preserves correctly-indented code and that it fixes
;; misaligned code.
;;
;; Patterns covered:
;;
;; 1. Simple procedure with `is func / begin / end func'
;;    - Top-level declaration: column 0
;;    - `begin' and `end func;': column 2
;;    - Body statements: column 4
;;
;; 2. Function with `result', `local', and `begin' sections
;;    - Section keywords (`result', `local', `begin', `end func;'): column 2
;;    - Variables inside sections: column 4
;;    - Body statements inside `begin': column 4
;;
;; 3. Expression function with `is return'
;;    - Declaration: column 0
;;    - `return' line: column 2
;;
;; 4. `if / elsif / else / end if' inside a proc body
;;    - `if', `elsif', `else', `end if;': column 4
;;    - Conditional bodies: column 6
;;
;; 5. `for' loop inside a proc body
;;    - `for ... do' and `end for;': column 4
;;    - Loop body: column 6
;;
;; 6. `while' loop inside a proc body
;;    - `while ... do' and `end while;': column 4
;;    - Loop body: column 6
;;
;; 7. `repeat / until' loop inside a proc body
;;    - `repeat' and `until ...;': column 4
;;    - Loop body: column 6
;;
;; 8. Nested `if' inside `for' inside a proc body
;;    - Outer `for' / `end for;': column 4
;;    - `if', `else', `end if;' inside for: column 6
;;    - Bodies inside if/else: column 8
;;
;; 9. `block / exception / end block' inside a proc body
;;    - `block', `exception', `end block;': column 4
;;    - Body inside block: column 6
;;    - `catch' line: column 6
;;    - Catch body: column 8
;;
;; 10. Multi-line `return' with `or' / `and' continuation
;;     - `return' line: column 2
;;     - First continuation (after `or'): column 9 (after `return ')
;;     - Subsequent continuation starting with `(': column 9
;;     - Line inside parens after that: column 10
;;
;; 11. Array definition block
;;     - Declaration: column 0
;;     - Array elements: column 2
;;
;; 12. Set definition block
;;     - Declaration: column 0
;;     - Set elements: column 2
;;
;; 13. One-line action/primitive declarations must not accumulate indentation.
;;
;; 14. Nested block-opening callable inside `begin', followed by `end func;'.
;;
;; 15. Struct member-list definition: `const type: X is new struct ... end struct;'
;;
;; 16. Enum member-list definition: `const type: X is new enum ... end enum;'
;;
;; 17. Enum inheritance-style member-list definition:

;; ---------------------------------------------------------------------------
;;; Code:

(require 'ert)
(require 'seed7-mode)

;; ---------------------------------------------------------------------------
;; Shared test helpers (also defined in seed7-test-indent-01.el, but each
;; test file is self-contained so they are redefined here).

(defun seed7-test-indent-02--goto-line (line)
  "Move point to the beginning of LINE (1-based)."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun seed7-test-indent-02--line-indentation (line)
  "Return indentation column of LINE (1-based)."
  (save-excursion
    (seed7-test-indent-02--goto-line line)
    (current-indentation)))

;; ---------------------------------------------------------------------------
;; 1. Simple procedure: `is func / begin / end func'
;; -------------------------------------------------
;;
;;   const proc: simpleProcedure is func      ; col 0
;;     begin                                  ; col 2
;;       writeln("hello");                    ; col 4
;;     end func;                              ; col 2

(defconst seed7-test-indent-02--simple-proc-correct
  (concat
   "const proc: simpleProcedure is func\n"
   "  begin\n"
   "    writeln(\"hello\");\n"
   "  end func;\n")
  "Correctly-indented simple procedure fixture.")

(defconst seed7-test-indent-02--simple-proc-misaligned
  (concat
   "const proc: simpleProcedure is func\n"
   "begin\n"
   "writeln(\"hello\");\n"
   "end func;\n")
  "Misaligned simple procedure fixture.")

(ert-deftest seed7-indent/simple-proc-keeps-correct-layout ()
  "Indenting an already-correct simple procedure keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--simple-proc-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--simple-proc-correct))))

(ert-deftest seed7-indent/simple-proc-fixes-misaligned-layout ()
  "Indenting a misaligned simple procedure restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--simple-proc-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--simple-proc-correct))))

;; ---------------------------------------------------------------------------
;; 2. Function with result / local / begin sections
;; ------------------------------------------------
;;
;;   const func integer: countItems is func   ; col 0
;;     result                                 ; col 2
;;       var integer: n is 0;                 ; col 4
;;     local                                  ; col 2
;;       var integer: i is 0;                 ; col 4
;;     begin                                  ; col 2
;;       n := 42;                             ; col 4
;;     end func;                              ; col 2

(defconst seed7-test-indent-02--func-sections-correct
  (concat
   "const func integer: countItems is func\n"
   "  result\n"
   "    var integer: n is 0;\n"
   "  local\n"
   "    var integer: i is 0;\n"
   "  begin\n"
   "    n := 42;\n"
   "  end func;\n")
  "Correctly-indented func with result/local/begin sections.")

(defconst seed7-test-indent-02--func-sections-misaligned
  (concat
   "const func integer: countItems is func\n"
   "result\n"
   "var integer: n is 0;\n"
   "local\n"
   "var integer: i is 0;\n"
   "begin\n"
   "n := 42;\n"
   "end func;\n")
  "Misaligned func with result/local/begin sections.")

(ert-deftest seed7-indent/func-sections-keeps-correct-layout ()
  "Indenting an already-correct func with result/local/begin keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--func-sections-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--func-sections-correct))))

(ert-deftest seed7-indent/func-sections-fixes-misaligned-layout ()
  "Indenting a misaligned func with result/local/begin restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--func-sections-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--func-sections-correct))))

;; ---------------------------------------------------------------------------
;; 3. Expression function with `is return'
;; ---------------------------------------
;;
;;   const func boolean: alwaysTrue is   ; col 0
;;     return TRUE;                      ; col 2

(defconst seed7-test-indent-02--is-return-correct
  (concat
   "const func boolean: alwaysTrue is\n"
   "  return TRUE;\n")
  "Correctly-indented `is return' function.")

(defconst seed7-test-indent-02--is-return-misaligned
  (concat
   "const func boolean: alwaysTrue is\n"
   "return TRUE;\n")
  "Misaligned `is return' function.")

(ert-deftest seed7-indent/is-return-keeps-correct-layout ()
  "Indenting an already-correct `is return' function keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--is-return-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--is-return-correct))))

(ert-deftest seed7-indent/is-return-fixes-misaligned-layout ()
  "Indenting a misaligned `is return' function restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--is-return-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--is-return-correct))))

;; ---------------------------------------------------------------------------
;; 4. `if / elsif / else / end if' inside a proc body
;; ---------------------------------------------------
;;
;;   const proc: classify (in integer: n) is func   ; col 0
;;     begin                                        ; col 2
;;       if n > 0 then                              ; col 4
;;         writeln("positive");                     ; col 6
;;       elsif n < 0 then                           ; col 4
;;         writeln("negative");                     ; col 6
;;       else                                       ; col 4
;;         writeln("zero");                         ; col 6
;;       end if;                                    ; col 4
;;     end func;                                    ; col 2

(defconst seed7-test-indent-02--if-elsif-else-correct
  (concat
   "const proc: classify (in integer: n) is func\n"
   "  begin\n"
   "    if n > 0 then\n"
   "      writeln(\"positive\");\n"
   "    elsif n < 0 then\n"
   "      writeln(\"negative\");\n"
   "    else\n"
   "      writeln(\"zero\");\n"
   "    end if;\n"
   "  end func;\n")
  "Correctly-indented if/elsif/else/end if fixture.")

(defconst seed7-test-indent-02--if-elsif-else-misaligned
  (concat
   "const proc: classify (in integer: n) is func\n"
   "begin\n"
   "if n > 0 then\n"
   "writeln(\"positive\");\n"
   "elsif n < 0 then\n"
   "writeln(\"negative\");\n"
   "else\n"
   "writeln(\"zero\");\n"
   "end if;\n"
   "end func;\n")
  "Misaligned if/elsif/else/end if fixture.")

(ert-deftest seed7-indent/if-elsif-else-keeps-correct-layout ()
  "Indenting an already-correct if/elsif/else block keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--if-elsif-else-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 6))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 4))
    (should (= (seed7-test-indent-02--line-indentation 10) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--if-elsif-else-correct))))

(ert-deftest seed7-indent/if-elsif-else-fixes-misaligned-layout ()
  "Indenting a misaligned if/elsif/else block restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--if-elsif-else-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 6))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 4))
    (should (= (seed7-test-indent-02--line-indentation 10) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--if-elsif-else-correct))))

;; ---------------------------------------------------------------------------
;; 5. `for' loop inside a proc body
;; ---------------------------------
;;
;;   const proc: countUp is func     ; col 0
;;     local                         ; col 2
;;       var integer: i is 0;        ; col 4
;;     begin                         ; col 2
;;       for i range 1 to 5 do       ; col 4
;;         writeln(i);               ; col 6
;;       end for;                    ; col 4
;;     end func;                     ; col 2

(defconst seed7-test-indent-02--for-loop-correct
  (concat
   "const proc: countUp is func\n"
   "  local\n"
   "    var integer: i is 0;\n"
   "  begin\n"
   "    for i range 1 to 5 do\n"
   "      writeln(i);\n"
   "    end for;\n"
   "  end func;\n")
  "Correctly-indented for loop fixture.")

(defconst seed7-test-indent-02--for-loop-misaligned
  (concat
   "const proc: countUp is func\n"
   "local\n"
   "var integer: i is 0;\n"
   "begin\n"
   "for i range 1 to 5 do\n"
   "writeln(i);\n"
   "end for;\n"
   "end func;\n")
  "Misaligned for loop fixture.")

(ert-deftest seed7-indent/for-loop-keeps-correct-layout ()
  "Indenting an already-correct for loop keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--for-loop-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--for-loop-correct))))

(ert-deftest seed7-indent/for-loop-fixes-misaligned-layout ()
  "Indenting a misaligned for loop restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--for-loop-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--for-loop-correct))))

;; ---------------------------------------------------------------------------
;; 6. `while' loop inside a proc body
;; ------------------------------------
;;
;;   const proc: countdown is func   ; col 0
;;     local                         ; col 2
;;       var integer: n is 10;       ; col 4
;;     begin                         ; col 2
;;       while n > 0 do              ; col 4
;;         n -:= 1;                  ; col 6
;;       end while;                  ; col 4
;;     end func;                     ; col 2

(defconst seed7-test-indent-02--while-loop-correct
  (concat
   "const proc: countdown is func\n"
   "  local\n"
   "    var integer: n is 10;\n"
   "  begin\n"
   "    while n > 0 do\n"
   "      n -:= 1;\n"
   "    end while;\n"
   "  end func;\n")
  "Correctly-indented while loop fixture.")

(defconst seed7-test-indent-02--while-loop-misaligned
  (concat
   "const proc: countdown is func\n"
   "local\n"
   "var integer: n is 10;\n"
   "begin\n"
   "while n > 0 do\n"
   "n -:= 1;\n"
   "end while;\n"
   "end func;\n")
  "Misaligned while loop fixture.")

(ert-deftest seed7-indent/while-loop-keeps-correct-layout ()
  "Indenting an already-correct while loop keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--while-loop-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--while-loop-correct))))

(ert-deftest seed7-indent/while-loop-fixes-misaligned-layout ()
  "Indenting a misaligned while loop restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--while-loop-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--while-loop-correct))))

;; ---------------------------------------------------------------------------
;; 7. `repeat / until' loop inside a proc body
;; ---------------------------------------------
;;
;;   const proc: repeatLoop is func   ; col 0
;;     local                          ; col 2
;;       var integer: n is 0;         ; col 4
;;     begin                          ; col 2
;;       repeat                       ; col 4
;;         n +:= 1;                   ; col 6
;;       until n >= 10;               ; col 4
;;     end func;                      ; col 2

(defconst seed7-test-indent-02--repeat-loop-correct
  (concat
   "const proc: repeatLoop is func\n"
   "  local\n"
   "    var integer: n is 0;\n"
   "  begin\n"
   "    repeat\n"
   "      n +:= 1;\n"
   "    until n >= 10;\n"
   "  end func;\n")
  "Correctly-indented repeat/until loop fixture.")

(defconst seed7-test-indent-02--repeat-loop-misaligned
  (concat
   "const proc: repeatLoop is func\n"
   "local\n"
   "var integer: n is 0;\n"
   "begin\n"
   "repeat\n"
   "n +:= 1;\n"
   "until n >= 10;\n"
   "end func;\n")
  "Misaligned repeat/until loop fixture.")

(ert-deftest seed7-indent/repeat-loop-keeps-correct-layout ()
  "Indenting an already-correct repeat/until loop keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--repeat-loop-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--repeat-loop-correct))))

(ert-deftest seed7-indent/repeat-loop-fixes-misaligned-layout ()
  "Indenting a misaligned repeat/until loop restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--repeat-loop-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--repeat-loop-correct))))

;; ---------------------------------------------------------------------------
;; 8. Nested `if / else / end if' inside `for' inside a proc body
;; --------------------------------------------------------------
;;
;;   const proc: nestedBlocks is func   ; col 0
;;     local                            ; col 2
;;       var integer: i is 0;           ; col 4
;;     begin                            ; col 2
;;       for i range 1 to 10 do         ; col 4
;;         if i > 5 then                ; col 6
;;           writeln("big");            ; col 8
;;         else                         ; col 6
;;           writeln("small");          ; col 8
;;         end if;                      ; col 6
;;       end for;                       ; col 4
;;     end func;                        ; col 2

(defconst seed7-test-indent-02--nested-if-in-for-correct
  (concat
   "const proc: nestedBlocks is func\n"
   "  local\n"
   "    var integer: i is 0;\n"
   "  begin\n"
   "    for i range 1 to 10 do\n"
   "      if i > 5 then\n"
   "        writeln(\"big\");\n"
   "      else\n"
   "        writeln(\"small\");\n"
   "      end if;\n"
   "    end for;\n"
   "  end func;\n")
  "Correctly-indented nested if-in-for fixture.")

(defconst seed7-test-indent-02--nested-if-in-for-misaligned
  (concat
   "const proc: nestedBlocks is func\n"
   "local\n"
   "var integer: i is 0;\n"
   "begin\n"
   "for i range 1 to 10 do\n"
   "if i > 5 then\n"
   "writeln(\"big\");\n"
   "else\n"
   "writeln(\"small\");\n"
   "end if;\n"
   "end for;\n"
   "end func;\n")
  "Misaligned nested if-in-for fixture.")

(ert-deftest seed7-indent/nested-if-in-for-keeps-correct-layout ()
  "Indenting an already-correct nested if-in-for keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-if-in-for-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 8))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 8))
    (should (= (seed7-test-indent-02--line-indentation 10) 6))
    (should (= (seed7-test-indent-02--line-indentation 11) 4))
    (should (= (seed7-test-indent-02--line-indentation 12) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-if-in-for-correct))))

(ert-deftest seed7-indent/nested-if-in-for-fixes-misaligned-layout ()
  "Indenting a misaligned nested if-in-for restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-if-in-for-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 8))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 8))
    (should (= (seed7-test-indent-02--line-indentation 10) 6))
    (should (= (seed7-test-indent-02--line-indentation 11) 4))
    (should (= (seed7-test-indent-02--line-indentation 12) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-if-in-for-correct))))

;; ---------------------------------------------------------------------------
;; 9. `block / exception / end block' inside a proc body
;; ------------------------------------------------------
;;
;;   const proc: blockTest is func   ; col 0
;;     begin                         ; col 2
;;       block                       ; col 4
;;         foo;                      ; col 6
;;       exception                   ; col 4
;;         catch RANGE_ERROR:        ; col 6
;;           bar;                    ; col 8
;;       end block;                  ; col 4
;;     end func;                     ; col 2

(defconst seed7-test-indent-02--block-exception-correct
  (concat
   "const proc: blockTest is func\n"
   "  begin\n"
   "    block\n"
   "      foo;\n"
   "    exception\n"
   "      catch RANGE_ERROR:\n"
   "        bar;\n"
   "    end block;\n"
   "  end func;\n")
  "Correctly-indented block/exception/end block fixture.")

(defconst seed7-test-indent-02--block-exception-misaligned
  (concat
   "const proc: blockTest is func\n"
   "begin\n"
   "block\n"
   "foo;\n"
   "exception\n"
   "catch RANGE_ERROR:\n"
   "bar;\n"
   "end block;\n"
   "end func;\n")
  "Misaligned block/exception/end block fixture.")

(ert-deftest seed7-indent/block-exception-keeps-correct-layout ()
  "Indenting an already-correct block/exception keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--block-exception-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 6))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 8))
    (should (= (seed7-test-indent-02--line-indentation 8) 4))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--block-exception-correct))))

(ert-deftest seed7-indent/block-exception-fixes-misaligned-layout ()
  "Indenting a misaligned block/exception restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--block-exception-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 6))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 6))
    (should (= (seed7-test-indent-02--line-indentation 7) 8))
    (should (= (seed7-test-indent-02--line-indentation 8) 4))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--block-exception-correct))))

;; ---------------------------------------------------------------------------
;; 10. Multi-line `return' with `or' / `and' continuation (regression)
;; --------------------------------------------------------------------
;;
;; This is the regression case from prg/bas7.sd7 around line 696.
;;
;;   const func boolean: isStringVar (in string: symbol) is   ; col 0
;;     return symbol in string_var_name or                    ; col 2
;;            symbol <> "" and                                ; col 9
;;            (symbol[length(symbol)] = '$' or ... and        ; col 9
;;             not symbol[length(symbol)] ...);               ; col 10

(defconst seed7-test-indent-02--return-or-and-correct
  (concat
   "const func boolean: isStringVar (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "         symbol <> \"\" and\n"
   "         (symbol[length(symbol)] = '$' or symbol[1] in defstr_var and\n"
   "          not symbol[length(symbol)] in numeric_var_suffix);\n")
  "Correctly-indented multi-line return with or/and fixture.")

(defconst seed7-test-indent-02--return-or-and-misaligned
  (concat
   "const func boolean: isStringVar (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "symbol <> \"\" and\n"
   "(symbol[length(symbol)] = '$' or symbol[1] in defstr_var and\n"
   "not symbol[length(symbol)] in numeric_var_suffix);\n")
  "Misaligned multi-line return with or/and fixture.")

(ert-deftest seed7-indent/return-or-and-keeps-correct-layout ()
  "Indenting an already-correct multi-line return with or/and keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-or-and-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 9))
    (should (= (seed7-test-indent-02--line-indentation 4) 9))
    (should (= (seed7-test-indent-02--line-indentation 5) 10))
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-or-and-correct))))

(ert-deftest seed7-indent/return-or-and-fixes-misaligned-layout ()
  "Indenting a misaligned multi-line return with or/and restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-or-and-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 9))
    (should (= (seed7-test-indent-02--line-indentation 4) 9))
    (should (= (seed7-test-indent-02--line-indentation 5) 10))
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-or-and-correct))))

;; ---------------------------------------------------------------------------
;; 11. Array definition block
;; --------------------------
;;
;;   const array integer: primes is [1] (   ; col 0
;;     2,                                   ; col 2
;;     3,                                   ; col 2
;;     5,                                   ; col 2
;;     7);                                  ; col 2

(defconst seed7-test-indent-02--array-def-correct
  (concat
   "const array integer: primes is [1] (\n"
   "  2,\n"
   "  3,\n"
   "  5,\n"
   "  7);\n")
  "Correctly-indented array definition fixture.")

(defconst seed7-test-indent-02--array-def-misaligned
  (concat
   "const array integer: primes is [1] (\n"
   "2,\n"
   "3,\n"
   "5,\n"
   "7);\n")
  "Misaligned array definition fixture.")

(ert-deftest seed7-indent/array-def-keeps-correct-layout ()
  "Indenting an already-correct array definition keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--array-def-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--array-def-correct))))

(ert-deftest seed7-indent/array-def-fixes-misaligned-layout ()
  "Indenting a misaligned array definition restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--array-def-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--array-def-correct))))

;; ---------------------------------------------------------------------------
;; 11b. Deeply nested array definition followed by another array
;;      declaration (mirrors the `Te'/`Td' tables in lib/aes.s7i).
;; --------------------------------------------------------------------------
;;
;;   const array array integer: Te is [] (   ; col 0
;;     [0] (1, 2,                            ; col 2
;;          3, 4));                          ; nested continuation
;;                                           ; blank
;;   const array array integer: Td is [] (   ; col 0  <- must stay 0
;;     [0] (5, 6,                            ; col 2
;;          7, 8));                          ; nested continuation

(defconst seed7-test-indent-02--nested-array-def-correct
  (concat
   "const array array integer: Te is [] (\n"
   "  [0] (1, 2,\n"
   "       3, 4));\n"
   "\n"
   "const array array integer: Td is [] (\n"
   "  [0] (5, 6,\n"
   "       7, 8));\n")
  "Correctly-indented adjacent deeply nested array definitions, mirroring
the `Te'/`Td' tables in lib/aes.s7i.")

(defconst seed7-test-indent-02--nested-array-def-misaligned
  (concat
   "const array array integer: Te is [] (\n"
   "[0] (1, 2,\n"
   "3, 4));\n"
   "\n"
   "      const array array integer: Td is [] (\n"
   "[0] (5, 6,\n"
   "7, 8));\n")
  "Misaligned version: body lines flush left and `Td' header pushed to
column 6, exercising the bug where `Td' inherited a residual
continuation-indent from `Te''s closing line instead of column 0.")

(defun seed7-test-indent-02--trim-trailing-ws (string)
  "Trim trailing whitespace from each line of STRING, preserving line breaks."
  (mapconcat #'string-trim-right (split-string string "\n") "\n"))

(ert-deftest seed7-indent/nested-array-def-keeps-correct-layout ()
  "Indenting already-correct adjacent nested array definitions keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-array-def-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 5) 0))
    (should (string= (seed7-test-indent-02--trim-trailing-ws (buffer-string))
                     (seed7-test-indent-02--trim-trailing-ws
                      seed7-test-indent-02--nested-array-def-correct)))))

(ert-deftest seed7-indent/nested-array-def-fixes-misaligned-layout ()
  "`indent-region' restores `Td' (and `Te') to column 0 after a deeply
nested closing continuation line, reproducing the lib/aes.s7i defect."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-array-def-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 5) 0))
    (should (string= (seed7-test-indent-02--trim-trailing-ws (buffer-string))
                     (seed7-test-indent-02--trim-trailing-ws
                      seed7-test-indent-02--nested-array-def-correct)))))

(ert-deftest seed7-indent/nested-array-def-tab-indents-second-header ()
  "`seed7-indent-line' alone (simulating TAB) on the misaligned `Td' header
line restores it to column 0, without touching the rest of the buffer."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-array-def-misaligned)
    (seed7-mode)
    (seed7-test-indent-02--goto-line 5)
    (seed7-indent-line)
    (should (= (seed7-test-indent-02--line-indentation 5) 0))))

;; ---------------------------------------------------------------------------
;; 12. Set definition block
;; ------------------------
;;
;;   var set of string: keywords is {   ; col 0
;;     "for",                           ; col 2
;;     "while",                         ; col 2
;;     "repeat"};                       ; col 2

(defconst seed7-test-indent-02--set-def-correct
  (concat
   "var set of string: keywords is {\n"
   "  \"for\",\n"
   "  \"while\",\n"
   "  \"repeat\"};\n")
  "Correctly-indented set definition fixture.")

(defconst seed7-test-indent-02--set-def-misaligned
  (concat
   "var set of string: keywords is {\n"
   "\"for\",\n"
   "\"while\",\n"
   "\"repeat\"};\n")
  "Misaligned set definition fixture.")

(ert-deftest seed7-indent/set-def-keeps-correct-layout ()
  "Indenting an already-correct set definition keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--set-def-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--set-def-correct))))

(ert-deftest seed7-indent/set-def-fixes-misaligned-layout ()
  "Indenting a misaligned set definition restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--set-def-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (= (seed7-test-indent-02--line-indentation 4) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--set-def-correct))))
;; ---------------------------------------------------------------------------
;; Regression: adjacent short `is return' functions (prg/chkarr.sd7, lines 50-87)
;; -------------------------------------------------------------------------
;;
;; Several consecutive `const func ... is' / `return ...;' declarations,
;; separated by blank lines, must each have their `return' line indented
;; to column 2.  This reproduces the bug where `seed7--block-end-pos-for'
;; misidentifies the end of a short function's enclosing block, so
;; `seed7-line-inside-a-block' fails to recognize the first `return' line
;; as belonging to the preceding `const func ... is' declaration.

(defconst seed7-test-indent-02--adjacent-is-return-correct
  (concat
   "const func boolean: boolExpr (ref boolean: value) is\n"
   "  return value and str(rand(1, 9))[2 ..] = \"\";\n"
   "\n"
   "\n"
   "const func integer: intExpr (in integer: number) is\n"
   "  return number + length(str(rand(1, 9))[2 ..]);\n"
   "\n"
   "\n"
   "const func float: floatExpr (in float: number) is\n"
   "  return number;\n")
  "Correctly-indented adjacent `is return' functions, as in prg/chkarr.sd7.")

(defconst seed7-test-indent-02--adjacent-is-return-misaligned
  (concat
   "const func boolean: boolExpr (ref boolean: value) is\n"
   "return value and str(rand(1, 9))[2 ..] = \"\";\n"
   "\n"
   "\n"
   "const func integer: intExpr (in integer: number) is\n"
   "return number + length(str(rand(1, 9))[2 ..]);\n"
   "\n"
   "\n"
   "const func float: floatExpr (in float: number) is\n"
   "return number;\n")
  "Misaligned adjacent `is return' functions, matching prg/chkarr.sd7 as-is.")

(ert-deftest seed7-indent/adjacent-is-return-keeps-correct-layout ()
  "Indenting already-correct adjacent `is return' functions keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--adjacent-is-return-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 0))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (= (seed7-test-indent-02--line-indentation 9) 0))
    (should (= (seed7-test-indent-02--line-indentation 10) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--adjacent-is-return-correct))))

(ert-deftest seed7-indent/adjacent-is-return-fixes-misaligned-layout ()
  "Indenting misaligned adjacent `is return' functions restores the layout.

Regression test for the bug reported against `prg/chkarr.sd7' lines 51, 55,
67, 71, 75, 79, 83: `seed7--block-end-pos-for' treats a short
`const func ... is' declaration like a long-body function ending in
`end func;', so the first `return' line of the short function is never
recognized as being inside the enclosing block and is left at column 0
by both `indent-region' and interactive `TAB'."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--adjacent-is-return-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 2))
    (should (= (seed7-test-indent-02--line-indentation 5) 0))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (= (seed7-test-indent-02--line-indentation 9) 0))
    (should (= (seed7-test-indent-02--line-indentation 10) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--adjacent-is-return-correct))))
;; ---------------------------------------------------------------------------

(ert-deftest seed7-indent/adjacent-is-return-tab-indents-return-line ()
  "Pressing TAB on an unindented short-function `return' line must indent it.

This exercises `seed7-indent-line' directly (the interactive TAB path),
as opposed to `indent-region', to catch the reported failure where TAB
on line 2 (`return value and ...;') does nothing."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--adjacent-is-return-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (forward-line 1)                    ; move to the first `return' line
    (seed7-indent-line)
    (should (= (seed7-test-indent-02--line-indentation 2) 2))))

;; ---------------------------------------------------------------------------
;; Tests modeled directly on prg/bas7.sd7 Lines 693-707

(defconst seed7-test-indent-02--multiline-return-short-func-correct
  (concat
   "const func boolean: isStringExpr (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "         symbol <> \"\" and\n"
   "         (symbol[length(symbol)] = '$' or symbol[1] = '\\\"' or\n"
   "          not symbol[length(symbol)] in numeric_var_suffix);\n"
   "\n"
   "\n"
   "const func boolean: isStringVar (in string: symbol) is\n"
   "  return symbol in string_var_name;\n")
  "Correctly-indented adjacent short functions with a multi-line `return'.")

(defconst seed7-test-indent-02--multiline-return-short-func-misaligned
  (concat
   "const func boolean: isStringExpr (in string: symbol) is\n"
   "  return symbol in string_var_name or\n"
   "         symbol <> \"\" and\n"
   "         (symbol[length(symbol)] = '$' or symbol[1] = '\\\"' or\n"
   "          not symbol[length(symbol)] in numeric_var_suffix);\n"
   "\n"
   "\n"
   "          const func boolean: isStringVar (in string: symbol) is\n"
   "            return symbol in string_var_name;\n")
  "Misaligned header/return after a multi-line `return' short function,
matching the bug reported against prg/bas7.sd7 lines 693-701.")

(ert-deftest seed7-indent/multiline-return-short-func-keeps-correct-layout ()
  "A `const func' header after a multi-line-`return' short function stays at
column 0, reproducing the layout of prg/bas7.sd7 lines 693-701."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-return-short-func-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 8) 0))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--multiline-return-short-func-correct))))

(ert-deftest seed7-indent/multiline-return-short-func-fixes-misaligned-layout ()
  "Regression test for the bug reported against prg/bas7.sd7 lines 700 and 707:
after a short function whose `return' statement spans multiple continuation
lines, the next `const func' header (and its own `return' line) must be
dedented back to column 0/2 instead of inheriting the deep indentation of the
`return' statement's last continuation line."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-return-short-func-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 8) 0))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--multiline-return-short-func-correct))))

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Tests modeled directly on prg/bas7.sd7 Lines 1655-1661

(defconst seed7-test-indent-02--multiline-forward-decl-correct
  (concat
   "const func string: exec_str_expr (\n"
   "                                  inout string: symbol,\n"
   "                                  inout string: line,\n"
   "                                  inout string: variable_name) is forward;\n"
   "\n"
   "\n"
   "const func string: exec_str_function (in defFnType: defFn, inout string: symbol, inout string: line) is func\n"
   "  result\n"
   "    var string: exprResult is \"\";\n"
   "  begin\n"
   "    exprResult := \"\";\n"
   "  end func;\n")
  "Correctly-indented multi-line forward declaration followed by a func,
matching the layout of prg/bas7.sd7 lines 1655-1661.")

(defconst seed7-test-indent-02--multiline-forward-decl-misaligned
  (concat
   "const func string: exec_str_expr (\n"
   "                                  inout string: symbol,\n"
   "                                  inout string: line,\n"
   "                                  inout string: variable_name) is forward;\n"
   "\n"
   "\n"
   "                                  const func string: exec_str_function (in defFnType: defFn, inout string: symbol, inout string: line) is func\n"
   "                                    result\n"
   "                                      var string: exprResult is \"\";\n"
   "                                    begin\n"
   "                                      exprResult := \"\";\n"
   "                                    end func;\n")
  "Misaligned func header/body after a multi-line forward declaration,
matching the bug reported against prg/bas7.sd7 line 1661: the `const func'
header inherits the deep indentation of the forward declaration's last
continuation line instead of being dedented back to column 0.")

(defun seed7-test-indent-02--normalize-blank-lines (text)
  "Replace whitespace-only lines in TEXT by empty lines."
  (replace-regexp-in-string "^[[:blank:]]+$" "" text))

(defun seed7-test-indent-02--check-multiline-forward-decl-layout ()
  "Assert the expected indentation of the multiline-forward-decl fixture."
  (should (= (seed7-test-indent-02--line-indentation 7) 0))
  (should (= (seed7-test-indent-02--line-indentation 8) 2))
  (should (= (seed7-test-indent-02--line-indentation 9) 4))
  (should (= (seed7-test-indent-02--line-indentation 10) 2))
  (should (= (seed7-test-indent-02--line-indentation 11) 4))
  (should (= (seed7-test-indent-02--line-indentation 12) 2)))

(ert-deftest seed7-indent/multiline-forward-decl-keeps-correct-layout ()
  "A `const func' header after a multi-line forward declaration stays at column 0."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-forward-decl-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-multiline-forward-decl-layout)
    (should
     (string=
      (seed7-test-indent-02--normalize-blank-lines (buffer-string))
      (seed7-test-indent-02--normalize-blank-lines
       seed7-test-indent-02--multiline-forward-decl-correct)))))

(ert-deftest seed7-indent/multiline-forward-decl-fixes-misaligned-layout ()
  "After a multi-line `... is forward;' declaration, the next `const func'
must be dedented back to column 0."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-forward-decl-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-multiline-forward-decl-layout)
    (should
     (string=
      (seed7-test-indent-02--normalize-blank-lines (buffer-string))
      (seed7-test-indent-02--normalize-blank-lines
       seed7-test-indent-02--multiline-forward-decl-correct)))))

;; ---------------------------------------------------------------------------
;; Test Seed7 code that has variables with names that start with 'return'
;; Make sure this does not cause issues in the indentation.

;; ---------------------------------------------------------------------------
;; Regression: variable names that start with the `return' keyword
;; (prg/bas7.sd7 exec_call_key, lines 8793-8938)
;; ---------------------------------------------------------------------------
;;
;; `seed7---inner-callables-2' and `seed7--callable-return-re' used to build
;; their "return ...;" pattern via `(format "return%s;" seed7--any-non-semicolon-re)'
;; with no boundary after the literal "return".  Since
;; `seed7--any-non-semicolon-re' is "[^;]+", this spuriously matched an
;; entire assignment statement like `return_variable := get_name(symbol, line);'
;; as if it were a `return ...;' statement, corrupting the nesting-counting
;; regex used by `seed7-beg-of-defun' / `seed7-to-block-forward' and breaking
;; indentation of every following line in the enclosing block.  The fix adds
;; `\\_>' immediately after "return" in both constants.

(defconst seed7-test-indent-02--return-prefixed-var-correct
  (concat
   "const proc: procWithReturnPrefixedVar (inout string: symbol, inout string: line) is func\n"
   "  local\n"
   "    var string: return_variable is \"\";\n"
   "    var string: status_variable is \"\";\n"
   "  begin\n"
   "    return_variable := get_name(symbol, line);\n"
   "    status_variable := get_name(symbol, line);\n"
   "    if return_variable = status_variable then\n"
   "      writeln(log, \"match\");\n"
   "    elsif return_variable <> \"\" then\n"
   "      writeln(log, \"nonempty\");\n"
   "    end if;\n"
   "  end func;\n")
  "Correctly-indented proc using a `return_variable' local variable,
matching the layout of prg/bas7.sd7 exec_call_key (lines 8793-8938).")

(defconst seed7-test-indent-02--return-prefixed-var-misaligned
  (concat
   "const proc: procWithReturnPrefixedVar (inout string: symbol, inout string: line) is func\n"
   "  local\n"
   "    var string: return_variable is \"\";\n"
   "    var string: status_variable is \"\";\n"
   "  begin\n"
   "    return_variable := get_name(symbol, line);\n"
   "  status_variable := get_name(symbol, line);\n"
   "  if return_variable = status_variable then\n"
   "    writeln(log, \"match\");\n"
   "  elsif return_variable <> \"\" then\n"
   "    writeln(log, \"nonempty\");\n"
   "  end if;\n"
   "  end func;\n")
  "Misaligned proc reproducing the bug: everything after the
`return_variable := ...;' assignment loses 2 columns of indentation,
matching prg/bas7.sd7 lines 8807-8938 before the `\\\\_>' regex fix.")

(defun seed7-test-indent-02--check-return-prefixed-var-layout ()
  "Assert the expected indentation for the return-prefixed-var fixture."
  (should (= (seed7-test-indent-02--line-indentation 1) 0))
  (should (= (seed7-test-indent-02--line-indentation 2) 2))
  (should (= (seed7-test-indent-02--line-indentation 3) 4))
  (should (= (seed7-test-indent-02--line-indentation 4) 4))
  (should (= (seed7-test-indent-02--line-indentation 5) 2))
  (should (= (seed7-test-indent-02--line-indentation 6) 4))
  (should (= (seed7-test-indent-02--line-indentation 7) 4))
  (should (= (seed7-test-indent-02--line-indentation 8) 4))
  (should (= (seed7-test-indent-02--line-indentation 9) 6))
  (should (= (seed7-test-indent-02--line-indentation 10) 4))
  (should (= (seed7-test-indent-02--line-indentation 11) 6))
  (should (= (seed7-test-indent-02--line-indentation 12) 4))
  (should (= (seed7-test-indent-02--line-indentation 13) 2)))

(ert-deftest seed7-indent/return-prefixed-var-keeps-correct-layout ()
  "A `return_variable' local variable must not be mistaken for a `return'
statement by the nesting-counting regexes, reproducing prg/bas7.sd7
exec_call_key's layout when already correctly indented."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-prefixed-var-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-return-prefixed-var-layout)
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-prefixed-var-correct))))

(ert-deftest seed7-indent/return-prefixed-var-fixes-misaligned-layout ()
  "Regression test for prg/bas7.sd7 lines 8807/8808: assigning to a
`return_variable' local must not corrupt indentation of subsequent lines
in the same block, as it did when `seed7---inner-callables-2' and
`seed7--callable-return-re' matched \"return_variable := ...;\" as a
`return ...;' statement."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-prefixed-var-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-return-prefixed-var-layout)
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-prefixed-var-correct))))

;; ---------------------------------------------------------------------------
;; Generalized case: several distinct `return'-prefixed identifiers used in
;; different statement positions (assignment target, condition, expression
;; argument), to ensure the `\\_>' fix is not narrowly specific to the exact
;; identifier `return_variable' or to the assignment position alone.

(defconst seed7-test-indent-02--return-prefixed-vars-multi-correct
  (concat
   "const func string: computeReturnCode (in string: symbol) is func\n"
   "  result\n"
   "    var string: returned_value is \"\";\n"
   "  local\n"
   "    var integer: return_code is 0;\n"
   "  begin\n"
   "    return_code := length(symbol);\n"
   "    if return_code > 0 then\n"
   "      returned_value := str(return_code);\n"
   "    else\n"
   "      returned_value := \"empty\";\n"
   "    end if;\n"
   "    writeln(log, returned_value);\n"
   "  end func;\n")
  "Correctly-indented function using several `return'-prefixed identifiers
(`returned_value', `return_code') as a result variable, a local variable,
an assignment target, a condition operand, and a call argument.")

(defconst seed7-test-indent-02--return-prefixed-vars-multi-misaligned
  (concat
   "const func string: computeReturnCode (in string: symbol) is func\n"
   "  result\n"
   "    var string: returned_value is \"\";\n"
   "  local\n"
   "    var integer: return_code is 0;\n"
   "  begin\n"
   "    return_code := length(symbol);\n"
   "  if return_code > 0 then\n"
   "    returned_value := str(return_code);\n"
   "  else\n"
   "    returned_value := \"empty\";\n"
   "  end if;\n"
   "  writeln(log, returned_value);\n"
   "  end func;\n")
  "Misaligned version reproducing the same class of bug for multiple
`return'-prefixed identifiers (`return_code', `returned_value') rather
than just `return_variable'.")

(defun seed7-test-indent-02--check-return-prefixed-vars-multi-layout ()
  "Assert the expected indentation for the multi-var return-prefixed fixture."
  (should (= (seed7-test-indent-02--line-indentation 1) 0))
  (should (= (seed7-test-indent-02--line-indentation 2) 2))
  (should (= (seed7-test-indent-02--line-indentation 3) 4))
  (should (= (seed7-test-indent-02--line-indentation 4) 2))
  (should (= (seed7-test-indent-02--line-indentation 5) 4))
  (should (= (seed7-test-indent-02--line-indentation 6) 2))
  (should (= (seed7-test-indent-02--line-indentation 7) 4))
  (should (= (seed7-test-indent-02--line-indentation 8) 4))
  (should (= (seed7-test-indent-02--line-indentation 9) 6))
  (should (= (seed7-test-indent-02--line-indentation 10) 4))
  (should (= (seed7-test-indent-02--line-indentation 11) 6))
  (should (= (seed7-test-indent-02--line-indentation 12) 4))
  (should (= (seed7-test-indent-02--line-indentation 13) 4))
  (should (= (seed7-test-indent-02--line-indentation 14) 2)))

(ert-deftest seed7-indent/return-prefixed-vars-multi-keeps-correct-layout ()
  "Several `return'-prefixed identifiers used as result/local variables,
assignment targets, condition operands, and call arguments must not
confuse the `return ...;' nesting-counting regexes when already correctly
indented."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-prefixed-vars-multi-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-return-prefixed-vars-multi-layout)
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-prefixed-vars-multi-correct))))

(ert-deftest seed7-indent/return-prefixed-vars-multi-fixes-misaligned-layout ()
  "Regression test generalizing the `return_variable' bug: `return_code'
and `returned_value' used in several statement positions must not corrupt
indentation of the surrounding `if'/`else'/`end if' block."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--return-prefixed-vars-multi-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (seed7-test-indent-02--check-return-prefixed-vars-multi-layout)
    (should (string= (buffer-string)
                     seed7-test-indent-02--return-prefixed-vars-multi-correct))))

;; ---------------------------------------------------------------------------
;; Test with more keywords

;; ---------------------------------------------------------------------------
;; Regression: variable names prefixed by any Seed7 keyword must not be
;; misidentified as that keyword by boundary-sensitive regexes.
;;
;; This generalizes the `return_variable' bug (prg/bas7.sd7 exec_call_key)
;; to every keyword family in seed7-mode.el that participates in
;; keyword-boundary matching: `seed7--pragma-keywords',
;; `seed7--lead-in-statement-keywords', `seed7-is-statement-keywords',
;; `seed7--in-middle-statement-keywords', `seed7--block-start-keywords',
;; plus "func", "proc", "repeat", "until", and "elsif" (Seed7 does not use
;; the spelling "elseif").
;;
;; For each keyword, an identifier is formed as "<keyword>_suffix" (or
;; "<keyword>Suffix" when the keyword itself ends in a way that would be
;; awkward with an underscore) and used as: a declared variable name, an
;; assignment target, a condition operand, and a call argument -- the same
;; positions that exposed the original `return_variable' bug.

(defconst seed7-test-indent-02--keyword-prefixed-identifiers
  '(;; seed7--pragma-keywords (Line ~1000 of seed7-mode.el)
    "library_name" "message_text" "info_flag" "trace_level"
    "decls_count" "names_list" "syntax_ok" "system_id"
    ;; seed7--lead-in-statement-keywords (Line ~1040)
    "raise_flag" "return_variable"
    ;; seed7-is-statement-keywords (Line ~1071)
    "forward_flag" "DYNAMIC_mode" "new_value" "sub_total" "action_code"
    ;; seed7--in-middle-statement-keywords (Line ~1107)
    "begin_marker" "default_value" "do_work" "downto_value"
    "exception_flag" "fixLen_value" "key_name" "len_value"
    "local_value" "of_type" "otherwise_flag" "param_value"
    "range_value" "result_code" "step_value" "then_flag"
    "to_value" "until_flag"
    ;; seed7--block-start-keywords (Line ~1140)
    "block_data" "case_value" "enum_type" "for_count" "global_var"
    "if_flag" "struct_data" "while_flag"
    ;; Additional keywords explicitly requested
    "func_name" "proc_name" "repeat_count" "elsif_flag")
  "Identifiers formed by suffixing every Seed7 keyword family that
participates in keyword-boundary regex matching in seed7-mode.el, used to
verify that `\\_<'/`\\_>'/whitespace-delimited (or buggy `\\<'/`\\>'/`\\b')
matching never misidentifies a keyword-prefixed identifier as the keyword
itself.")

(defun seed7-test-indent-02--keyword-prefixed-fixture (ident)
  "Return a correctly-indented Seed7 proc body using IDENT as a variable name.

IDENT is used as a declared local variable, an assignment target, a
condition operand, and a `writeln' argument -- the same statement
positions that exposed the original `return_variable' bug."
  (concat
   (format "const proc: procUsing_%s (in string: symbol) is func\n" ident)
   "  local\n"
   (format "    var string: %s is \"\";\n" ident)
   "  begin\n"
   (format "    %s := symbol;\n" ident)
   (format "    if %s <> \"\" then\n" ident)
   (format "      writeln(log, %s);\n" ident)
   "    else\n"
   "      writeln(log, \"empty\");\n"
   "    end if;\n"
   "  end func;\n"))

(defun seed7-test-indent-02--keyword-prefixed-fixture-flat (ident)
  "Return an unindented (column 0) version of the IDENT fixture body."
  (mapconcat #'string-trim
             (split-string (seed7-test-indent-02--keyword-prefixed-fixture ident)
                            "\n")
             "\n"))

(defun seed7-test-indent-02--check-keyword-prefixed-layout ()
  "Assert the expected indentation columns for the IDENT fixture."
  (should (= (seed7-test-indent-02--line-indentation 1) 0))
  (should (= (seed7-test-indent-02--line-indentation 2) 2))
  (should (= (seed7-test-indent-02--line-indentation 3) 4))
  (should (= (seed7-test-indent-02--line-indentation 4) 2))
  (should (= (seed7-test-indent-02--line-indentation 5) 4))
  (should (= (seed7-test-indent-02--line-indentation 6) 4))
  (should (= (seed7-test-indent-02--line-indentation 7) 6))
  (should (= (seed7-test-indent-02--line-indentation 8) 4))
  (should (= (seed7-test-indent-02--line-indentation 9) 6))
  (should (= (seed7-test-indent-02--line-indentation 10) 4))
  (should (= (seed7-test-indent-02--line-indentation 11) 2)))

(ert-deftest seed7-indent/keyword-prefixed-vars-keep-correct-layout ()
  "Variables prefixed by a Seed7 keyword must not lose or gain indentation
when the surrounding code is already correctly indented, for every keyword
in `seed7-test-indent-02--keyword-prefixed-identifiers'."
  (dolist (ident seed7-test-indent-02--keyword-prefixed-identifiers)
    (with-temp-buffer
      (setq-local indent-tabs-mode nil)
      (insert (seed7-test-indent-02--keyword-prefixed-fixture ident))
      (seed7-mode)
      (indent-region (point-min) (point-max))
      (seed7-test-indent-02--check-keyword-prefixed-layout)
      (should (string= (buffer-string)
                       (seed7-test-indent-02--keyword-prefixed-fixture ident))))))

(ert-deftest seed7-indent/keyword-prefixed-vars-fix-misaligned-layout ()
  "Regression test generalizing the `return_variable' bug: for every
keyword in `seed7-test-indent-02--keyword-prefixed-identifiers', a
same-named-prefixed variable used as a declaration, assignment target,
condition operand, and call argument must not corrupt `indent-region'
of the surrounding `if'/`else'/`end if' block when starting from
unindented (column 0) code."
  (dolist (ident seed7-test-indent-02--keyword-prefixed-identifiers)
    (with-temp-buffer
      (setq-local indent-tabs-mode nil)
      (insert (seed7-test-indent-02--keyword-prefixed-fixture-flat ident))
      (seed7-mode)
      (indent-region (point-min) (point-max))
      (seed7-test-indent-02--check-keyword-prefixed-layout)
      (should (string= (buffer-string)
                       (seed7-test-indent-02--keyword-prefixed-fixture ident))))))

;; ---------------------------------------------------------------------------
;; 13. One-line action/primitive declarations must not accumulate indentation.
;;    Reproduces the array.s7i defect: a run of `const proc:'/`const func'
;;    one-liners (each terminated by `;'), interspersed with block comments,
;;    must all stay at the SAME indentation column, and a nested block-
;;    opening callable must indent relative to its true enclosing `begin',
;;    not relative to a preceding one-liner sibling.

(defconst seed7-test-indent-02--action-decls-correct
  (concat
   "const type: arrayType is func\n"
   "  local\n"
   "    ...\n"
   "  begin\n"
   "    const proc: (inout arrayType: dest) := (in arrayType: source) is action \"ARR_CPY\";\n"
   "    const proc: (inout arrayType: dest) &:= (in arrayType: source) is action \"ARR_APPEND\";\n"
   "\n"
   "    (**\n"
   "     *  A block comment between two one-line declarations.\n"
   "     *)\n"
   "    const proc: (inout arrayType: dest) &:= (in baseType: element) is action \"ARR_PUSH\";\n"
   "\n"
   "    const func arrayType: [] (in tupleType: aTuple) is action \"ARR_ARRLIT\";\n"
   "  end func;\n")
  "Correctly-indented run of one-line action declarations inside `begin'.")

(defconst seed7-test-indent-02--action-decls-misaligned
  (concat
   "const type: arrayType is func\n"
   "  local\n"
   "    ...\n"
   "  begin\n"
   "    const proc: (inout arrayType: dest) := (in arrayType: source) is action \"ARR_CPY\";\n"
   "      const proc: (inout arrayType: dest) &:= (in arrayType: source) is action \"ARR_APPEND\";\n"
   "\n"
   "        (**\n"
   "         *  A block comment between two one-line declarations.\n"
   "         *)\n"
   "        const proc: (inout arrayType: dest) &:= (in baseType: element) is action \"ARR_PUSH\";\n"
   "\n"
   "          const func arrayType: [] (in tupleType: aTuple) is action \"ARR_ARRLIT\";\n"
   "  end func;\n")
  "Same declarations, but progressively over-indented, mirroring the
array.s7i defect where each one-liner inherits an extra indent step
from the previous sibling instead of staying aligned under `begin'.")

(ert-deftest seed7-indent/action-decls-keep-correct-layout ()
  "A run of one-line action declarations under `begin' keeps a flat layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--action-decls-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (dolist (line '(5 6 11 13))
      (should (= (seed7-test-indent-02--line-indentation line) 4)))
    (should (string= (buffer-string)
                     seed7-test-indent-02--action-decls-correct))))

(ert-deftest seed7-indent/action-decls-fix-misaligned-layout ()
  "`indent-region' flattens a progressively over-indented run of one-line
action declarations back to the `begin' body column."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--action-decls-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (dolist (line '(5 6 11 13))
      (should (= (seed7-test-indent-02--line-indentation line) 4)))
    (should (string= (buffer-string)
                     seed7-test-indent-02--action-decls-correct))))

(ert-deftest seed7-indent/action-decl-tab-does-not-inherit-drift ()
  "`seed7-indent-line' (TAB) alone on a drifted one-liner restores it to
the `begin' body column, without needing a full region pass."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--action-decls-misaligned)
    (seed7-mode)
    (seed7-test-indent-02--goto-line 6)
    (seed7-indent-line)
    (should (= (seed7-test-indent-02--line-indentation 6) 4))))

;; ---------------------------------------------------------------------------
;; 14. Nested block-opening callable inside `begin', followed by `end func;'.
;; Reproduces the ENABLE_SORT defect at array.s7i Lines 602-610.

(defconst seed7-test-indent-02--nested-callable-in-begin-correct
  (concat
   "const proc: ENABLE_SORT (in type: arrayType) is func\n"
   "  begin\n"
   "    const reference: (attr arrayType) . dataCompare is getobj(x);\n"
   "\n"
   "    const func arrayType: SORT (in arrayType: arr, in reference: dataCompare) is action \"ARR_SORT\";\n"
   "\n"
   "    const func arrayType: sort (in arrayType: arr_obj) is\n"
   "      return SORT(arr_obj, arrayType.dataCompare);\n"
   "  end func;\n")
  "Correctly-indented ENABLE_SORT shape: nested `sort' aligns with `SORT'
under `begin' (column 4); outer `end func;' dedents to `begin' level
(column 2).")

(defconst seed7-test-indent-02--nested-callable-in-begin-misaligned
  (concat
   "const proc: ENABLE_SORT (in type: arrayType) is func\n"
   "  begin\n"
   "    const reference: (attr arrayType) . dataCompare is getobj(x);\n"
   "\n"
   "    const func arrayType: SORT (in arrayType: arr, in reference: dataCompare) is action \"ARR_SORT\";\n"
   "\n"
   "             const func arrayType: sort (in arrayType: arr_obj) is\n"
   "               return SORT(arr_obj, arrayType.dataCompare);\n"
   "             end func;\n")
  "Misaligned version reproducing the reported array.s7i Lines 608/610 defect:
`sort' and its `end func;' drift far to the right instead of aligning
with `SORT' (col 4) and `begin' (col 2) respectively.")

(ert-deftest seed7-indent/nested-callable-in-begin-keeps-correct-layout ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-callable-in-begin-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-callable-in-begin-correct))))

(ert-deftest seed7-indent/nested-callable-in-begin-fixes-misaligned-layout ()
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-callable-in-begin-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 7) 4))
    (should (= (seed7-test-indent-02--line-indentation 8) 6))
    (should (= (seed7-test-indent-02--line-indentation 9) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-callable-in-begin-correct))))

(ert-deftest seed7-indent/nested-callable-end-func-tab-dedents ()
  "TAB alone on the drifted `end func;' line dedents it to `begin' level."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-callable-in-begin-misaligned)
    (seed7-mode)
    (seed7-test-indent-02--goto-line 9)
    (seed7-indent-line)
    (should (= (seed7-test-indent-02--line-indentation 9) 2))))

(ert-deftest seed7-indent/multiline-one-liner-decl-does-not-open-block ()
  "A one-line action declaration whose parameter list wraps onto a
second physical line (e.g. array.s7i's `insert'/ARR_RANGE shape) must
not be treated as opening a compound block, and must not corrupt the
indentation of declarations that follow it."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert (concat
             "const type: arrayType is func\n"
             "  begin\n"
             "    const proc: insert (inout arrayType: arr, in integer: index,\n"
             "                        in baseType: element) is action \"ARR_INSERT\";\n"
             "\n"
             "    const proc: append (inout arrayType: arr) is action \"ARR_APPEND\";\n"
             "  end func;\n"))
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 24)) ; continuation, adjust if a different rule applies
    (should (= (seed7-test-indent-02--line-indentation 6) 4))
    (should (= (seed7-test-indent-02--line-indentation 7) 2))))

;; ---------------------------------------------------------------------------
;; 15. Struct member-list definition: `const type: X is new struct ... end struct;'
;; ----------------------------------------------------------------------------
;;
;;   const type: defFnType is new struct     ; col 0
;;     var string: name is "";                ; col 4
;;     var string: params is "";               ; col 4
;;     var string: expression is "";           ; col 4
;;   end struct;                               ; col 2

(defconst seed7-test-indent-02--struct-correct
  (concat
   "const type: defFnType is new struct\n"
   "    var string: name is \"\";\n"
   "    var string: params is \"\";\n"
   "    var string: expression is \"\";\n"
   "  end struct;\n")
  "Correctly-indented struct member-list fixture.")

(defconst seed7-test-indent-02--struct-misaligned
  (concat
   "const type: defFnType is new struct\n"
   "var string: name is \"\";\n"
   "var string: params is \"\";\n"
   "var string: expression is \"\";\n"
   "end struct;\n")
  "Misaligned struct member-list fixture.")

(ert-deftest seed7-indent/struct-keeps-correct-layout ()
  "Indenting an already-correct struct definition keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 4))
    (should (= (seed7-test-indent-02--line-indentation 5) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-correct))))

(ert-deftest seed7-indent/struct-fixes-misaligned-layout ()
  "Indenting a misaligned struct definition (via `indent-region') restores
the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 4))
    (should (= (seed7-test-indent-02--line-indentation 5) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-correct))))

(ert-deftest seed7-indent/struct-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line of a misaligned struct
definition (simulating manual <TAB> on each line) restores the
expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 4))
    (should (= (seed7-test-indent-02--line-indentation 4) 4))
    (should (= (seed7-test-indent-02--line-indentation 5) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-correct))))

;; ---------------------------------------------------------------------------

(defconst seed7-test-indent-02--struct-sub-correct
  (concat
   "const type: aesState is sub noCipherState struct\n"
   "    var array bin32: encryptionSubKey is 0 times bin32.value;\n"
   "    var array bin32: decryptionSubKey is 0 times bin32.value;\n"
   "    var integer: rounds is 0;\n"
   "    var string: cipherBlock is \"\";\n"
   "  end struct;\n"))

(defconst seed7-test-indent-02--struct-sub-misaligned
  (concat
   "const type: aesState is sub noCipherState struct\n"
   "var array bin32: encryptionSubKey is 0 times bin32.value;\n"
   "var array bin32: decryptionSubKey is 0 times bin32.value;\n"
   "var integer: rounds is 0;\n"
   "var string: cipherBlock is \"\";\n"
   "end struct;\n"))

(ert-deftest seed7-indent/struct-sub-keeps-correct-layout ()
  "Indenting an already-correct `is sub ... struct' definition keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-sub-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-sub-correct))))

(ert-deftest seed7-indent/struct-sub-fixes-misaligned-layout ()
  "Indenting a misaligned `is sub ... struct' definition restores the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-sub-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 5) 4))
    (should (= (seed7-test-indent-02--line-indentation 6) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-sub-correct))))

(ert-deftest seed7-indent/struct-sub-fixes-misaligned-layout-line-by-line ()
  "`seed7-indent-line' on each line restores a misaligned `is sub ... struct'."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--struct-sub-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (string= (buffer-string)
                     seed7-test-indent-02--struct-sub-correct))))

;; ---------------------------------------------------------------------------
;; 16. Enum member-list definition: `const type: X is new enum ... end enum;'
;; ----------------------------------------------------------------------------
;;
;;   const type: color is new enum      ; col 0
;;     red, green, blue                  ; col 4
;;   end enum;                           ; col 2

(defconst seed7-test-indent-02--enum-correct
  (concat
   "const type: color is new enum\n"
   "    red, green, blue\n"
   "  end enum;\n")
  "Correctly-indented enum member-list fixture.")

(defconst seed7-test-indent-02--enum-misaligned
  (concat
   "const type: color is new enum\n"
   "red, green, blue\n"
   "end enum;\n")
  "Misaligned enum member-list fixture.")

(ert-deftest seed7-indent/enum-keeps-correct-layout ()
  "Indenting an already-correct enum definition keeps the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-correct))))

(ert-deftest seed7-indent/enum-fixes-misaligned-layout ()
  "Indenting a misaligned enum definition (via `indent-region') restores
the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-correct))))

(ert-deftest seed7-indent/enum-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line of a misaligned enum
definition (simulating manual <TAB> on each line) restores the
expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-correct))))

;; ---------------------------------------------------------------------------
;; 17. Enum inheritance-style member-list definition:
;;     `const type: X is sub BaseType enum ... end enum;'
;; ----------------------------------------------------------------------------
;;
;;   const type: extendedColor is sub color enum   ; col 0
;;     yellow, orange, purple                       ; col 4
;;   end enum;                                       ; col 2

(defconst seed7-test-indent-02--enum-sub-correct
  (concat
   "const type: extendedColor is sub color enum\n"
   "    yellow, orange, purple\n"
   "  end enum;\n")
  "Correctly-indented inheritance-style enum member-list fixture.")

(defconst seed7-test-indent-02--enum-sub-misaligned
  (concat
   "const type: extendedColor is sub color enum\n"
   "yellow, orange, purple\n"
   "end enum;\n")
  "Misaligned inheritance-style enum member-list fixture.")

(ert-deftest seed7-indent/enum-sub-keeps-correct-layout ()
  "Indenting an already-correct inheritance-style enum definition keeps
the layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-sub-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-sub-correct))))

(ert-deftest seed7-indent/enum-sub-fixes-misaligned-layout ()
  "Indenting a misaligned inheritance-style enum definition (via
`indent-region') restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-sub-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-sub-correct))))

(ert-deftest seed7-indent/enum-sub-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line of a misaligned
inheritance-style enum definition (simulating manual <TAB> on each
line) restores the expected layout."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--enum-sub-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))
    (should (= (seed7-test-indent-02--line-indentation 2) 4))
    (should (= (seed7-test-indent-02--line-indentation 3) 2))
    (should (string= (buffer-string)
                     seed7-test-indent-02--enum-sub-correct))))

;; ---------------------------------------------------------------------------
;; 18. Func declaration with trailing whitespace + comment after `is func',
;;     and indentation of the declaration that follows the closing `end func;'
;; ----------------------------------------------------------------------------
;;
;; NOTE: line 1 below intentionally ends with `is func <SPACE>#' — trailing
;; whitespace followed by a `#' comment.  This must be preserved verbatim
;; (many editors strip trailing whitespace on save; the `#' after the space
;; keeps it non-trailing in the source, so it survives such cleanups).

(defconst seed7-test-indent-02--func-trailing-comment-correct
  (concat
   "const func string: gcmMult (in string: factor1, in factorHType: factorH) is func #\n"
   "  result\n"
   "    var string: product is \"\";\n"
   "  local\n"
   "    var integer: index is 0;\n"
   "  begin\n"
   "    index := 1;\n"
   "  end func;\n"
   "\n"
   "const proc: main is func\n"
   "  begin\n"
   "    writeln(\"done\");\n"
   "  end func;\n")
  "Correctly-indented fixture: `is func' line has trailing ` #' comment.
The second declaration (`const proc: main is func') must be indented at
column 0, verifying that the following declaration is not corrupted by
the preceding trailing-whitespace/comment `is func' line.")

(defconst seed7-test-indent-02--func-trailing-comment-misaligned
  (concat
   "const func string: gcmMult (in string: factor1, in factorHType: factorH) is func #\n"
   "result\n"
   "var string: product is \"\";\n"
   "local\n"
   "var integer: index is 0;\n"
   "begin\n"
   "index := 1;\n"
   "end func;\n"
   "\n"
   "const proc: main is func\n"
   "begin\n"
   "writeln(\"done\");\n"
   "end func;\n")
  "Misaligned counterpart of `seed7-test-indent-02--func-trailing-comment-correct'.")

(ert-deftest seed7-indent/func-trailing-comment-keeps-correct-layout ()
  "A `is func' line followed by trailing whitespace and a `#' comment must
not break indentation of its own body nor of the declaration that follows."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--func-trailing-comment-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 1) 0))   ; declaration
    (should (= (seed7-test-indent-02--line-indentation 2) 2))   ; result
    (should (= (seed7-test-indent-02--line-indentation 3) 4))   ; var string
    (should (= (seed7-test-indent-02--line-indentation 4) 2))   ; local
    (should (= (seed7-test-indent-02--line-indentation 5) 4))   ; var integer
    (should (= (seed7-test-indent-02--line-indentation 6) 2))   ; begin
    (should (= (seed7-test-indent-02--line-indentation 7) 4))   ; index := 1;
    (should (= (seed7-test-indent-02--line-indentation 8) 2))   ; end func;
    (should (= (seed7-test-indent-02--line-indentation 10) 0))  ; const proc: main
    (should (= (seed7-test-indent-02--line-indentation 11) 2))  ; begin
    (should (= (seed7-test-indent-02--line-indentation 12) 4))  ; writeln(...)
    (should (= (seed7-test-indent-02--line-indentation 13) 2))  ; end func;
    (should (string= (buffer-string)
                     seed7-test-indent-02--func-trailing-comment-correct))))

(ert-deftest seed7-indent/func-trailing-comment-fixes-misaligned-layout ()
  "`indent-region' must restore correct indentation even when the earlier
`is func' line has trailing whitespace + a `#' comment."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--func-trailing-comment-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string)
                     seed7-test-indent-02--func-trailing-comment-correct))))

(ert-deftest seed7-indent/func-trailing-comment-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line (simulating manual <TAB>) must
also restore correct indentation, including for the declaration following
`end func;'."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--func-trailing-comment-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (string= (buffer-string)
                     seed7-test-indent-02--func-trailing-comment-correct))))

;; ---------------------------------------------------------------------------
;; 19. `if' condition continuation lines containing a nested `:=' (declare-
;;     and-assign inside a function call argument) must not be mistaken for
;;     a statement-level assignment continuation.
;; ---------------------------------------------------------------------------

(defconst seed7-test-indent-02--nested-assign-in-logic-correct
  (concat
   "const proc: main is func\n"
   "  local\n"
   "    var string: stri is \"\";\n"
   "    var boolean: okay is TRUE;\n"
   "  begin\n"
   "    if not raisesIndexError(stri := \"\" [ .. integer.first ]) or\n"
   "       not raisesIndexError(stri := \"\" [ ..            -5 ]) or\n"
   "       not raisesIndexError(stri := \"\" [ ..            -2 ]) then\n"
   "      writeln(\" ***** stri := STRING [ .. STOP ] does not work correctly. (1)\");\n"
   "      okay := FALSE;\n"
   "    end if;\n"
   "  end func;\n")
  "Correctly-indented fixture: `if' condition continuation lines each
contain a nested `:=' inside a function call argument list (a Seed7
declare-and-assign sub-expression), not a statement-level assignment.")

(defconst seed7-test-indent-02--nested-assign-in-logic-misaligned
  (concat
   "const proc: main is func\n"
   "local\n"
   "var string: stri is \"\";\n"
   "var boolean: okay is TRUE;\n"
   "begin\n"
   "if not raisesIndexError(stri := \"\" [ .. integer.first ]) or\n"
   "not raisesIndexError(stri := \"\" [ ..            -5 ]) or\n"
   "not raisesIndexError(stri := \"\" [ ..            -2 ]) then\n"
   "writeln(\" ***** stri := STRING [ .. STOP ] does not work correctly. (1)\");\n"
   "okay := FALSE;\n"
   "end if;\n"
   "end func;\n")
  "Un-indented `seed7-test-indent-02--nested-assign-in-logic-correct'.")

(ert-deftest seed7-indent/nested-assign-in-logic-keeps-correct-layout ()
  "A nested `:=' inside a function-call argument on an `if' condition
continuation line must not trigger assignment-statement-continuation
alignment, nor cause runaway/compounding indentation."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-assign-in-logic-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 6) 4))   ; if ...
    (should (= (seed7-test-indent-02--line-indentation 7) 7))   ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 8) 7))   ; not ... then
    (should (= (seed7-test-indent-02--line-indentation 9) 6))   ; writeln(...)
    (should (= (seed7-test-indent-02--line-indentation 10) 6))  ; okay := FALSE;
    (should (= (seed7-test-indent-02--line-indentation 11) 4))  ; end if;
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-assign-in-logic-correct))))

(ert-deftest seed7-indent/nested-assign-in-logic-fixes-misaligned-layout ()
  "`indent-region' must restore correct indentation for `if' condition
continuation lines that each contain a nested `:=', without compounding
drift to the right."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-assign-in-logic-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-assign-in-logic-correct))))

(ert-deftest seed7-indent/nested-assign-in-logic-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line (simulating manual <TAB>) must
also restore correct indentation for this pattern."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--nested-assign-in-logic-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (string= (buffer-string)
                     seed7-test-indent-02--nested-assign-in-logic-correct))))

;; ---------------------------------------------------------------------------
;; 20. `if' condition continuation lines using `=' comparisons with nested
;;     `[ .. ]' string slicing and `intExpr(...)' function calls, preceded
;;     by an unrelated top-level `:=' assignment statement.
;; ---------------------------------------------------------------------------

(defconst seed7-test-indent-02--slice-compare-eq-correct
  (concat
   "const proc: main is func\n"
   "  local\n"
   "    var string: stri is \"\";\n"
   "    var boolean: okay is TRUE;\n"
   "  begin\n"
   "    stri := \"1234567890\";\n"
   "    if not raisesIndexError(stri[ .. integer.first ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ ..            -5 ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ ..            -2 ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ ..            -1 ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ .. intExpr(integer.first) ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ .. intExpr(           -5) ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ .. intExpr(           -2) ] = \"abcdefgh\") or\n"
   "       not raisesIndexError(stri[ .. intExpr(           -1) ] = \"abcdefgh\") then\n"
   "      writeln(\" ***** STRING [ .. STOP ] does not work correctly. (9)\");\n"
   "      okay := FALSE;\n"
   "    end if;\n"
   "  end func;\n")
  "Correctly-indented fixture: `if' continuation lines use `=' comparisons
against nested `[ .. ]' string slices and `intExpr(...)' calls, with a
single space after `if' so continuation lines align at column 7
(if-column 4 + \"if\" length 2 + 1). The preceding `stri := \"1234567890\";'
statement is an unrelated top-level `:=' assignment and must not affect
alignment of the `if' continuation lines below.")

(defconst seed7-test-indent-02--slice-compare-eq-misaligned
  (concat
   "const proc: main is func\n"
   "local\n"
   "var string: stri is \"\";\n"
   "var boolean: okay is TRUE;\n"
   "begin\n"
   "stri := \"1234567890\";\n"
   "if not raisesIndexError(stri[ .. integer.first ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ ..            -5 ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ ..            -2 ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ ..            -1 ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ .. intExpr(integer.first) ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ .. intExpr(           -5) ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ .. intExpr(           -2) ] = \"abcdefgh\") or\n"
   "not raisesIndexError(stri[ .. intExpr(           -1) ] = \"abcdefgh\") then\n"
   "writeln(\" ***** STRING [ .. STOP ] does not work correctly. (9)\");\n"
   "okay := FALSE;\n"
   "end if;\n"
   "end func;\n")
  "Misaligned counterpart of `seed7-test-indent-02--slice-compare-eq-correct'.
Only leading whitespace differs from the `-correct' fixture; mid-line
spacing after the first non-blank character on each line is identical,
since `seed7-mode' indentation logic never rewrites it.")

(ert-deftest seed7-indent/slice-compare-eq-keeps-correct-layout ()
  "Nested `[ .. ]' slices and `intExpr(...)' calls combined with `='
comparisons on `if' continuation lines (single space after `if') must
keep the stable logic-check-expression column 7, unaffected by an
earlier unrelated `:=' statement."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--slice-compare-eq-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 6) 4))   ; stri := "1234567890";
    (should (= (seed7-test-indent-02--line-indentation 7) 4))   ; if ...
    (should (= (seed7-test-indent-02--line-indentation 8) 7))   ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 9) 7))   ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 10) 7))  ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 11) 7))  ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 12) 7))  ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 13) 7))  ; not ... or
    (should (= (seed7-test-indent-02--line-indentation 14) 7))  ; not ... then
    (should (= (seed7-test-indent-02--line-indentation 15) 6))  ; writeln(...)
    (should (= (seed7-test-indent-02--line-indentation 16) 6))  ; okay := FALSE;
    (should (= (seed7-test-indent-02--line-indentation 17) 4))  ; end if;
    (should (string= (buffer-string)
                     seed7-test-indent-02--slice-compare-eq-correct))))

(ert-deftest seed7-indent/slice-compare-eq-fixes-misaligned-layout ()
  "`indent-region' must restore correct indentation for `if' continuation
lines using `=' comparisons against nested `[ .. ]' slices and
`intExpr(...)' calls."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--slice-compare-eq-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string)
                     seed7-test-indent-02--slice-compare-eq-correct))))

(ert-deftest seed7-indent/slice-compare-eq-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line (simulating manual <TAB>) must
also restore correct indentation for this pattern."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--slice-compare-eq-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (string= (buffer-string)
                     seed7-test-indent-02--slice-compare-eq-correct))))

;; ---------------------------------------------------------------------------
;; 21. Top-level `:=' assignment whose RHS is a multi-line function call:
;;     every continuation line remains inside the still-open parens of the
;;     call and must align under the column following the open paren, not
;;     under the column following `:='.  Based on the `aTime := time(...)'
;;     statement in Seed7's lib/x509cert.s7i.
;; ---------------------------------------------------------------------------

(defconst seed7-test-indent-02--multiline-call-arg-in-assign-correct
  (concat
   "const proc: main is func\n"
   "  local\n"
   "    var string: stri is \"20260713100000Z\";\n"
   "    var time: aTime is time.value;\n"
   "  begin\n"
   "    aTime := time(year,\n"
   "                  integer(stri[ 3 fixLen 2]),  # month\n"
   "                  integer(stri[ 5 fixLen 2]),  # day\n"
   "                  integer(stri[ 7 fixLen 2]),  # hour\n"
   "                  integer(stri[ 9 fixLen 2]),  # minute\n"
   "                  integer(stri[11 fixLen 2])); # second\n"
   "  end func;\n")
  "Correctly-indented fixture: a top-level `:=' assignment whose RHS is a
multi-line function call.  Every continuation line is inside the still-open
parens of the `time(...)' call and must align under the column following
`time(', not under the column following `:='.")

(defconst seed7-test-indent-02--multiline-call-arg-in-assign-misaligned
  (concat
   "const proc: main is func\n"
   "local\n"
   "var string: stri is \"20260713100000Z\";\n"
   "var time: aTime is time.value;\n"
   "begin\n"
   "aTime := time(year,\n"
   "integer(stri[ 3 fixLen 2]),  # month\n"
   "integer(stri[ 5 fixLen 2]),  # day\n"
   "integer(stri[ 7 fixLen 2]),  # hour\n"
   "integer(stri[ 9 fixLen 2]),  # minute\n"
   "integer(stri[11 fixLen 2])); # second\n"
   "end func;\n")
  "Un-indented counterpart of
`seed7-test-indent-02--multiline-call-arg-in-assign-correct'.")

(ert-deftest seed7-indent/multiline-call-arg-in-assign-keeps-correct-layout ()
  "Every continuation line of the multi-line `time(...)' call on the RHS of
the top-level `aTime := ...' assignment must align under the column
following the open paren of `time(', not under the column following `:='."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-call-arg-in-assign-correct)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (= (seed7-test-indent-02--line-indentation 6) 4))    ; aTime := time(year,
    (should (= (seed7-test-indent-02--line-indentation 7) 18))   ; integer(...) # month
    (should (= (seed7-test-indent-02--line-indentation 8) 18))   ; integer(...) # day
    (should (= (seed7-test-indent-02--line-indentation 9) 18))   ; integer(...) # hour
    (should (= (seed7-test-indent-02--line-indentation 10) 18))  ; integer(...) # minute
    (should (= (seed7-test-indent-02--line-indentation 11) 18))  ; integer(...) # second
    (should (= (seed7-test-indent-02--line-indentation 12) 2))   ; end func;
    (should (string= (buffer-string)
                     seed7-test-indent-02--multiline-call-arg-in-assign-correct))))

(ert-deftest seed7-indent/multiline-call-arg-in-assign-fixes-misaligned-layout ()
  "`indent-region' must restore correct indentation for a multi-line
function-call argument list appearing on the RHS of a top-level `:='
assignment, without any continuation line drifting to the assignment
operator's column."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-call-arg-in-assign-misaligned)
    (seed7-mode)
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string)
                     seed7-test-indent-02--multiline-call-arg-in-assign-correct))))

(ert-deftest seed7-indent/multiline-call-arg-in-assign-fixes-misaligned-layout-line-by-line ()
  "Calling `seed7-indent-line' on each line (simulating manual <TAB>) must
also restore correct indentation for this pattern."
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (insert seed7-test-indent-02--multiline-call-arg-in-assign-misaligned)
    (seed7-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (seed7-indent-line)
      (forward-line 1))
    (should (string= (buffer-string)
                     seed7-test-indent-02--multiline-call-arg-in-assign-correct))))

;; ---------------------------------------------------------------------------
(provide 'seed7-test-indent-02)

;;; seed7-test-indent-02.el ends here
