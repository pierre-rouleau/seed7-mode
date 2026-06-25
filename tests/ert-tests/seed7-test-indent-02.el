;;; seed7-test-indent-02.el --- Comprehensive ERT tests for Seed7 indentation  -*- lexical-binding: t; -*-

;; Author    : Pierre Rouleau <prouleau001@gmail.com>
;; Time-stamp: <2026-06-25 20:00:00 EDT, updated by Pierre Rouleau>

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
(provide 'seed7-test-indent-02)

;;; seed7-test-indent-02.el ends here