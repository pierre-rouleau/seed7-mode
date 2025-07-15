===========================================================================
Analysis of Emacs basic functions required to implement mark-defun properly
===========================================================================

:Home URL:
:Project:

:Created:  Monday, July 14 2025.

:Author:  Pierre Rouleau <prouleau001@gmail.com>
:Modified: 2025-07-14 12:47:14, by Pierre Rouleau.

:Copyright: © 2025, Pierre Rouleau


.. contents::  **Table of Contents**
.. sectnum::

.. ---------------------------------------------------------------------------



..
   Let me review what end-of-defun-function does on my system.
   Let me know if my understanding is wrong.

Overview
========



Analysis of functionality required by end-of-defun-function
-----------------------------------------------------------

The definition of ``end-of-defun-function`` on lisp.e on my Emacs 30.1 is
starting at line 522 of lisp.el:

.. code:: emacs-lisp

  (defvar end-of-defun-function
    (lambda () (forward-sexp 1))
    "Function for `end-of-defun' to call.
  This is used to find the end of the defun at point.
  It is called with no argument, right after calling `beginning-of-defun-raw'.
  So the function can assume that point is at the beginning of the defun body.
  It should move point to the first position after the defun.")

The disassembled code I see is::

  byte code:
    args: nil
  0       constant  forward-sexp
  1       constant  1
  2       call      1
  3       return

That's essentially ``(forward-sexp 1)``
Therefore I should be able to eval-expression the following 2
- ``(funcall end-of-defun-function)`` , or
-  ``(forward-sexp 1)``,

and get the same results in an emacs-lisp-mode buffer, and that's the case:
starting from:

- the opening parens of a defun,
- any empty line above the opening parens of the same defun,
- Any comment line just above the defun,

it lands at the closing parens.

However, if I eval-expression ``(end-of-defun)`` from the opening parens of a defun
then point moves to the beginning of the next line following the closing parens
of the defun.

The extra functionality of moving to the beginning of the next line is
implemented by the ``end-of-defun`` function which uses the
end-of-defun-function.

I conclude that the function assigned to the end-of-defun-function variable is
supposed to move point to the very end of the function, not the next line,
when invoked at the beginning of the function or just above.


Analysis of functionality required by beginning-of-defun-function
-----------------------------------------------------------------

The default value for ``beginning-of-defun-function`` *and* its value in
emacs-lisp-mode buffers is nil, therefore I cannot test the default behaviour
and need to rely on its docstring, which says::

  The function takes the same argument as `beginning-of-defun' and should
  behave similarly, returning non-nil if it found the beginning of a defun.
  Ideally it should move to a point right before an open-paren which encloses
  the body of the defun.


Checking the behaviour of seed7-mode functions assigned to the variables
------------------------------------------------------------------------

The seed7-mode assigns the following:

- end-of-defun-function        set to: seed7-beg-of-defun
- beginning-of-defun-function  set to: seed7-end-of-defun

The signature of these seed7-mode functions are::

  (defun seed7-beg-of-defun (&optional n silent dont-push-mark)
  (defun seed7-end-of-defun (&optional n silent dont-push-mark)


The signatures of the lisp.el functions are::

  (defun end-of-defun (&optional arg interactive)
  (defun beginning-of-defun (&optional arg)

With no arguments, the seed7 functions interpret their ``n`` parameter
as the lisp ``arg`` parameter:

- nil means the current defun (as 1)
- 2 or larger means 2 or more repetitions of the operation.
- A negative value means using the other function with ``(abs n)``
  as the argument.


Testing the behaviour of ``seed7-beg-of-defun`` and ``seed7-end-of-defun``
on Seed7 code, I see the exact corresponding behaviour that ``beginning-of-defun``
and ``end-of-defun`` (or ``(forward-sexp 1)``) have on elisp code.

I also test with ``beginning-of-defun`` and ``end-of-defun`` in Seed7 code
and see the same behaviour.


What else must I verify to ensure that ``seed7-beg-of-defun`` and
``seed7-end-of-defun`` are appropriate to be assigned to the variables
and can make ``mark-defun`` work properly for Seed7 code?



.. ---------------------------------------------------------------------------

Identifying the lisp.el variables used by Emacs navigation code
===============================================================

The first step in the analysis is to identify the value of the various
variables used by the lisp.el file, the file that holds  the logic for
``end-of-defun``, ``beginning-of-defun`` and ``mark-defun``.

- I wrote code to print their values see by a major mode.
  That code is the ``pel-show-lisp-control-variables`` command from
  the `pel-emacs-analyze.el`_ file, which is part of my `PEL project`_.
- I executed the commands on several files using major modes for Emacs Lisp,
  Python, C and Seed7 for comparison purposes.
- I also wrote simple example code files that can be used to test the
  bahaviour of the commands on various major modes. These files are part of
  this project.  They are:

  - `c-functions.c`_
  - `elisp-functions.el`_
  - `python-functions.py`_
  - `seed7-functions.sd7`_

The Test source code files
--------------------------

The following sub-sections show the content of the test code files, along with
the line numbers for convenience.

The Elisp test file: elisp-functions.el
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:Ref: `elisp-functions.el`_

.. code:: elisp

   1 ;; Emacs Lisp Example Code Used For Tests
   2
   3 (defun fct-b ()
   4   "First test function."
   5   (message "fct-b"))
   6
   7
   8 (defun fct-b ()
   9   "First test function."
  10   (message "fct-b"))
  11
  12
  13 ;; comment before fct-c
  14 (defun fct-c ()
  15   "First test function."
  16   (message "fct-c"))
  17
  18
  19 ;; comment before fct-d
  20
  21 (defun fct-d ()
  22   "First test function."
  23   (message "fct-d"))
  24
  25
  26 ;; comment line 1 before (fct-e)
  27 ;; comment line 2 before fct-e
  28 ;; comment line 3 before fct-e
  29
  30
  31 (defun fct-e ()
  32   "First test function."
  33   (message "fct-e"))
  34
  35 ;; -----last line (line 35) ---------------------------------------------------


The C test file: c-functions.c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:Ref: `c-functions.c`_

.. code:: c

     1 #include <stdio.h> /* C example code for Tests */
     2
     3 void greet_1() {
     4     printf("Bonjour\n");
     5 }
     6
     7
     8 void greet_2() {
     9     printf("Buongiorno\n");
    10 }
    11
    12
    13 /* comment before function 3 */
    14 void greet_3() {
    15     printf("Guten tag\n");
    16 }
    17
    18
    19 /* comment before function 4 */
    20
    21 void greet_4() {
    22     printf("Buen día\n");
    23 }
    24
    25
    26 /* comment line 1 before function 5 */
    27 /* comment line 2 before function 5
    28  * followed by another one */
    29
    30
    31 void greet_5() {
    32     printf("Bom dia\n");
    33 }
    34
    35 /* --------last line (line 35) ---------------------------------------------- */


The Python test file: python-functions.py
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:Ref: `python-functions.py`_

.. code:: python

     1 # Python Example Code for Tests
     2
     3 def greet_1(text='Bonjour'):
     4     """ Print greeting."""
     5     print("%s\n" % text)
     6
     7
     8 def greet_2(text='Buongiorno'):
     9     """ Print greeting."""
    10     print("%s\n" % text)
    11
    12
    13 # comment before function 3
    14 def greet_3(text='Guten morgen'):
    15     """ Print greeting."""
    16     print("%s\n" % text)
    17
    18
    19 # comment before function 4
    20
    21 def greet_4(text='Buen día'):
    22     """ Print greeting."""
    23     print("%s\n" % text)
    24
    25
    26 # comment line 1 before function 5
    27 # comment line 2 before function 5
    28 # comment line 3 before function 5
    29
    30
    31 def greet_5(text='Bom dia'):
    32     """ Print greeting."""
    33     print("%s\n" % text)
    34
    35 # ----------last line (line 35) ----------------------------------------------


The Seed7 test file: seed7-functions.sd7
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:Ref: `seed7-functions.sd7`_

.. code:: pascal

     1 # Seed7 Example Code Used For Tests
     2
     3 $ include "seed7_05.s7i";
     4   include "float.s7i";
     5
     6 const func boolean: f1_flipCoin is
     7   return rand(FALSE, TRUE);
     8
     9
    10 const func boolean:  f2_flipCoin is func
    11   result
    12     var boolean: coinState is FALSE;
    13   begin
    14     coinState := rand(FALSE, TRUE);
    15   end func;
    16
    17 # function 3: inverse
    18 const func float: f3_inverse (in float: number) is
    19   return 1.0 // number;
    20
    21 #  function 4: power
    22
    23 const func float: f4_power (in float: number, in integer: base) is
    24   return number ** base;
    25
    26
    27 #  function 5: log 10 of power
    28 #  with 3 comment lines
    29 #  before it.
    30
    31
    32 const func float: f4_log10_of_power (in float: number, in integer: base) is
    33   return log10(number ** base);
    34
    35 # --------last line (line 35) ------------------------------------------------


The lisp.el variables used by Emacs
-----------------------------------

The following sections contain what ``pel-show-lisp-control-variables`` prints
on various versions of Emacs for elisp, C, Python and Seed7 files.

The lisp.el variables on Emacs 30.1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  ----lisp.el control variables (Emacs GNU Emacs 30.1 (build 1, aarch64-apple-darwin23.6.0)
   of 2025-02-23) from elisp-functions.el --- Monday, July 14, 2025 @ 16:34:18 -----

  Buffer's major mode : emacs-lisp-mode

  User options:
  - defun-prompt-regexp                     : nil
  - parens-require-spaces                   : t
  - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
  - delete-pair-blink-delay                 : 1

  Variables:
  - forward-sexp-function                   : nil
  - beginning-of-defun-function             : nil
  - end-of-defun-function                   : #[0 "\300\301!\207" [forward-sexp 1] 2]
  - end-of-defun-moves-to-eol               : t
  - narrow-to-defun-include-comments        : nil

  ----lisp.el control variables (Emacs GNU Emacs 30.1 (build 1, aarch64-apple-darwin23.6.0)
   of 2025-02-23) from python-functions.py --- Monday, July 14, 2025 @ 16:34:27 -----

  Buffer's major mode : python-mode

  User options:
  - defun-prompt-regexp                     : nil
  - parens-require-spaces                   : t
  - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
  - delete-pair-blink-delay                 : 1

  Variables:
  - forward-sexp-function                   : python-nav-forward-sexp
  - beginning-of-defun-function             : python-nav-beginning-of-defun
  - end-of-defun-function                   : python-nav-end-of-defun
  - end-of-defun-moves-to-eol               : t
  - narrow-to-defun-include-comments        : nil

  ----lisp.el control variables (Emacs GNU Emacs 30.1 (build 1, aarch64-apple-darwin23.6.0)
   of 2025-02-23) from c-functions.c --- Monday, July 14, 2025 @ 16:34:55 -----

  Buffer's major mode : c-mode

  User options:
  - defun-prompt-regexp                     : nil
  - parens-require-spaces                   : nil
  - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
  - delete-pair-blink-delay                 : 1

  Variables:
  - forward-sexp-function                   : nil
  - beginning-of-defun-function             : c-beginning-of-defun
  - end-of-defun-function                   : c-end-of-defun
  - end-of-defun-moves-to-eol               : t
  - narrow-to-defun-include-comments        : nil

  ----lisp.el control variables (Emacs GNU Emacs 30.1 (build 1, aarch64-apple-darwin23.6.0)
   of 2025-02-23) from seed7-functions.sd7 --- Monday, July 14, 2025 @ 16:35:10 -----

  Buffer's major mode : seed7-mode

  User options:
  - defun-prompt-regexp                     : nil
  - parens-require-spaces                   : t
  - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
  - delete-pair-blink-delay                 : 1

  Variables:
  - forward-sexp-function                   : nil
  - beginning-of-defun-function             : seed7--beg-of-defun-conventional
  - end-of-defun-function                   : seed7--end-of-defun-conventional
  - end-of-defun-moves-to-eol               : nil
  - narrow-to-defun-include-comments        : nil


The lisp.el variables on Emacs 30.1.90
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    ----lisp.el control variables (Emacs GNU Emacs 30.1.90 (build 2, x86_64-pc-linux-gnu)
     of 2025-06-03) from elisp-functions.el --- Monday, July 14, 2025 @ 16:25:38 -----

    Buffer's major mode : emacs-lisp-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : 1

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : nil
    - end-of-defun-function                   : #[0 "\300\301!\207" [forward-sexp 1] 2]
    - end-of-defun-moves-to-eol               : t
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 30.1.90 (build 2, x86_64-pc-linux-gnu)
     of 2025-06-03) from python-functions.py --- Monday, July 14, 2025 @ 16:25:50 -----

    Buffer's major mode : python-ts-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : 1

    Variables:
    - forward-sexp-function                   : python-nav-forward-sexp
    - beginning-of-defun-function             : treesit-beginning-of-defun
    - end-of-defun-function                   : treesit-end-of-defun
    - end-of-defun-moves-to-eol               : t
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 30.1.90 (build 2, x86_64-pc-linux-gnu)
     of 2025-06-03) from python-functions.py --- Monday, July 14, 2025 @ 16:26:25 -----

    Buffer's major mode : python-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : 1

    Variables:
    - forward-sexp-function                   : python-nav-forward-sexp
    - beginning-of-defun-function             : python-nav-beginning-of-defun
    - end-of-defun-function                   : python-nav-end-of-defun
    - end-of-defun-moves-to-eol               : t
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 30.1.90 (build 2, x86_64-pc-linux-gnu)
     of 2025-06-03) from c-functions.c --- Monday, July 14, 2025 @ 16:26:45 -----

    Buffer's major mode : c-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : nil
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : 1

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : c-beginning-of-defun
    - end-of-defun-function                   : c-end-of-defun
    - end-of-defun-moves-to-eol               : t
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 30.1.90 (build 2, x86_64-pc-linux-gnu)
     of 2025-06-03) from seed7-functions.sd7 --- Monday, July 14, 2025 @ 16:27:13 -----

    Buffer's major mode : seed7-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : 1

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : seed7--beg-of-defun-conventional
    - end-of-defun-function                   : seed7--end-of-defun-conventional
    - end-of-defun-moves-to-eol               : nil
    - narrow-to-defun-include-comments        : nil


The lisp.el variables on Emacs 26.3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we can see, on Emacs 26.3 the variables ``delete-pair-blink-delay`` and ``end-of-defun-moves-to-eol``
did not exist.

::

    ----lisp.el control variables (Emacs GNU Emacs 26.3 (build 1, x86_64-apple-darwin18.6.0)
     of 2019-08-30) from emacs-customization.el --- Monday, July 14, 2025 @ 16:17:24 -----

    Buffer's major mode : emacs-lisp-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : (delete-pair-blink-delay "**is currently unbound!**")

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : nil
    - end-of-defun-function                   : #[0 "\300\301!\207" [forward-sexp 1] 2]
    - end-of-defun-moves-to-eol               : (end-of-defun-moves-to-eol "**is currently unbound!**")
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 26.3 (build 1, x86_64-apple-darwin18.6.0)
     of 2019-08-30) from test-python.py --- Monday, July 14, 2025 @ 16:18:00 -----

    Buffer's major mode : python-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : (delete-pair-blink-delay "**is currently unbound!**")

    Variables:
    - forward-sexp-function                   : python-nav-forward-sexp
    - beginning-of-defun-function             : python-nav-beginning-of-defun
    - end-of-defun-function                   : python-nav-end-of-defun
    - end-of-defun-moves-to-eol               : (end-of-defun-moves-to-eol "**is currently unbound!**")
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 26.3 (build 1, x86_64-apple-darwin18.6.0)
     of 2019-08-30) from test-c.c --- Monday, July 14, 2025 @ 16:18:38 -----

    Buffer's major mode : c-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : nil
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : (delete-pair-blink-delay "**is currently unbound!**")

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : c-beginning-of-defun
    - end-of-defun-function                   : c-end-of-defun
    - end-of-defun-moves-to-eol               : (end-of-defun-moves-to-eol "**is currently unbound!**")
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 26.3 (build 1, x86_64-apple-darwin18.6.0)
     of 2019-08-30) from test-cpp.cpp --- Monday, July 14, 2025 @ 16:19:23 -----

    Buffer's major mode : c++-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : nil
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : (delete-pair-blink-delay "**is currently unbound!**")

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : c-beginning-of-defun
    - end-of-defun-function                   : c-end-of-defun
    - end-of-defun-moves-to-eol               : (end-of-defun-moves-to-eol "**is currently unbound!**")
    - narrow-to-defun-include-comments        : nil

    ----lisp.el control variables (Emacs GNU Emacs 26.3 (build 1, x86_64-apple-darwin18.6.0)
     of 2019-08-30) from test-seed7.sd7 --- Monday, July 14, 2025 @ 16:20:01 -----

    Buffer's major mode : seed7-mode

    User options:

    - defun-prompt-regexp                     : nil
    - parens-require-spaces                   : t
    - insert-pair-alist                       : ((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))
    - delete-pair-blink-delay                 : (delete-pair-blink-delay "**is currently unbound!**")

    Variables:
    - forward-sexp-function                   : nil
    - beginning-of-defun-function             : seed7--beg-of-defun-conventional
    - end-of-defun-function                   : seed7--end-of-defun-conventional
    - end-of-defun-moves-to-eol               : nil
    - narrow-to-defun-include-comments        : nil


.. ---------------------------------------------------------------------------


The Emacs Lisp test code in file
`tests/emacs-analysis/elisp-functions.el`_
defines five elisp defun in 35 lines of code:

.. code:: elisp

          ;; Emacs Lisp Example Code

          (defun fct-b ()
            "First test function."
            (message "fct-b"))


          (defun fct-b ()
            "First test function."
            (message "fct-b"))


          ;; comment before fct-c
          (defun fct-c ()
            "First test function."
            (message "fct-c"))


          ;; comment before fct-d

          (defun fct-d ()
            "First test function."
            (message "fct-d"))


          ;; comment line 1 before (fct-e)
          ;; comment line 2 before fct-e
          ;; comment line 3 before fct-e


          (defun fct-e ()
            "First test function."
            (message "fct-e"))

          ;; -----last line (line 35) ---------------------------------------------------

Test Results
------------

For Emacs Lisp Code
~~~~~~~~~~~~~~~~~~~


The lisp.el settings for emacs-lisp-mode buffers is the following:

================================== ============= ============================================================================ =====================================
lisp.el variable                   Declared with Default value                                                                Value used in emacs-lisp-mode buffers
================================== ============= ============================================================================ =====================================
defun-prompt-regexp                defcustom     nil                                                                          nil
parens-require-spaces              defcustom     t                                                                            t
forward-sexp-function              defvar        nil, which mean it uses ``forward-sexp-default-function``                    nil : it uses ``forward-sexp-default-function``.
beginning-of-defun-function        defvar        nil                                                                          nil
end-of-defun-function              defvar        ``(lambda () (forward-sexp 1))``                                             ``#[0 "\300\301!\207" [forward-sexp 1] 2]``.
end-of-defun-moves-to-eol          defvar        t                                                                            t
narrow-to-defun-include-comments   defvar        nil                                                                          nil
insert-pair-alist                  defcustom     ``'((?\( ?\)) (?\[ ?\]) (?\{ ?\}) (?\< ?\>) (?\" ?\") (?\' ?\') (?\` ?\'))`` ``((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))``
delete-pair-blink-delay            defcustom     blink-matching-delay : 1                                                     1
================================== ============= ============================================================================ =====================================



The end position of evaluating ``(sexp-forward 1)`` and (end-of-defun) from
various locations.  Showing the start position (point, line, column) before
execution of a function, and the resulting position (point, line column) after
execution of ``(sexp-forward 1)`` and ``(end-of-function)``.


================ ============================== ======================================== =====================================
Start position   After ``(sexp-forward 1)``     After ``(end-of-defun)``                 After `(beginning-of-defun)``
================ ============================== ======================================== =====================================
``1,    1,  0``  ``105,  5, 20``, end of fct-a  ``106,  6,  0``, Line after end of fct-a ``1,    1,  0``, top of buffer
``43,   2,  0``  ``105,  5, 20``, end of fct-a  ``106,  6,  0``, Line after end of fct-a ``1,    1,  0``, top of buffer
``44,   3,  0``  ``105,  5, 20``, end of fct-a  ``106,  6,  0``, Line after end of fct-a ``1,    1,  0``, top of buffer
``60,   4,  0``  ``84,   4, 24``, end of string ``106,  6,  0``, Line after end of fct-a ``44,   3,  0``, beginning of fct-a
``85,   5,  0``  ``104,  5, 19``, end of call   ``106,  6,  0``, Line after end of fct-a ``44,   3,  0``, beginning of fct-a
``106,  6,  0``  ``169, 10, 20``, end of fct-b  ``170, 11,  0``, Line after end of fct-b ``44,   3,  0``, beginning of fct-a
``107,  7,  0``  ``169, 10, 20``, end of fct-b  ``170, 11,  0``, Line after end of fct-b ``44,   3,  0``, beginning of fct-a
``108,  8,  0``  ``169, 10, 20``, end of fct-b  ``170, 11,  0``, Line after end of fct-b ``44,   3,  0``, beginning of fct-a
``124,  9,  0``  ``148,  9, 24``, end of string ``170, 11,  0``, Line after end of fct-b ``108,  8,  0``, beginning of fct-b
``149, 10,  0``  ``168, 10, 19``, end of call   ``170, 11,  0``, Line after end of fct-b ``108,  8,  0``, beginning of fct-b
``170, 11,  0``  ``257, 16, 20``, end of fct-c  ``258, 17,  0``, Line after end of fct-c ``108,  8,  0``, beginning of fct-b
``171, 12,  0``  ``257, 16, 20``, end of fct-c  ``258, 17,  0``, Line after end of fct-c ``108,  8,  0``, beginning of fct-b
``172, 13,  0``  ``257, 16, 20``, end of fct-c  ``258, 17,  0``, Line after end of fct-c ``108,  8,  0``, beginning of fct-b
``196, 14,  0``  ``257, 16, 20``, end of fct-c  ``258, 17,  0``, Line after end of fct-c ``108,  8,  0``, beginning of fct-b
``212, 15,  0``  ``236, 15, 24``, end of string ``258, 17,  0``, Line after end of fct-c ``196, 14,  0``, beginning of fct-c
``237, 16,  0``  ``256, 16, 19``, end of call   ``258, 17,  0``, Line after end of fct-c ``196, 14,  0``, beginning of fct-c
``258, 17,  0``  ``346, 23, 20``, end of fct-d  ``347, 24,  0``, Line after end of fct-d ``196, 14,  0``, beginning of fct-c
``259, 18,  0``  ``346, 23, 20``, end of fct-d  ``347, 24,  0``, Line after end of fct-d ``196, 14,  0``, beginning of fct-c
``260, 19,  0``  ``346, 23, 20``, end of fct-d  ``347, 24,  0``, Line after end of fct-d ``196, 14,  0``, beginning of fct-c
``284, 20,  0``  ``346, 23, 20``, end of fct-d  ``347, 24,  0``, Line after end of fct-d ``196, 14,  0``, beginning of fct-c
``285, 21,  0``  ``346, 23, 20``, end of fct-d  ``347, 24,  0``, Line after end of fct-d ``196, 14,  0``, beginning of fct-c
``301, 22,  0``  ``325, 22, 24``, end of string ``347, 24,  0``, Line after end of fct-d ``285, 21,  0``, beginning of fct-d
``326, 23,  0``  ``345, 23, 19``, end of call   ``347, 24,  0``, Line after end of fct-d ``285, 21,  0``, beginning of fct-d
``347, 24,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``348, 25,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``349, 26,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``382, 27,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``413, 28,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``444, 29,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``445, 30,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``446, 31,  0``  ``507, 33, 20``, end of fct-e  ``508, 34,  0``, Line after end of fct-e ``285, 21,  0``, beginning of fct-d
``462, 32,  0``  ``486, 32, 24``, end of string ``508, 34,  0``, Line after end of fct-e ``446, 31,  0``, beginning of fct-e
``487, 33,  0``  ``506, 33, 19``, end of call   ``508, 34,  0``, Line after end of fct-e ``446, 31,  0``, beginning of fct-e
``508, 34,  0``  ``589, 36,  0``, end of buffer ``589, 36,  0``, end of buffer           ``446, 31,  0``, beginning of fct-e
``509, 35,  0``  ``589, 36,  0``, end of buffer ``589, 36,  0``, end of buffer           ``446, 31,  0``, beginning of fct-e
================ ============================== ======================================== =====================================


For Emacs Lisp code, the ``end-of-defun`` moves to the first column on the
line right after the end of a function as long as it is issued
before, inside a comment or an empty line, or inside the code of the function
when issued from the beginning of the line (as done in the tests above).
It also moves to the same spot if issued inside a a function sexp
inside the function.

This is not exactly the case for ``forward-sexp 1)`` which may move to the end
of another sexp inside a comment, a string or inside the function.

For Emacs Lisp code, the only "*external logic*" is the ``(forward-sexp 1)``
lambda provided to ``end-of-defun-function`` variable.  All other logic is
lisp.el code.



For Python Code
~~~~~~~~~~~~~~~

The lisp.el settings for python-mode buffers is the following:

================================== =============  =====================================
lisp.el variable                   Declared with  Value used in python-mode buffers
================================== =============  =====================================
defun-prompt-regexp                defcustom      nil
parens-require-spaces              defcustom      t
forward-sexp-function              defvar         python-nav-forward-sexp
beginning-of-defun-function        defvar         python-nav-beginning-of-defun
end-of-defun-function              defvar         python-nav-end-of-defun
end-of-defun-moves-to-eol          defvar         t
narrow-to-defun-include-comments   defvar         nil
insert-pair-alist                  defcustom      ``((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))``
delete-pair-blink-delay            defcustom      1
================================== =============  =====================================

The results of the tests over the Python code shown in the
`The Python test file: python-functions.py`_
section
are shown in the following table.

================ ===================================== ============================== ========================================= =============================================
Start position   After ``(python-nav-end-of-defun 1)`` After ``(end-of-defun)``       After ``(python-nav-beginning-of-defun)`` After ``(beginning-of-defun)``
================ ===================================== ============================== ========================================= =============================================
``1,    1,  0``  ``115,  6,  0``,  end of fct_a!!      ``115,  6,  0``, end of fct_a  ``1,    1,  0``, top of buffer            ``1,    1,  0``, top of buffer
``33,   2,  0``  ``115,  6,  0``,  end of fct_a!!      ``115,  6,  0``, end of fct_a  ``33,   2,  0``, **no move**              ``33,   2,  0``, **no move**
``34,   3,  0``  ``115,  6,  0``,  end of fct_a        ``115,  6,  0``, end of fct_a  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``63,   4,  0``  ``115,  6,  0``,  end of fct_a        ``115,  6,  0``, end of fct_a  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``90,   5,  0``  ``115,  6,  0``,  end of fct_a        ``115,  6,  0``, end of fct_a  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``115,  6,  0``  ``115,  6,  0``,  **no move**         ``201, 11,  0``, end of fct_b  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``116,  7,  0``  ``116,  7,  0``,  **no move**         ``201, 11,  0``, end of fct_b  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``117,  8,  0``  ``201, 11,  0``,  end of fct_b        ``201, 11,  0``, end of fct_b  ``34,   3,  0``, beginning of fct_a       ``34,   3,  0``, beginning of fct_a
``149,  9,  0``  ``201, 11,  0``,  end of fct_b        ``201, 11,  0``, end of fct_b  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``176, 10,  0``  ``201, 11,  0``,  end of fct_b        ``201, 11,  0``, end of fct_b  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``201, 11,  0``  ``201, 11,  0``,  **no move**         ``317, 17,  0``, end of fct_c  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``202, 12,  0``  ``202, 12,  0``,  **no move**         ``317, 17,  0``, end of fct_c  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``203, 13,  0``  ``203, 13,  0``,  **no move**         ``317, 17,  0``, end of fct_c  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``231, 14,  0``  ``317, 17,  0``,  end of fct_c        ``317, 17,  0``, end of fct_c  ``117,  8,  0``, beginning of fct_b       ``117,  8,  0``, beginning of fct_b
``265, 15,  0``  ``317, 17,  0``,  end of fct_c        ``317, 17,  0``, end of fct_c  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``292, 16,  0``  ``317, 17,  0``,  end of fct_c        ``317, 17,  0``, end of fct_c  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``317, 17,  0``  ``317, 17,  0``,  **no move**         ``430, 24,  0``, end of fct_d  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``318, 18,  0``  ``318, 18,  0``,  **no move**         ``430, 24,  0``, end of fct_d  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``319, 19,  0``  ``319, 19,  0``,  **no move**         ``430, 24,  0``, end of fct_d  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``347, 20,  0``  ``347, 20,  0``,  **no move**         ``430, 24,  0``, end of fct_d  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``348, 21,  0``  ``430, 24,  0``,  end of fct_d        ``430, 24,  0``, end of fct_d  ``231, 14,  0``, beginning of fct_c       ``231, 14,  0``, beginning of fct_c
``378, 22,  0``  ``430, 24,  0``,  end of fct_d        ``430, 24,  0``, end of fct_d  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``405, 23,  0``  ``430, 24,  0``,  end of fct_d        ``430, 24,  0``, end of fct_d  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``430, 24,  0``  ``430, 24,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``431, 25,  0``  ``431, 25,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``432, 26,  0``  ``432, 26,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``467, 27,  0``  ``467, 27,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``502, 28,  0``  ``502, 28,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``537, 29,  0``  ``537, 29,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``538, 30,  0``  ``538, 30,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``539, 31,  0``  ``620, 34,  0``,  end of fct_e        ``620, 34,  0``, end of fct_e  ``348, 21,  0``, beginning of fct_d       ``348, 21,  0``, beginning of fct_d
``568, 32,  0``  ``620, 34,  0``,  end of fct_e        ``620, 34,  0``, end of fct_e  ``539, 31,  0``, beginning of fct_e       ``539, 31,  0``, beginning of fct_e
``595, 33,  0``  ``620, 34,  0``,  end of fct_e        ``620, 34,  0``, end of fct_e  ``539, 31,  0``, beginning of fct_e       ``539, 31,  0``, beginning of fct_e
``620, 34,  0``  ``620, 34,  0``,  **no move**         ``620, 34,  0``, end of fct_e  ``539, 31,  0``, beginning of fct_e       ``539, 31,  0``, beginning of fct_e
``621, 35,  0``  ``621, 35,  0``,  end of buffer       ``621, 35,  0``, end of buffer ``539, 31,  0``, beginning of fct_e       ``539, 31,  0``, beginning of fct_e
================ ===================================== ============================== ========================================= =============================================


For Seed7 Code
~~~~~~~~~~~~~~

The lisp.el settings for seed7-mode buffers is the following:

================================== =============  =====================================
lisp.el variable                   Declared with  Value used in python-mode buffers
================================== =============  =====================================
defun-prompt-regexp                defcustom      nil
parens-require-spaces              defcustom      t
forward-sexp-function              defvar         nil
beginning-of-defun-function        defvar         seed7-nav-beg-of-defun
end-of-defun-function              defvar         seed7-nav-end-of-defun
end-of-defun-moves-to-eol          defvar         t
narrow-to-defun-include-comments   defvar         nil
insert-pair-alist                  defcustom      ``((40 41) (91 93) (123 125) (60 62) (34 34) (39 39) (96 39))``
delete-pair-blink-delay            defcustom      1
================================== =============  =====================================

The results of the tests over the Seed7 code shown in the
`The Seed7 test file: seed7-functions.sd7`_
section
are shown in the following table.

Results for unmodified lisp.el on Emacs 30.1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

================ ===================================== ================================== ========================================= =============================================
Start position   After ``(seed7-nav-end-of-defun 1)``  After ``(end-of-defun)``           After ``(seed7-nav-beginning-of-defun)``  After ``(beginning-of-defun)``
================ ===================================== ================================== ========================================= =============================================
``1,    1,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``37,   2,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``38,   3,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``64,   4,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``87,   5,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``88,   6,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``123,  7,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1
``151,  8,  0``  ``295, 15, 11``, end of fct 2         ``394, 20,  0``, **end of fct 3**
``152,  9,  0``  ``295, 15, 11``, end of fct 2         ``394, 20,  0``, **end of fct 3**
``153, 10,  0``  ``295, 15, 11``, end of fct 2         ``394, 20,  0``, **end of fct 3**
``194, 11,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2
``203, 12,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2
``240, 13,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2
``248, 14,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2
``284, 15,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2
``296, 16,  0``  ``393, 19, 23``, end of fct 3         ``509, 25,  0``, **end of fct 4**
``297, 17,  0``  ``393, 19, 23``, end of fct 3         ``509, 25,  0``, **end of fct 4**
``319, 18,  0``  ``393, 19, 23``, end of fct 3         ``509, 25,  0``, **end of fct 4**
``370, 19,  0``  ``393, 19, 23``, end of fct 3         ``394, 20,  0``, end of fct 3
``394, 20,  0``  ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **end of fct 5**
``395, 21,  0``  ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **end of fct 5**
``416, 22,  0``  ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **end of fct 5**
``417, 23,  0``  ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **end of fct 5**
``484, 24,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4
``509, 25,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``510, 26,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``511, 27,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``542, 28,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``566, 29,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``580, 30,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``581, 31,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``582, 32,  0``  ``689, 33, 31``, end of fct 5         ``770, 36,  0``, **end of buffer**
``658, 33,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5
``690, 34,  0``  ``770, 36,  0``, end of buffer        ``1,    1,  0``, **top of buffer**
``691, 35,  0``  ``770, 36,  0``, end of buffer        ``1,    1,  0``, **top of buffer**
================ ===================================== ================================== ========================================= =============================================



..
   Testing (seed7-nav-beginning-of-defun 1)
        1,  1,  0  ->   1,  1,  0
       37,  2,  0  ->   1,  1,  0
       38,  3,  0  ->   1,  1,  0
       64,  4,  0  ->   1,  1,  0
       87,  5,  0  ->   1,  1,  0
       88,  6,  0  ->   1,  1,  0
      123,  7,  0  ->  88,  6,  0
      151,  8,  0  ->  88,  6,  0
      152,  9,  0  ->  88,  6,  0
      153, 10,  0  ->  88,  6,  0
      194, 11,  0  -> 153, 10,  0
      203, 12,  0  -> 153, 10,  0
      240, 13,  0  -> 153, 10,  0
      248, 14,  0  -> 153, 10,  0
      284, 15,  0  -> 153, 10,  0
      296, 16,  0  -> 153, 10,  0
      297, 17,  0  -> 153, 10,  0
      319, 18,  0  -> 153, 10,  0
      370, 19,  0  -> 319, 18,  0
      394, 20,  0  -> 319, 18,  0
      395, 21,  0  -> 319, 18,  0
      416, 22,  0  -> 319, 18,  0
      417, 23,  0  -> 319, 18,  0
      484, 24,  0  -> 417, 23,  0
      509, 25,  0  -> 417, 23,  0
      510, 26,  0  -> 417, 23,  0
      511, 27,  0  -> 417, 23,  0
      542, 28,  0  -> 417, 23,  0
      566, 29,  0  -> 417, 23,  0
      580, 30,  0  -> 417, 23,  0
      581, 31,  0  -> 417, 23,  0
      582, 32,  0  -> 417, 23,  0
      658, 33,  0  -> 582, 32,  0
      690, 34,  0  -> 582, 32,  0
      691, 35,  0  -> 582, 32,  0

   Testing (beginning-of-defun)
        1,  1,  0  ->   1,  1,  0
       37,  2,  0  ->   1,  1,  0
       38,  3,  0  ->   1,  1,  0
       64,  4,  0  ->   1,  1,  0
       87,  5,  0  ->   1,  1,  0
       88,  6,  0  ->   1,  1,  0
      123,  7,  0  ->  88,  6,  0
      151,  8,  0  ->  88,  6,  0
      152,  9,  0  ->  88,  6,  0
      153, 10,  0  ->  88,  6,  0
      194, 11,  0  -> 153, 10,  0
      203, 12,  0  -> 153, 10,  0
      240, 13,  0  -> 153, 10,  0
      248, 14,  0  -> 153, 10,  0
      284, 15,  0  -> 153, 10,  0
      296, 16,  0  -> 153, 10,  0
      297, 17,  0  -> 153, 10,  0
      319, 18,  0  -> 153, 10,  0
      370, 19,  0  -> 319, 18,  0
      394, 20,  0  -> 319, 18,  0
      395, 21,  0  -> 319, 18,  0
      416, 22,  0  -> 319, 18,  0
      417, 23,  0  -> 319, 18,  0
      484, 24,  0  -> 417, 23,  0
      509, 25,  0  -> 417, 23,  0
      510, 26,  0  -> 417, 23,  0
      511, 27,  0  -> 417, 23,  0
      542, 28,  0  -> 417, 23,  0
      566, 29,  0  -> 417, 23,  0
      580, 30,  0  -> 417, 23,  0
      581, 31,  0  -> 417, 23,  0
      582, 32,  0  -> 417, 23,  0
      658, 33,  0  -> 582, 32,  0
      690, 34,  0  -> 582, 32,  0
      691, 35,  0  -> 582, 32,  0

.. ---------------------------------------------------------------------------
..
   Testing (python-nav-forward-sexp 1)
        1,  1,  0    ->  32,  1, 31
       33,  2,  0    -> 114,  5, 24
       34,  3,  0    -> 114,  5, 24
       63,  4,  0    ->  69,  4,  6
       90,  5,  0    ->  99,  5,  9
      115,  6,  0    -> 200, 10, 24
      116,  7,  0    -> 200, 10, 24
      117,  8,  0    -> 200, 10, 24
      149,  9,  0    -> 155,  9,  6
      176, 10,  0    -> 185, 10,  9
      201, 11,  0    -> 316, 16, 24
      202, 12,  0    -> 316, 16, 24
      203, 13,  0    -> 230, 13, 27
      231, 14,  0    -> 316, 16, 24
      265, 15,  0    -> 271, 15,  6
      292, 16,  0    -> 301, 16,  9
      317, 17,  0    -> 429, 23, 24
      318, 18,  0    -> 429, 23, 24
      319, 19,  0    -> 346, 19, 27
      347, 20,  0    -> 429, 23, 24
      348, 21,  0    -> 429, 23, 24
      378, 22,  0    -> 384, 22,  6
      405, 23,  0    -> 414, 23,  9
      430, 24,  0    -> 619, 33, 24
      431, 25,  0    -> 619, 33, 24
      432, 26,  0    -> 466, 26, 34
      467, 27,  0    -> 501, 27, 34
      502, 28,  0    -> 536, 28, 34
      537, 29,  0    -> 619, 33, 24
      538, 30,  0    -> 619, 33, 24
      539, 31,  0    -> 619, 33, 24
      568, 32,  0    -> 574, 32,  6
      595, 33,  0    -> 604, 33,  9
      620, 34,  0    -> 700, 36,  0
      621, 35,  0    -> 699, 35, 78




The code of ``python-nav-end-of-defun`` from Emacs python.el is:

.. code:: elisp

          (defun python-nav-end-of-defun ()
            "Move point to the end of def or class.
          Returns nil if point is not in a def or class."
            (interactive)
            (let ((beg-defun-indent)
                  (beg-pos (point)))
              (when (or (python-info-looking-at-beginning-of-defun)
                        (python-nav-beginning-of-defun 1)
                        (python-nav-beginning-of-defun -1))
                (setq beg-defun-indent (current-indentation))
                (while (progn
                         (python-nav-end-of-statement)
                         (python-util-forward-comment 1)
                         (and (> (current-indentation) beg-defun-indent)
                              (not (eobp)))))
                (python-util-forward-comment -1)
                (forward-line 1)
                ;; Ensure point moves forward.
                (and (> beg-pos (point)) (goto-char beg-pos))
                ;; Return non-nil if we did something (because then we were in a
                ;; def/class).
                (/= beg-pos (point)))))


As described in the docstring of ``python-nav-end-of-defun``
that function does not move point when it is on an empty line or a comment
line located just before the Python function.

  This behaviour **differs** from what ``(forward-sexp 1)`` issued from a
  point located above the defun as shown by the table for emacs lisp.

But look at the test results!  The function behaves **differently** at the
beginning of the buffer!

  If ``python-nav-end-of-defun`` is evaluated from the beginning of line 1 and
  2, which are above the beginning of Python ``fct_a``, point lands at the
  beginning of line 6, just after the end of ``fct_a``.  It behaves as if line 1
  and 2 are part of the first function.

Which behaviour is required by ``end-of-defun-function``?
Its docstring does not describe this.  Here's a copy of the lisp.el code:

.. code:: elisp

          (defvar end-of-defun-function
            (lambda () (forward-sexp 1))
            "Function for `end-of-defun' to call.
          This is used to find the end of the defun at point.
          It is called with no argument, right after calling `beginning-of-defun-raw'.
          So the function can assume that point is at the beginning of the defun body.
          It should move point to the first position after the defun.")



.. ---------------------------------------------------------------------------
..
      Log in a emacs lisp file

     ..
        1, 1, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=1
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=1
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
      :end-of-defun: after cond. Before skip: point=91
      :end-of-defun: after cond. After skip: point=91
     -> 91, 6, 0
     Repeating (test)
        28, 2, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=28
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=1
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
      :end-of-defun: after cond. Before skip: point=91
      :end-of-defun: after cond. After skip: point=91
     -> 91, 6, 0
     Repeating (test)
        29, 3, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=29
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=29
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
      :end-of-defun: after cond. Before skip: point=91
      :end-of-defun: after cond. After skip: point=91
     -> 91, 6, 0
     Repeating (test)
        45, 4, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=45
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=29
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
      :end-of-defun: after cond. Before skip: point=91
      :end-of-defun: after cond. After skip: point=91
     -> 91, 6, 0
     Repeating (test)
        70, 5, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=70
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=29
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
      :end-of-defun: after cond. Before skip: point=91
      :end-of-defun: after cond. After skip: point=91
     -> 91, 6, 0
     Repeating (test)
        91, 6, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=91
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=29
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=91
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=93
     :end-of-defun: #5: point=154
      :end-of-defun: after cond. Before skip: point=154
      :end-of-defun: after cond. After skip: point=155
     -> 155, 11, 0
     Repeating (test)
        92, 7, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=92
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=29
     :end-of-defun: #2 : point=90
     :end-of-defun: #3 : point=92
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=93
     :end-of-defun: #5: point=154
      :end-of-defun: after cond. Before skip: point=154
      :end-of-defun: after cond. After skip: point=155
     -> 155, 11, 0
     Repeating (test)
        93, 8, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=93
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=155
      :end-of-defun: after cond. Before skip: point=155
      :end-of-defun: after cond. After skip: point=155
     -> 155, 11, 0
     Repeating (test)
        109, 9, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=109
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=155
      :end-of-defun: after cond. Before skip: point=155
      :end-of-defun: after cond. After skip: point=155
     -> 155, 11, 0
     Repeating (test)
        134, 10, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=134
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=155
      :end-of-defun: after cond. Before skip: point=155
      :end-of-defun: after cond. After skip: point=155
     -> 155, 11, 0
     Repeating (test)
        155, 11, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=155
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=155
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=181
     :end-of-defun: #5: point=242
      :end-of-defun: after cond. Before skip: point=242
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        156, 12, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=156
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=156
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=181
     :end-of-defun: #5: point=242
      :end-of-defun: after cond. Before skip: point=242
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        157, 13, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=157
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=93
     :end-of-defun: #2 : point=154
     :end-of-defun: #3 : point=157
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=181
     :end-of-defun: #5: point=242
      :end-of-defun: after cond. Before skip: point=242
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        181, 14, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=181
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=243
      :end-of-defun: after cond. Before skip: point=243
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        197, 15, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=197
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=243
      :end-of-defun: after cond. Before skip: point=243
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        222, 16, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=222
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=243
      :end-of-defun: after cond. Before skip: point=243
      :end-of-defun: after cond. After skip: point=243
     -> 243, 17, 0
     Repeating (test)
        243, 17, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=243
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=243
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=270
     :end-of-defun: #5: point=331
      :end-of-defun: after cond. Before skip: point=331
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        244, 18, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=244
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=244
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=270
     :end-of-defun: #5: point=331
      :end-of-defun: after cond. Before skip: point=331
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        245, 19, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=245
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=245
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=270
     :end-of-defun: #5: point=331
      :end-of-defun: after cond. Before skip: point=331
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        269, 20, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=269
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=181
     :end-of-defun: #2 : point=242
     :end-of-defun: #3 : point=269
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=270
     :end-of-defun: #5: point=331
      :end-of-defun: after cond. Before skip: point=331
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        270, 21, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=270
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=332
      :end-of-defun: after cond. Before skip: point=332
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        286, 22, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=286
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=332
      :end-of-defun: after cond. Before skip: point=332
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        311, 23, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=311
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=332
      :end-of-defun: after cond. Before skip: point=332
      :end-of-defun: after cond. After skip: point=332
     -> 332, 24, 0
     Repeating (test)
        332, 24, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=332
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=332
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        333, 25, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=333
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=333
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        334, 26, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=334
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=334
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        367, 27, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=367
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=367
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        398, 28, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=398
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=398
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        429, 29, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=429
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=429
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        430, 30, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=430
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=270
     :end-of-defun: #2 : point=331
     :end-of-defun: #3 : point=430
     :end-of-defun: #4 (just after ’(when (setq success (beginning-of-defun-raw (- arg)))’ : point=431
     :end-of-defun: #5: point=492
      :end-of-defun: after cond. Before skip: point=492
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        431, 31, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=431
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=493
      :end-of-defun: after cond. Before skip: point=493
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        447, 32, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=447
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=493
      :end-of-defun: after cond. Before skip: point=493
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        472, 33, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=472
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=493
      :end-of-defun: after cond. Before skip: point=493
      :end-of-defun: after cond. After skip: point=493
     -> 493, 34, 0
     Repeating (test)
        493, 34, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=493
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=493
      :end-of-defun: after cond. Before skip: point=575
      :end-of-defun: after cond. After skip: point=575
     -> 575, 37, 0
     Repeating (test)
        494, 35, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=494
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=494
      :end-of-defun: after cond. Before skip: point=575
      :end-of-defun: after cond. After skip: point=575
     -> 575, 37, 0
     Repeating (test)
        574, 36, 0
     :end-of-defun: arg=nil, interactive=nil, called from point=574
      :end-of-defun: changed arg to: 1
     :end-of-defun: #1 : point=431
     :end-of-defun: #2 : point=492
     :end-of-defun: #3 : point=574
      :end-of-defun: after cond. Before skip: point=575
      :end-of-defun: after cond. After skip: point=575
     -> 575, 37, 0




.. ---------------------------------------------------------------------------
.. links:
.. elisp/seed7-mode/tests/emacs-analysis/mark-defun-requirements.rst

.. _PEL project:          https://github.com/pierre-rouleau/pel
.. _pel-emacs-analyze.el: https://github.com/pierre-rouleau/pel/blob/master/pel-emacs-analyze.el
.. _c-functions.c:                           ./c-functions.c
.. _elisp-functions.el:
.. _tests/emacs-analysis/elisp-functions.el: ./elisp-functions.el
.. _python-functions.py:                     ./python-functions.py
.. _seed7-functions.sd7:                     ./seed7-functions.sd7

.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:
