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


Using the file `elisp-functions.el`_ (shown in the section titled `The Elisp
test file: elisp-functions.el`_) the following table shows the result of
testing several navigation functions for Emacs Lisp code.

Each column show the point, the line number, the column number and a
description.  The first column shows the position before execution of the
function and the other columns show the result position after evaluation of a
function identified in the title.

======================== ============================== ================================= =====================================
Start position           After ``(sexp-forward 1)``     After ``(end-of-defun)``          After ``(beginning-of-defun)``
======================== ============================== ================================= =====================================
``1,    1,  0``,         ``105,  5, 20``, end of fct 1  ``106,  6,  0``, line-aeof fct 1  ``1,    1,  0``, top of buffer
``43,   2,  0``,         ``105,  5, 20``, end of fct 1  ``106,  6,  0``, line-aeof fct 1  ``1,    1,  0``, top of buffer
``44,   3,  0``, fct 1   ``105,  5, 20``, end of fct 1  ``106,  6,  0``, line-aeof fct 1  ``1,    1,  0``, top of buffer
``60,   4,  0``, fct 1   ``84,   4, 24``, end of string ``106,  6,  0``, line-aeof fct 1  ``44,   3,  0``, beginning of fct 1
``85,   5,  0``, fct 1   ``104,  5, 19``, end of call   ``106,  6,  0``, line-aeof fct 1  ``44,   3,  0``, beginning of fct 1
``106,  6,  0``,         ``169, 10, 20``, end of fct 2  ``170, 11,  0``, line-aeof fct 2  ``44,   3,  0``, beginning of fct 1
``107,  7,  0``,         ``169, 10, 20``, end of fct 2  ``170, 11,  0``, line-aeof fct 2  ``44,   3,  0``, beginning of fct 1
``108,  8,  0``, fct 2   ``169, 10, 20``, end of fct 2  ``170, 11,  0``, line-aeof fct 2  ``44,   3,  0``, beginning of fct 1
``124,  9,  0``, fct 2   ``148,  9, 24``, end of string ``170, 11,  0``, line-aeof fct 2  ``108,  8,  0``, beginning of fct 2
``149, 10,  0``, fct 2   ``168, 10, 19``, end of call   ``170, 11,  0``, line-aeof fct 2  ``108,  8,  0``, beginning of fct 2
``170, 11,  0``,         ``257, 16, 20``, end of fct 3  ``258, 17,  0``, line-aeof fct 3  ``108,  8,  0``, beginning of fct 2
``171, 12,  0``,         ``257, 16, 20``, end of fct 3  ``258, 17,  0``, line-aeof fct 3  ``108,  8,  0``, beginning of fct 2
``172, 13,  0``,         ``257, 16, 20``, end of fct 3  ``258, 17,  0``, line-aeof fct 3  ``108,  8,  0``, beginning of fct 2
``196, 14,  0``, fct 3   ``257, 16, 20``, end of fct 3  ``258, 17,  0``, line-aeof fct 3  ``108,  8,  0``, beginning of fct 2
``212, 15,  0``, fct 3   ``236, 15, 24``, end of string ``258, 17,  0``, line-aeof fct 3  ``196, 14,  0``, beginning of fct 3
``237, 16,  0``, fct 3   ``256, 16, 19``, end of call   ``258, 17,  0``, line-aeof fct 3  ``196, 14,  0``, beginning of fct 3
``258, 17,  0``,         ``346, 23, 20``, end of fct 4  ``347, 24,  0``, line-aeof fct 4  ``196, 14,  0``, beginning of fct 3
``259, 18,  0``,         ``346, 23, 20``, end of fct 4  ``347, 24,  0``, line-aeof fct 4  ``196, 14,  0``, beginning of fct 3
``260, 19,  0``,         ``346, 23, 20``, end of fct 4  ``347, 24,  0``, line-aeof fct 4  ``196, 14,  0``, beginning of fct 3
``284, 20,  0``,         ``346, 23, 20``, end of fct 4  ``347, 24,  0``, line-aeof fct 4  ``196, 14,  0``, beginning of fct 3
``285, 21,  0``, fct 4   ``346, 23, 20``, end of fct 4  ``347, 24,  0``, line-aeof fct 4  ``196, 14,  0``, beginning of fct 3
``301, 22,  0``, fct 4   ``325, 22, 24``, end of string ``347, 24,  0``, line-aeof fct 4  ``285, 21,  0``, beginning of fct 4
``326, 23,  0``, fct 4   ``345, 23, 19``, end of call   ``347, 24,  0``, line-aeof fct 4  ``285, 21,  0``, beginning of fct 4
``347, 24,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``348, 25,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``349, 26,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``382, 27,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``413, 28,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``444, 29,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``445, 30,  0``,         ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``446, 31,  0``, fct 5   ``507, 33, 20``, end of fct 5  ``508, 34,  0``, line-aeof fct 5  ``285, 21,  0``, beginning of fct 4
``462, 32,  0``, fct 5   ``486, 32, 24``, end of string ``508, 34,  0``, line-aeof fct 5  ``446, 31,  0``, beginning of fct 5
``487, 33,  0``, fct 5   ``506, 33, 19``, end of call   ``508, 34,  0``, line-aeof fct 5  ``446, 31,  0``, beginning of fct 5
``508, 34,  0``,         ``589, 36,  0``, end of buffer ``589, 36,  0``, end of buffer    ``446, 31,  0``, beginning of fct 5
``509, 35,  0``,         ``589, 36,  0``, end of buffer ``589, 36,  0``, end of buffer    ``446, 31,  0``, beginning of fct 5
======================== ============================== ================================= =====================================

In the table,

- ``line-aeof`` means "line after end of function",
- the names of the functions were replaced by a sequence number to better represent their position.

  - fct 1 is: fct-a
  - fct 2 is: fct-b
  - fct 3 is: fct-c
  - fct 4 is: fct-d
  - fct 5 is: fct-e

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

========================= ===================================== ================================ ========================================= =============================================
Start position            After ``(python-nav-end-of-defun 1)`` After ``(end-of-defun)``         After ``(python-nav-beginning-of-defun)`` After ``(beginning-of-defun)``
========================= ===================================== ================================ ========================================= =============================================
``1,    1,  0``,          ``115,  6,  0``,  end of fct 1!!      ``115,  6,  0``, line-aeo fct 1  ``1,    1,  0``, top of buffer            ``1,    1,  0``, top of buffer
``33,   2,  0``,          ``115,  6,  0``,  end of fct 1!!      ``115,  6,  0``, line-aeo fct 1  ``33,   2,  0``, **no move**              ``33,   2,  0``, **no move**
``34,   3,  0``, fct 1    ``115,  6,  0``,  end of fct 1        ``115,  6,  0``, line-aeo fct 1  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``63,   4,  0``, fct 1    ``115,  6,  0``,  end of fct 1        ``115,  6,  0``, line-aeo fct 1  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``90,   5,  0``, fct 1    ``115,  6,  0``,  end of fct 1        ``115,  6,  0``, line-aeo fct 1  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``115,  6,  0``,          ``115,  6,  0``,  **no move**         ``201, 11,  0``, line-aeo fct 2  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``116,  7,  0``,          ``116,  7,  0``,  **no move**         ``201, 11,  0``, line-aeo fct 2  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``117,  8,  0``, fct 2    ``201, 11,  0``,  end of fct 2        ``201, 11,  0``, line-aeo fct 2  ``34,   3,  0``, beginning of fct 1       ``34,   3,  0``, beginning of fct 1
``149,  9,  0``, fct 2    ``201, 11,  0``,  end of fct 2        ``201, 11,  0``, line-aeo fct 2  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``176, 10,  0``, fct 2    ``201, 11,  0``,  end of fct 2        ``201, 11,  0``, line-aeo fct 2  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``201, 11,  0``,          ``201, 11,  0``,  **no move**         ``317, 17,  0``, line-aeo fct 3  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``202, 12,  0``,          ``202, 12,  0``,  **no move**         ``317, 17,  0``, line-aeo fct 3  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``203, 13,  0``,          ``203, 13,  0``,  **no move**         ``317, 17,  0``, line-aeo fct 3  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``231, 14,  0``, fct 3    ``317, 17,  0``,  end of fct 3        ``317, 17,  0``, line-aeo fct 3  ``117,  8,  0``, beginning of fct 2       ``117,  8,  0``, beginning of fct 2
``265, 15,  0``, fct 3    ``317, 17,  0``,  end of fct 3        ``317, 17,  0``, line-aeo fct 3  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``292, 16,  0``, fct 3    ``317, 17,  0``,  end of fct 3        ``317, 17,  0``, line-aeo fct 3  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``317, 17,  0``,          ``317, 17,  0``,  **no move**         ``430, 24,  0``, line-aeo fct 4  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``318, 18,  0``,          ``318, 18,  0``,  **no move**         ``430, 24,  0``, line-aeo fct 4  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``319, 19,  0``,          ``319, 19,  0``,  **no move**         ``430, 24,  0``, line-aeo fct 4  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``347, 20,  0``,          ``347, 20,  0``,  **no move**         ``430, 24,  0``, line-aeo fct 4  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``348, 21,  0``, fct 4    ``430, 24,  0``,  end of fct 4        ``430, 24,  0``, line-aeo fct 4  ``231, 14,  0``, beginning of fct 3       ``231, 14,  0``, beginning of fct 3
``378, 22,  0``, fct 4    ``430, 24,  0``,  end of fct 4        ``430, 24,  0``, line-aeo fct 4  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``405, 23,  0``, fct 4    ``430, 24,  0``,  end of fct 4        ``430, 24,  0``, line-aeo fct 4  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``430, 24,  0``,          ``430, 24,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``431, 25,  0``,          ``431, 25,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``432, 26,  0``,          ``432, 26,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``467, 27,  0``,          ``467, 27,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``502, 28,  0``,          ``502, 28,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``537, 29,  0``,          ``537, 29,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``538, 30,  0``,          ``538, 30,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``539, 31,  0``, fct 5    ``620, 34,  0``,  end of fct 5        ``620, 34,  0``, line-aeo fct 5  ``348, 21,  0``, beginning of fct 4       ``348, 21,  0``, beginning of fct 4
``568, 32,  0``, fct 5    ``620, 34,  0``,  end of fct 5        ``620, 34,  0``, line-aeo fct 5  ``539, 31,  0``, beginning of fct 5       ``539, 31,  0``, beginning of fct 5
``595, 33,  0``, fct 5    ``620, 34,  0``,  end of fct 5        ``620, 34,  0``, line-aeo fct 5  ``539, 31,  0``, beginning of fct 5       ``539, 31,  0``, beginning of fct 5
``620, 34,  0``,          ``620, 34,  0``,  **no move**         ``620, 34,  0``, line-aeo fct 5  ``539, 31,  0``, beginning of fct 5       ``539, 31,  0``, beginning of fct 5
``621, 35,  0``,          ``621, 35,  0``,  end of buffer       ``621, 35,  0``, end of buffer   ``539, 31,  0``, beginning of fct 5       ``539, 31,  0``, beginning of fct 5
========================= ===================================== ================================ ========================================= =============================================

In the table,

- ``line-aeof`` means "line after end of function",
- the names of the functions were replaced by a sequence number to better represent their position.

  - fct 1 is: greet_a
  - fct 2 is: greet_b
  - fct 3 is: greet_c
  - fct 4 is: greet_d
  - fct 5 is: greet_e

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


Results for unmodified lisp.el on Emacs 30.1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The results of the tests over the Seed7 code shown in the
`The Seed7 test file: seed7-functions.sd7`_
section
are shown in the following table.
These are executed with the lisp.el version of Emacs 30.1


====================== ===================================== ===================================== ========================================= =============================================
Start position         After ``(seed7-nav-end-of-defun 1)``  After ``(end-of-defun)``              After ``(seed7-nav-beginning-of-defun)``  After ``(beginning-of-defun)``
====================== ===================================== ===================================== ========================================= =============================================
``1,    1,  0``,       ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``1,    1,  0``, not moved                ``1,    1,  0``, not moved
``37,   2,  0``,       ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``37,   2,  0``, not moved                ``37,   2,  0``, not moved
``38,   3,  0``,       ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``38,   3,  0``, not moved                ``38,   3,  0``, not moved
``64,   4,  0``,       ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``64,   4,  0``, not moved                ``64,   4,  0``, not moved
``87,   5,  0``,       ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``87,   5,  0``, not moved                ``87,   5,  0``, not moved
``88,   6,  0``, fct 1 ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``123,  7,  0``, fct 1 ``150,  7, 27``, end of fct 1         ``151,  8,  0``, line-aeof fct 1      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``151,  8,  0``,       ``295, 15, 11``, end of fct 2         ``394, 20,  0``, **line-aeof fct 3**  ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``152,  9,  0``,       ``295, 15, 11``, end of fct 2         ``394, 20,  0``, **line-aeof fct 3**  ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``153, 10,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``194, 11,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``203, 12,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``240, 13,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``248, 14,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``284, 15,  0``, fct 2 ``295, 15, 11``, end of fct 2         ``296, 16,  0``, line-aeof fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``296, 16,  0``,       ``393, 19, 23``, end of fct 3         ``509, 25,  0``, **line-aeof fct 4**  ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``297, 17,  0``,       ``393, 19, 23``, end of fct 3         ``509, 25,  0``, **line-aeof fct 4**  ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``319, 18,  0``, fct 3 ``393, 19, 23``, end of fct 3         ``394, 20,  0``, line-aeof fct 3      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``370, 19,  0``, fct 3 ``393, 19, 23``, end of fct 3         ``394, 20,  0``, line-aeof fct 3      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``394, 20,  0``,       ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **line-aeof fct 5**  ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``395, 21,  0``,       ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **line-aeof fct 5**  ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``416, 22,  0``,       ``508, 24, 24``, end of fct 4         ``690, 34,  0``, **line-aeof fct 5**  ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``417, 23,  0``, fct 4 ``508, 24, 24``, end of fct 4         ``509, 25,  0``, line-aeof fct 4      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``484, 24,  0``, fct 4 ``508, 24, 24``, end of fct 4         ``509, 25,  0``, line-aeof fct 4      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``509, 25,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``510, 26,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``511, 27,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``542, 28,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``566, 29,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``580, 30,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``581, 31,  0``,       ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``582, 32,  0``, fct 5 ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
``658, 33,  0``, fct 5 ``689, 33, 31``, end of fct 5         ``690, 34,  0``, line-aeof fct 5      ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
``690, 34,  0``,       ``690, 34,  0``, not moved            ``690, 34,  0``, not moved            ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
``691, 35,  0``,       ``691, 35,  0``, not moved            ``691, 35,  0``, not moved            ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
====================== ===================================== ===================================== ========================================= =============================================

In the table,

- ``line-aeof`` means "line after end of function",
- the names of the functions were replaced by a sequence number to better represent their position.

  - fct 1 is: f1_flipcoin
  - fct 2 is: f2_flipCoin
  - fct 3 is: f3_inverse
  - fct 4 is: f4_power
  - fct 5 is: f4_log10_of_power

The results show that the ``end-of-defun`` function, when issued between fct 1
and 2, 2 and 3, and 3 and 4, move point to the end of an extra function:
when issued between function 1 and 2, instead of moving point the the next
line after function 2 it moves point to the line after function 3.

The table also shows that ``(seed7-nav-end-of-defun 1)`` always move point to
the exact end of the current function (when point is inside a function) or the
function that follows (when point is inside a non-code line just before that
function).

This behaviour is similar to what ``(sexp-forward 1)`` does in Emacs Lisp
code, with the exception that ``(seed7-nav-end-of-defun 1)`` does not stop
at the end of an internal block made of parens or of Seed7 code statement.
The seed7-mode has another command for that.

The Python ``(python-nav-end-of-defun 1)`` behaves differently: it does not
move point when it is between 2 functions.

The Seed7 programming language differs from Python, lisp and C-like languages.
It has functions and procedures.  There are several ways of declaring
functions, a long and a short format.  Inside library files (the ``.s7i``
files), a function can hold nested function and procedure declarations and
definitions.  See the `array.s7i`_ file as an example of this.


.. ---------------------------------------------------------------------------

Results for modified lisp.el on Emacs 30.1
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The results of the tests over the Seed7 code shown in the
`The Seed7 test file: seed7-functions.sd7`_
section
are shown in the following table.
These are executed with the lisp.el version of Emacs 30.1 using the
following modification to lisp.el:

.. code:: diff

          diff --git a/lisp/emacs-lisp/lisp.el b/lisp/emacs-lisp/lisp.el
          index b6ebe75dbad..320b3c23e2f 100644
          --- a/lisp/emacs-lisp/lisp.el
          +++ b/lisp/emacs-lisp/lisp.el
          @@ -538,6 +538,9 @@ buffer-end
                      (side-effect-free error-free))
             (if (> arg 0) (point-max) (point-min)))

          +(defvar end-of-defun-skips-one nil
          +  "Set this to t when end-of-defun skips one function.")
          +
           (defun end-of-defun (&optional arg interactive)
             "Move forward to next end of defun.
           With argument, do it that many times.
          @@ -592,7 +595,8 @@ end-of-defun
                   ;; in between two defun's), or is at the end of a defun
                   ;; (because we started in the middle of a defun).
                   (unless (zerop arg)
          -          (when (setq success (beginning-of-defun-raw (- arg)))
          +          (when (and (setq success (beginning-of-defun-raw (- arg)))
          +                     (not end-of-defun-skips-one))
                       (funcall end-of-defun-function))))
                  ((< arg 0)
                   ;; Moving backward.




================ ===================================== ================================== ========================================= =============================================
Start position   After ``(seed7-nav-end-of-defun 1)``  After ``(end-of-defun)``           After ``(seed7-nav-beginning-of-defun)``  After ``(beginning-of-defun)``
================ ===================================== ================================== ========================================= =============================================
``1,    1,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``1,    1,  0``, no move                  ``1,    1,  0``, no move
``37,   2,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``37,   2,  0``, no move                  ``37,   2,  0``, no move
``38,   3,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``38,   3,  0``, no move                  ``38,   3,  0``, no move
``64,   4,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``64,   4,  0``, no move                  ``64,   4,  0``, no move
``87,   5,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``87,   5,  0``, no move                  ``87,   5,  0``, no move
``88,   6,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``123,  7,  0``  ``150,  7, 27``, end of fct 1         ``151,  8,  0``, end of fct 1      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``151,  8,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``152,  9,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``153, 10,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``88,   6,  0``, beginning of fct 1       ``88,   6,  0``, beginning of fct 1
``194, 11,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``203, 12,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``240, 13,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``248, 14,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``284, 15,  0``  ``295, 15, 11``, end of fct 2         ``296, 16,  0``, end of fct 2      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``296, 16,  0``  ``393, 19, 23``, end of fct 3         ``394, 20,  0``, end of fct 3      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``297, 17,  0``  ``393, 19, 23``, end of fct 3         ``394, 20,  0``, end of fct 3      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``319, 18,  0``  ``393, 19, 23``, end of fct 3         ``394, 20,  0``, end of fct 3      ``153, 10,  0``, beginning of fct 2       ``153, 10,  0``, beginning of fct 2
``370, 19,  0``  ``393, 19, 23``, end of fct 3         ``394, 20,  0``, end of fct 3      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``394, 20,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``395, 21,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``416, 22,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``417, 23,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4      ``319, 18,  0``, beginning of fct 3       ``319, 18,  0``, beginning of fct 3
``484, 24,  0``  ``508, 24, 24``, end of fct 4         ``509, 25,  0``, end of fct 4      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``509, 25,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``510, 26,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``511, 27,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``542, 28,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``566, 29,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``580, 30,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``581, 31,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``582, 32,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``417, 23,  0``, beginning of fct 4       ``417, 23,  0``, beginning of fct 4
``658, 33,  0``  ``689, 33, 31``, end of fct 5         ``690, 34,  0``, end of fct 5      ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
``690, 34,  0``  ``770, 36,  0``, end of buffer        ``690, 34,  0``, end of buffer     ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
``691, 35,  0``  ``770, 36,  0``, end of buffer        ``691  35,  0``, end of buffer     ``582, 32,  0``, beginning of fct 5       ``582, 32,  0``, beginning of fct 5
================ ===================================== ================================== ========================================= =============================================

In the table,

- ``line-aeof`` means "line after end of function",
- the names of the functions were replaced by a sequence number to better represent their position.

  - fct 1 is: f1_flipcoin
  - fct 2 is: f2_flipCoin
  - fct 3 is: f3_inverse
  - fct 4 is: f4_power
  - fct 5 is: f4_log10_of_power

As we can see in the table above the modification to lisp.el solves the
problem for Seed7 code.  The modifications do not affect other modes because
the Seed7 code is the only mode function that evaluates:

.. code:: elisp

  (setq-local end-of-defun-skips-one t)

The modification prevents ``end-of-defun`` from skipping one Seed7 function
and ``mark-defun`` from marking 2 Seed7 functions.

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
.. _array.s7i:                               ../seed7-code/array.s7i

.. ---------------------------------------------------------------------------

..
       Local Variables:
       time-stamp-line-limit: 10
       time-stamp-start: "^:Modified:[ \t]+\\\\?"
       time-stamp-end:   "\\.$"
       End:
