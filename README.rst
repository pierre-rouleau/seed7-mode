=============================================================
Seed7-mode - Emacs support for the Seed7 Programming Language
=============================================================


This is a **work-in-progress** version of seed7-mode 🚧 .
A lot of features are implemented but there's still some
areas that need improvements or fixes.
Please create a bug report for  any problem you detect.

Currently Implemented Features
==============================


Display seed-mode version information
-------------------------------------

The ``seed7-mode-version`` command, bound to ``C-c v`` in seed7-mode buffers,
display the version UTC time stamp of the file in the message area, at bottom
of the screen.  Use this to verify the version of the seed7-mode.el code you
are using.

Please include this information in bug reports.

Customize seed7-mode
--------------------

Several features of the ``seed7-mode`` are customizable.
Use the ``seed7-mode-customize`` command, bound to ``C-c C`` to quickly open the
``seed7-mode`` customization buffer.

The command is also accessible from the top menu as can be see
`here <screenshots/menu-customize.png>`_. Once issued it opens the
`seed7-mode customization buffer <screenshots/seed7-customize.png>`_.

Use the customization buffer to view or change one or several customizable
user-option variables.

Since these control the behaviour of the ``seed7-mode`` the changes will only
apply to buffers that are then opened in ``seed7-mode``.  If you already have
a buffer using the mode, change to ``fundamental-mode`` and then back to
``seed7-mode`` for the changes to take effect..


Syntax highlighting with user-customizable faces.
-------------------------------------------------

Font lock syntax highlighting for Seed7 supports different faces for different
Seed7 syntactic elements, including various faces for different types of
numbers and warning font to identify some syntactic errors in the code, but
not all.

See the following example screenshots:

=========================== ================================================
Screenshot                  Description
=========================== ================================================
`Terminal-mode Emacs`_      Shows the default highlighting of Seed7 code on
                            Emacs running in a macOS Terminal.

`Graphical Emacs`_          Shows the default highlighting of Seed7 code on
                            a basic GUI Emacs running in a macOS with the
                            default scheme.
=========================== ================================================

Emacs outline minor mode support
--------------------------------

The seed7-mode supports the `Emacs outline minor mode`_: collapse and
expansion of blocks, where heading is the first line of a declaration
block. This allows navigation and code manipulation by headings (blocks).

===================================== ================================================
Screenshot                            Description
===================================== ================================================
`bas.sd7 outline all collapsed`_      The ``prg/bas.sd7`` file shown with ``outline-minor-mode``
                                      active after executing ``outline-hide-sublevels``
`bas.sd7 outline with 2 expanded`_    The same ``prg/bas.sd7`` file shown with ``outline-minor-mode``
                                      active after expanding 2 outlines with ``outline-show-entry``
                                      at lines 154 and 369.
===================================== ================================================


Emacs top-level menu support
----------------------------

Provides a Seed7 top level menu entry which provides access to the commands.
Here's some examples:

===================================== ================================================
Screenshot                            Description
===================================== ================================================
`Terminal menu 1`_                    Using the top menu to access Seed7 code browsing
                                      inside a terminal Emacs.

`Terminal menu 2`_                    The selecting the Seed7 item as a second step
                                      in the menu using a terminal Emacs.

`GUI Emacs menu`_                     Using macOS GUI Emacs with iMenu to list the
                                      functions is the Seed7 file.  Unlike Windows
                                      and most Linux desktops,
                                      the Emacs menu shows up inside the macOS
                                      top screen menu by default but it's also
                                      possible to make it show inside the
                                      Emacs frame as the next screen captures
                                      show.

`Navigation to structure menu`_       The menu can be opened inside the frame
                                      explicitly with the ``menu-bar-open``
                                      command.  Here it is showing the
                                      navigation listing the structures
                                      declared inside ``prg/bas.sd7``

`Comments menu`_                      The comments-specific commands.

`Template insertion menu`_            The template insertion commands.
                                      These can be expanded easily with
                                      ``<TAB>``
                                      as described in the
                                      `Code Template Insertion`_
                                      section.
===================================== ================================================



Support for imenu mode and Speedbar
-----------------------------------

- Support `Emacs imenu mode`_ and `Emacs Speedbar`_ for:

  - Seed7 procedures,
  - Seed7 functions,
  - Seed7 interfaces,
  - Seed7 structures,
  - Seed7 enums.

See the following example screenshots:

=========================== ================================================
Screenshot                  Description
=========================== ================================================
`Terminal Emacs Speedbar`_  Using Emacs Speedbar to navigate the Seed7/pgm
                            directory, listing function, procedures,
                            structures, etc...

`GUI Emacs Speedbar`_       Using macOS GUI Emacs with Speedbar in a separate
                            GUI frame showing beside the local instance of
                            the `PEL Speedbar PDF`_.

`GUI Emacs menu`_           Using macOS GUI Emacs with iMenu to list the
                            functions is the Seed7 file.  Unlike Windows
                            and most Linux desktops,
                            the Emacs menu shows up inside the macOS
                            top screen menu.

`Terminal menu 1`_          Using the top menu to access Seed7 code browsing
                            inside a terminal Emacs.

`Terminal menu 2`_          The selecting the Seed7 item as a second step
                            in the menu using a terminal Emacs.

`Terminal Ivy prompt`_      Instead of using the menu, using a prompt
                            with completion driven by ivy to search and select
                            Seed7 element.
                            Shown inside a terminal Emacs.
=========================== ================================================


Indentation Control / Automatic Indentation
-------------------------------------------

Code indentation is done automatically when the ``<tab>`` key is pressed
from anywhere on the line, except when used to expand a callable parameter in most cases.
Automatic code indentation is also done when the ``<return>`` key is pressed.

The ``auto-fill`` mode is supported.

The following customizable user-option variables control Seed7 code indentation.

= ======================= =================================================
. Variable                Purpose
= ======================= =================================================
. **seed7-auto-indent**   Control whether auto-indentation of Seed7 code is active.

                          - It is on by default.
                          - To disable auto-indentation, set ``seed7-auto-indent`` to nil,
                            then disable (by activating ``fundamental-mode``)
                            and re-enable ``seed7-mode``.

. **seed7-indent-width**  Number of columns used for each indentation level of Seed7 code.
                          Defaults to 2.

. **indent-tabs-mode**    Control whether Emacs inserts ASCII hard TAB characters
                          when indenting.  If set to nil Emacs will only use
                          ASCII SPACE characters.  If active (set to T or any non-nil
                          value) Emacs will insert ASCII hard TAB character to fill
                          indentation according to the number of columns of
                          indentation required and the number of columns
                          used to render a hard tab (as specified by **tab-width**).

                          For Seed7 code, the convention is to avoid hard TAB
                          characters in the code, therefore the **indent-tab-mode**
                          should be turned off to prevent seed7-mode auto-indentation
                          to insert hard TAB characters.

                          Use the **untabify** command to replace all hard TAB characters
                          in the buffer by the equivalent number of SPACE characters.

. **tab-width**           Controls the the column width of a hard TAB on display.
= ======================= =================================================


Notes:

- One aspect of the auto indentation logic is that it checks (and extracts)
  the name of procedure and functions and the return type of the function.
  The auto-indentation will only work properly once these are identified.
  This acts as a reminder to fill in the missing parts.
- 🚧 As this code is still under early development please report any problem you
  may encounter.


Code Template Insertion
-----------------------

Seed7 code templates are inserted at point when the ``<tab>`` key is pressed
after one of the supported code identifier keywords under specific
constraints:

- When point is following one of the keywords from the first group and that is
  the only word on the current line, or
- when point is following one of the keywords of the second group and is
  located just before a ``)`` character (with or without a space between point
  and the closing parenthesis.   The second group holds keywords for argument
  declarations.

After successfully expanding the Seed7 code template, point is located at the
first location that must be filled.  This location, and the following
locations that must be filled, are internally identified
by `Emacs tempo markers`_. You can use the ``tempo-forward-mark`` and
``tempo-backward-mark`` to move point to these markers.  The ``seed7-mode``
keyboard map binds the ``<backtab>`` key to  the ``tempo-forward-mark``
command.  So after expanding a code template, fill the first field and then
press ``<backtab>`` to move point to the next field.

As mentioned above there are two groups of keywords, listed in the following
tables.

**First Group -- Statements:**

Expand the following keywords when point is located just after any of these
keywords, with the keyword being the only word on the current line.

============ =========================================================
Keyword      Expansion
============ =========================================================
**inc**      include statement
**const**    `constant declaration`_
**var**      `variable declaration`_
.
**proc**     `procedure declaration`_
**func**     `function declaration`_
**funcs**    `short function declaration`_
.
**enum**     `enum type declaration`_
**struct**   `struct type declaration`_
.
**case**     `case statement`_
**if**       `if statement`_
**ife**      `if statement`_ with an else clause
**ifei**     `if statement`_ with an elsif clause
**ifeie**    `if statement`_ with an elsif and an else clause
**repeat**   `repeat - until statement`_
**while**    `while statement`_
**for**      `for statement`_
**foru**     `for-until statement`_
**fors**     `for-step statement`_
**fore**     `for-each statement`_
**foreu**    `for-each statement`_ combined with an until condition
**forek**    `for-each-key statement`_
**foreku**   `for-each-key statement`_ combined with an until condition
**fork**     `for-key statement`_
**forku**    `for-key statement`_ combined with an until condition
.
**bl**       `Exception handler block`_
**gl**       ``global`` - ``end global`` block.
============ =========================================================


**Second Group -- Argument Declarations**

Expand the following keywords with ``<tab>`` when point is located just after
any of these keywords and before the closing parenthesis of a parameter list.

============ =========================================================
Keyword      Expansion
============ =========================================================
**in**       Declaration of an `in-parameter`_.
**inout**    Declaration of an `inout-parameter`_.
**invar**    Declaration of an `in-var-parameter`_.
**callbn**   Declaration of a `call-by-name parameter`_.
**ref**      Declaration of a `reference-parameter`_.
**val**      Declaration of a `value-parameter`_.
============ =========================================================

Seed7 Specific Abbreviations
----------------------------

:Ref: `Abbrevs @ Emacs Manual`_
:Ref: `Abbrev Concepts`_
:Ref: `Examining and Editing Abbrevs`_

By default, the **seed7-support-abbrev-mode** user-option is on (non-nil).
This makes **seed7-mode** support Seed7-specific abbreviations that can be automatically
expanded when the **abbrev-mode** is active.

You can expand a set
of Seed7 keywords by typing their (*system*) abbreviation followed by a word-separating
character such as ``<space>``, ``<RET>` or ``;`` and others.
All Seed7 abbreviations are short mnemonic character sequences that start with ``;``.

Dynamically enable or disable the ``abbrev-mode`` with the ``M-x abbrev-mode``
command.  List the abbreviations with ``M-x list-abbrevs``.  You can create
other abbreviations and edit them with ``M-x edit-abbrevs``.  You cannot
change the pre-defined Seed7 system abbreviations via the abbrev commands.
However you can change all Seed7 system abbreviations since the list is
customizable and defined by the **seed7-abbreviations** customizable user-option.
Of course you can also create your own abbreviations via the abbrev mode
commands to complement what is provided by seed7-mode.

While abbrev-mode is active, you can explicitly prevent expansion of the
keyword type ``C-q`` after the keyword before any white-space or punctuation
character.

If you do not want to use Seed7-specific abbreviations, you can change the
customization of this user-variable with ``M-x customize-option RET
seed7-support-abbrev-mode`` turning it off, changing the mode to another major
mode (such as fundamental-mode) and re-enabling the seed7-mode.  If you *Apply
and Save* the customization, the setting will persist across Emacs sessions.


The list of default supported abbreviations is controlled by the
**seed7-abbreviations** customizable user-option.  You can modify the
keyword or the expansion of any entry and add or delete entries
these via customization.   The new values are activated the next time a
buffer starts the seed7-mode.

The default abbreviations are shown inside the 8 following
tables:

- `pragmas`_
- `in-statement keywords`_
- `in-middle statement keywords`_
- `block clause keywords`_
- `predefined types`_
- `predefined constants`_
- `predefined variables`_
- `errinfo values`_



pragmas
~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;de``             decls
``;in``             info
``;li``             library
``;msg``            message
``;na``             names
``;syn``            syntax
``;sys``            system
``;tr``             trace
=================== ======================

in-statement keywords
~~~~~~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;fo``             forward
``;n``              new
``;no``             noop
``;ra``             raise
``;rt``             return
=================== ======================


in-middle statement keywords
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;dt``             downto
``;exc``            exception
``;lo``             local
``;pa``             param
``;rg``             range
``;rs``             result
``;st``             step
=================== ======================

block clause keywords
~~~~~~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;ct``             catch
``;e``              else
``;ei``             elsif
``;o``              otherwise
``;w``              when
=================== ======================

predefined types
~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;a``              array
``;bi``             bigInteger
``;br``             bigRational
``;b3``             bin32
``;b6``             bin64
``;bt``             bitset
``;bo``             boolean
``;bs``             bstring
``;ca``             category
``;c``              char
``;cf``             clib_file
``;co``             color
``;cx``             complex
``;db``             database
``;du``             duration
``;en``             enum
``;ex``             expr
``;fi``             file
``;fs``             fileSys
``;fl``             float
``;h``              hash
``;i``              integer
``;ob``             object
``;pro``            process
``;pr``             program
``;rat``            rational
``;rf``             reference
``;rfl``            ref_list
``;s``              set
``;sq``             sqlStatement
``;sti``            string
``;stu``            struct
``;tx``             text
``;ti``             time
``;ty``             type
``;v``              void
``;pw``             PRIMITIVE_WINDOW
=================== ======================

predefined constants
~~~~~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;em``             empty
``;f``              FALSE
``;inf``            Infinity
``;t``              TRUE
=================== ======================

predefined variables
~~~~~~~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;ck``             CONSOLE_KEYBOARD
``;gk``             GRAPH_KEYBOARD
``;kb``             KEYBOARD
``;sc``             STD_CONSOLE
``;se``             STD_ERR
``;si``             STD_IN
``;sn``             STD_NULL
``;so``             STD_OUT
=================== ======================

errinfo values
~~~~~~~~~~~~~~

=================== ======================
Abbreviation        Expansion
=================== ======================
``;ok``             OKAY_NO_ERROR
``;ae``             ACTION_ERROR
``;ce``             COPY_ERROR
``;cre``            CREATE_ERROR
``;dbe``            DATABASE_ERROR
``;dse``            DESTROY_ERROR
``;fe``             FILE_ERROR
``;ge``             GRAPHIC_ERROR
``;ie``             INDEX_ERROR
``;ine``            IN_ERROR
``;me``             MEMORY_ERROR
``;ne``             NUMERIC_ERROR
``;oe``             OVERFLOW_ERROR
``;re``             RANGE_ERROR
=================== ======================

Code Navigation Commands
------------------------

Some of the commands have a built-in key binding in the seed7-key-map but not
all of them.  The `PEL Seed7 support`_ provides more key bindings using function keys.

= ============================ ============ =============================================================
. Function                     Key Binding  Description
= ============================ ============ =============================================================
. seed7-beg-of-defun           ``C-M-a``    Move point backward to beginning of function or procedure.
                                            With optional repeat argument.
. seed7-end-of-defun           ``C-M-e``    Move point backward to beginning of function or procedure.
                                            With optional repeat argument.
. seed7-beg-of-next-defun      ``C-c C-n``  Move point forward to beginning of next function or procedure.
                                            With optional repeat argument.
. seed7-to-block-forward       ``C-c C-e``  Move point forward to the end line of the matching statement:

                                            - array declaration (from begin to end)
                                            - ``block``,
                                            - `case statement`_:

                                              - Move from ``case`` to ``end case``
                                                but also across the ``when`` sections.

                                            - ``enum block``
                                            - any of the for statements:

                                              - `for`_
                                              - `for-each`_
                                              - `for-each-key`_
                                              - `for-key`_
                                              - `for-step`_
                                              - `for-until`_

                                            - `if statement`_:

                                              - Move from ``if`` to ``end if``,
                                                but also when at ``else`` or
                                                ``elsif`` move to the next portion.

                                            - `repeat - until statement`_
                                            - ``struct`` or
                                            - `while statement`_.

                                            If none is found move to the end of the function or procedure.

. seed7-to-block-backward      ``C-c C-a``  Move point backward to the beginning line of the matching
                                            block or statement (listed above).
= ============================ ============ =============================================================

Code Marking Commands
---------------------

= ============================ =========== =============================================================
. Function                     Key Binding Description
= ============================ =========== =============================================================
. seed7-mark-defun             ``C-M-h``   Mark the current function or procedure.
                                           With point between two; mark the next one.
= ============================ =========== =============================================================

Compilation Command
-------------------

= ============================ =========== =============================================================
. Function                     Key Binding Description
= ============================ =========== =============================================================
. seed7-compile                            Static check Seed7 file visited in current buffer.
                                           With optional argument compile it.
                                           All resulting warning or errors are shown in a compile-mode buffer.
= ============================ =========== =============================================================


- The static checking and compilation commands are identified in customizable user options.
- The static checking defaults to ``s7check`` and the compilation to ``s7c``.
- To perform static checking of Seed7 files, compile the `s7check.sd7`_
  part of seed7 program examples and use the generated executable.

Comment Management Commands
---------------------------

= ============================ =========== =============================================================
. Function                     Key Binding Description
= ============================ =========== =============================================================
. seed7-toggle-comment-style   ``C-c ;``   Toggle between comments to line-end and block comments.

                                           - Use ``comment-dwim`` and ``comment-block`` to create or
                                             remove comments of selected style.
                                             The default style is selected by **seed7-uses-block-comment**
                                             (off by default), and the **comment-style**
                                             customizable user-options.
= ============================ =========== =============================================================

Compatibility
=============

The seed7-mode is compatible with:

- Emacs `comment-dwim`_ command.  The recommended key binding for it is ``M-;``
- Emacs `which-function-mode`_, when active shows the name of the current Seed7 function or procedure in the
  mode line. It also works with Seed7 actions and forward declarations.
- The `iedit`_ package that allows selecting variables inside a specific block, function or procedure.
- The `expand-region`_ package to quickly select the current word, block, function/procedure.
- Drew Adam's `hide-comnt`_  package which  provides
  the `hide/show-comments-toggle` command to hide or show all comments.

More commands will be implemented.

.. ---------------------------------------------------------------------------

How To Install seed7-mode with plain vanilla Emacs
==================================================


Preliminary notes to new Emacs users
------------------------------------

:Reference: `The Emacs Initialization File`_

Emacs can and will use a user initialization file, `init.el` if it finds one.

- Emacs looks for the init.el file inside the directory identified by
  the `user-emacs-directory` variable, one of many variable controls Emacs behaviour.

  - Emacs looks for the following files, in the following order by default:

    - ``~/.emacs``
    - ``~/.emacs.el``
    - ``~/.emacs.d/init.el``
    - ``~.config/emacs/init.el``

- Once started the name of the Emacs init file is stored inside the value of the
  **user-init-file** variable.
- It also stores the name of the Emacs directory inside the **user-emacs-directory**
  variable.

Inside Emacs you can see the current value of the above variables by typing the ``C-h o``
followed by the name of the variable.  For example:

- Type ``C-h o user-emacs-directory RET``; that will open a buffer
  describing the purpose of this  variable and
  show it's current value.  It also has a link to the Emacs Lisp
  code that defines it (which is part of Emacs and you should not modify).
- Type ``C-h o user-init-file`` to show the value of this variable.

The `user-emacs-directory` identifies the directory where Emacs
looks for the init.el file.  In Unix-like OS installations it is often
set to `"~/.emacs.d/"`.  Under Windows it will be located somewhere else.

**Changing from ~/.emacs to ~/.emacs.d/init.el**

If you have used Emacs default you may be using the ``~/.emacs`` file for your
Emacs init file.

- Using a complete directory to hold your Emacs initialization
  file *and* other Emacs related files, like the downloaded packages, your
  spelling dictionaries, your persistent customization, etc...

- To get Emacs use the ``~/.emacs.d/init.el`` file instead:

  - Create the ``~/.emacs.d`` directory,
  - Move your ``~/.emacs`` or ``~/.emacs.el`` file to ``~/.emacs.d/init.el``.
  - When you restart Emacs, check the value of **user-emacs-directory** and
    **user-init-file**; they should reflect the new location.


Install seed7-mode for plain-vanilla Emacs
------------------------------------------

Make sure your Emacs initialization file is stored inside the ``~/.emacs.d``
directory and is ``~/.emacs.d/init.el``.  If this is not the case read the
previous section.  Once this is done proceed with the following:

- **1: Create the utils sub-directory** to store stand-alone utilities Emacs lisp files
  like seed7-mode.el.
  That directory should be located inside the directory
  identified by Emacs `user-emacs-directory`:

  - Under Unix-like OS, for example, you would normally create the `~/.emacs.d/utils` directory.

- **2: Create the init.el file if it does not exists**:

  - Emacs `user-emacs-directory` identifies the directory where the init.el
    file should be located.

    - Under Unix-like OS, the file is normally `~/.emacs.d/init.el`

  - Create the file if it does not already exist.

- **3: Update init.el: write code to find files in utils and auto-load seed7-mode**

  - Inside your init.el file, write the following code:

  .. code:: elisp

            ;;; -*-lexical-binding: t; -*-

            (push (expand-file-name "utils" user-emacs-directory) load-path)
            (autoload 'seed7-mode "seed7-mode" nil :interactive)
            (add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))

  - The first line activates lexical-binding.
    It **must** be the very first line of the file.
  - The other lines can be anywhere, but must be executed (in case you have
    some conditional logic).

- **4: Download seed7_mode.el file and copy it in the utils directory**

  - The utils directory is the one you created above.

- **5: Byte compile seed7-mode.el**

  - Open Emacs and edit (visit) the `seed7-mode.el` file located in your utils directory.
  - Byte compile it by typing the following command: `M-x emacs-lisp-byte-compile-and-load`

  Byte compiling is not absolutely necessary but it will verify that
  everything is ok inside the file and will also speed up Emacs startup.
  Just remember to byte-compile that file every time you modify it,
  otherwise Emacs will complain that it's using a byte-compile file
  that is older than the source file.


How To update seed7-mode in plain Emacs
=======================================

To update to a later revision,

- Erase the seed7-mode.el and
  seed7-mode.elc files from the utils directory where you stored them.
- Download the new revision in the same directory.
- Byte-compile the new file as described in the previous section.


.. ---------------------------------------------------------------------------

How to install and use seed7-mode with PEL
==========================================

You can also use my `PEL Emacs project`_ which deals with all installation and
control details of several packages including this seed7-mode.


- First `install PEL as described in the PEL manual`_
- To activate the installation and activation of the seed7-mode package you
  must set the PEL user-option for Seed7: **pel-use-seed7** to the value ``t``
  (which is one of the possible *true* values in Emacs Lisp).

  - Once PEL is installed, use the ``C-h o pel-use-seed7 RET``  key
    sequence to open the customization buffer to set this user option.  Then close
    Emacs and restart it. PEL will download and install the file in your
    ``~/.emacs.d/utils`` directory.
  - Open a Seed7 file, PEL provides extra command key bindings for Seed7 under
    the ``F12`` key prefix.

    - See the `PEL Seed7 PDF`_ for more information about PEL Seed7 Support.
    - The `PEL Index PDF`_ has links to several other PDF files on various
      Emacs-specific topics.

How to Update Seed7-Mode with PEL
=================================

With PEL, updating is a little simpler:
just delete your ``~/.emacs.d/utils/seed7-mode.*`` files and restart Emacs;
it will download the new version and byte-compile it.


.. ---------------------------------------------------------------------------

Future
======


Once this code is stable I will add the logic to make it a proper Emacs
package and probably will include it under MELPA.  But the code is far from
being ready for that.

Any help, questions, suggestions are welcome!

.. ---------------------------------------------------------------------------
.. links


.. _Terminal-mode Emacs:             screenshots/terminal-example-01.png
.. _Graphical Emacs:                 screenshots/graphic-light-example-01.png
.. _Terminal Emacs Speedbar:         screenshots/terminal-seed7-speedbar-01.png
.. _GUI Emacs Speedbar:              screenshots/macOS-gui-speedbar-frame.png
.. _GUI Emacs menu:                  screenshots/macOS-gui-menu-01.png
.. _Terminal menu 1:                 screenshots/terminal-menu-01.png
.. _Terminal menu 2:                 screenshots/terminal-menu-02.png
.. _Terminal Ivy prompt:             screenshots/terminal-imenu-gh-01.png
.. _bas.sd7 outline all collapsed:   screenshots/terminal-outline-minor-mode.png
.. _bas.sd7 outline with 2 expanded: screenshots/terminal-outline-minor-mode-01.png
.. _Navigation to structure menu:    screenshots/menu-defs-struct.png
.. _Comments menu:                   screenshots/menu-comments.png
.. _Template insertion menu:         screenshots/menu-insert.png
.. _Emacs imenu mode:                           https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html
.. _Emacs Speedbar:                             https://www.gnu.org/software/emacs/manual/html_node/speedbar/
.. _while statement:                            https://seed7.sourceforge.net/manual/stats.htm#while-statement
.. _repeat - until statement:                   https://seed7.sourceforge.net/manual/stats.htm#repeat-statement
.. _if statement:                               https://seed7.sourceforge.net/manual/stats.htm#if-statement
.. _for statement:
.. _for:                                        https://seed7.sourceforge.net/manual/stats.htm#for-statement
.. _for-each statement:
.. _for-each:                                   https://seed7.sourceforge.net/manual/stats.htm#for-each-statement
.. _for-each-key statement:
.. _for-each-key:                               https://seed7.sourceforge.net/manual/stats.htm#for-each-key-statement
.. _for-key statement:
.. _for-key:                                    https://seed7.sourceforge.net/manual/stats.htm#for-key-statement
.. _for-step statement:
.. _for-step:                                   https://seed7.sourceforge.net/manual/stats.htm#for-step-statement
.. _for-until statement:
.. _for-until:                                  https://seed7.sourceforge.net/manual/stats.htm#for-until-statement
.. _case statement:                             https://seed7.sourceforge.net/manual/stats.htm#case-statement
.. _Exception handler block:                    https://seed7.net/manual/errors.htm
.. _s7check.sd7:                                https://github.com/pierre-rouleau/seed7/blob/master/prg/s7check.sd7
.. _iedit:                                      https://github.com/victorhge/iedit
.. _expand-region:                              https://github.com/magnars/expand-region.el?tab=readme-ov-file#readme
.. _hide-comnt:                                 https://github.com/emacsmirror/hide-comnt
.. _The Emacs Initialization File:              https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
.. _PEL Emacs project:                          https://github.com/pierre-rouleau/pel?tab=readme-ov-file#readme
.. _install PEL as described in the PEL manual: https://github.com/pierre-rouleau/pel/blob/master/doc/pel-manual.rst#how-to-install-pel
.. _PEL Seed7 support:
.. _PEL Seed7 PDF:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/pl-seed7.pdf
.. _PEL Index PDF:                              https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/-index.pdf
.. _PEL Speedbar PDF:                           https://raw.githubusercontent.com/pierre-rouleau/pel/master/doc/pdf/speedbar.pdf
.. _Emacs outline minor mode:                   https://www.gnu.org/software/emacs/manual/html_node/emacs/Outline-Minor-Mode.html
.. _comment-dwim:                               https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html
.. _which-function-mode:                        https://www.gnu.org/software/emacs/manual/html_node/emacs/Which-Function.html
.. _Emacs tempo markers:                        https://www.gnu.org/software/emacs/manual/html_node/autotype/Tempo.html
.. _value-parameter:                            https://seed7.sourceforge.net/manual/params.htm#val_parameter
.. _reference-parameter:                        https://seed7.sourceforge.net/manual/params.htm#ref_parameter
.. _in-parameter:                               https://seed7.sourceforge.net/manual/params.htm#in_parameter
.. _in-var-parameter:                           https://seed7.sourceforge.net/manual/params.htm#in_var_parameter
.. _inout-parameter:                            https://seed7.sourceforge.net/manual/params.htm#inout_parameter
.. _call-by-name parameter:                     https://seed7.sourceforge.net/manual/params.htm#call_by_name_parameter
.. _constant declaration:                       https://seed7.sourceforge.net/manual/decls.htm#Constant_declarations
.. _variable declaration:                       https://seed7.sourceforge.net/manual/decls.htm#Variable_declarations
.. _procedure declaration:                      https://seed7.sourceforge.net/manual/decls.htm#Procedure_declarations
.. _short function declaration:
.. _function declaration:                       https://seed7.sourceforge.net/manual/decls.htm#Function_declarations
.. _enum type declaration:                      https://seed7.sourceforge.net/manual/types.htm#enumeration
.. _struct type declaration:                    https://seed7.sourceforge.net/manual/types.htm#struct
.. _Abbrev Concepts:                            https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrev-Concepts.html
.. _Abbrevs @ Emacs Manual:                     https://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
.. _Examining and Editing Abbrevs:              https://www.gnu.org/software/emacs/manual/html_node/emacs/Editing-Abbrevs.html#Editing-Abbrevs

.. ---------------------------------------------------------------------------

..  LocalWords:  PEL
