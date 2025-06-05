=============================================================
Seed7-mode - Emacs support for the Seed7 Programming Language
=============================================================


⚠️  Early, **work-in-progress** version of seed7-mode  🚧 .

Currently Implemented Features
==============================

- Syntax highlighting with user-customizable faces.
- Supports `Emacs outline minor mode`_: collapse and expansion of blocks,
  where heading is the first line of a declaration block. This allows
  navigation and code manipulation by headings (blocks).
- Provides a Seed7 top level menu entry.
- Support `Emacs imenu mode`_ and `Emacs Speedbar`_ for:

  - Seed7 procedures,
  - Seed7 functions,
  - Seed7 interfaces,
  - Seed7 structures,
  - Seed7 enums.

- Automatic indentation.
  The indentation is done automatically when the TAB key is pressed
  (from anywhere on the line). It's also done when the RETURN key is pressed.
  It support the ``auto-fill`` mode.

  - To disable auto-indentation, set ``seed7-auto-indent`` to nil, then
    disable and re-enable ``seed7-mode``.
    You can also disable it permanently by customizing it.
  - The auto-indent logic expects blocks to be complete, as it checks
    them for completion and nesting. That mechanism may change in the future,
    providing a less strict support.  However, during development, it allows
    me to detect more errors.
  - One aspect of the auto indentation logic is that it checks (and extracts)
    the name of procedure and functions and the return type of the function.
    The auto-indentation will only work properly once these are identified.
    This acts as a reminder to fill in the missing parts.
  - As this code is still under development please report any problem you
    may encounter.

- Automatic completion of the following statement when typing the ``<TAB>``
  key after the following keywords appearing at the beginning of a line
  inside Seed7 code:

  - **case**, **if**, **repeat**, **while**,
  - The 9 variations of for loops:

    - **for** expands to the `for statement`_,
    - **foru** expands to the `for-until statement`_,
    - **fors** expands to the `for-step statement`_,
    - **fore** expands to the `for-each statement`_,
    - **foreu** expands to the `for-each statement`_ combined with an until condition,
    - **forek** expands to the `for-each-key statement`_,
    - **foreku** expands to the `for-each-key statement`_ combined with an until condition,
    - **fork** expands to the `for-key statement`_,
    - **forku** expands to the `for-key statement`_ combined with an until condition,

  - **proc** and **func** expand to the boiler plate code for procedure and
    function.
  - **funcs** expands to the boiler plate code for short function.
  - **inc** expands to a partly filled include statement.
  - **enum** expands to a enum declaration.
  - **struct** expands to a structure declaration statement.
  - **var** expands to a variable declaration.


See the following example screenshots:

=========================== ================================================
Screenshot                  Description
=========================== ================================================
`Terminal-mode Emacs`_      Shows the default highlighting of Seed7 code on
                            Emacs running in a macOS Terminal.

`Graphical Emacs`_          Shows the default highlighting of Seed7 code on
                            a basic GUI Emacs running in a macOS with the
                            default scheme.

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

Implemented Commands
--------------------

Code Navigation Commands
~~~~~~~~~~~~~~~~~~~~~~~~

Some of the commands have a built-in key binding in the seed7-key-map but not
all of them.  The `PEL Seed7 support`_ provides more key bindings using function keys.

= ============================ =========== =============================================================
. Function                     Key Binding Description
= ============================ =========== =============================================================
. seed7-beg-of-defun           ``C-M-a``   Move point backward to beginning of function or procedure.
                                           With optional repeat argument.
. seed7-end-of-defun           ``C-M-e``   Move point backward to beginning of function or procedure.
                                           With optional repeat argument.
. seed7-beg-of-next-defun                  Move point forward to beginning of next function or procedure.
                                           With optional repeat argument.
. seed7-to-block-forward                   Move point forward to the end line of the matching statement:

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

. seed7-to-block-backward                  Move point backward to the beginning line of the matching
                                           block or statement (listed above).
= ============================ =========== =============================================================

Code Marking Commands
~~~~~~~~~~~~~~~~~~~~~

= ============================ =========== =============================================================
. Function                     Key Binding Description
= ============================ =========== =============================================================
. seed7-mark-defun             ``C-M-h``   Mark the current function or procedure.
                                           With point between two; mark the next one.
= ============================ =========== =============================================================

Compilation Command
~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

- Emacs comment-dwim.  The recommended key binding for it is ``M-;``
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


.. _Terminal-mode Emacs:      screenshots/terminal-example-01.png
.. _Graphical Emacs:          screenshots/graphic-light-example-01.png
.. _Terminal Emacs Speedbar:  screenshots/terminal-seed7-speedbar-01.png
.. _GUI Emacs Speedbar:       screenshots/macOS-gui-speedbar-frame.png
.. _GUI Emacs menu:           screenshots/macOS-gui-menu-01.png
.. _Terminal menu 1:          screenshots/terminal-menu-01.png
.. _Terminal menu 2:          screenshots/terminal-menu-02.png
.. _Terminal Ivy prompt:      screenshots/terminal-imenu-gh-01.png

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


.. ---------------------------------------------------------------------------

..  LocalWords:  PEL
