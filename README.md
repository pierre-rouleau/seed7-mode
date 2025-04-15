# seed7-mode
Emacs support for the Seed7 Programming language

⚠️  Early, **work-in-progress** version of seed7-mode  🚧 .

# Currently Implemented Features #

- Syntax highlighting with user-customizable faces.
  See [terminal-mode Emacs](screenshots/terminal-example-01.png) and
  [graphical Emacs](screenshots/graphic-light-example-01.png) screen-shots of Seed7 library code.

- Support [Emacs imenu mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html) and
  [Emacs Speedbar](https://www.gnu.org/software/emacs/manual/html_node/speedbar/) for:

  - Seed7 procedures,
  - Seed7 functions,
  - Seed7 interfaces,
  - Seed7 structures,
  - Seed7 enums.

- Implemented Commands:

  - Code Navigation Commands:

    | Function | Key Binding | Description |
    | -------- | ----------- | ----------- |
    | seed7-beg-of-defun | `C-M-a` | Move point backward to beginning of function or procedure. Optional repeat argument. |
    | seed7-end-of-defun | `C-M-e` | Move point backward to beginning of function or procedure. Optional repeat argument. |
    | seed7-beg-of-next-defun |    | Move point forward to beginning of next function or procedure. Optional repeat argument. |
    | seed7-to-block-backward |    | Move point backward to the beginning line of the matching `block`, `case`, `enum`, `for`, `if`, `struct` or `while` statement. |
    | seed7-to-block-forward  |    | Move point forward to the end line of the matching `block`, `case`, `enum`, `for`, `if`, `struct` or `while` statement. |

  - Code Marking Commands:

    | Function | Key Binding | Description |
    | -------- | ----------- | ----------- |
    | seed7-mark-defun | `C-M-h` | Mark the current function or procedure. |

  - Compilation commands:

    | Function | Key Binding | Description |
    | -------- | ----------- | ----------- |
    | seed7-compile |  | Static check Seed7 file visited in current buffer. With optional argument compile it. |


   Also:

   - The static checking and compilation commands are identified in customizable user options.
   - The static checking defaults to `s7check` and the compilation to `s7c`.
   - Currently the `s7check` is not part of seed7 distribution but you can use
     this [simple Seed7 program](https://github.com/ThomasMertes/seed7/issues/34#issuecomment-2789748990)
     that implements it.

# Compatibility #

The seed7-mode is compatible with:

- Emacs comment-dwim.  The recommended key binding for it is `M-;`
- [iedit](https://github.com/victorhge/iedit)
- [expand-region](https://github.com/magnars/expand-region.el?tab=readme-ov-file#readme).


More commands will be implemented.

# How To Install seed7-mode #

**Preliminary notes to new Emacs users:**

Emacs can and will use a user initialization file, `init.el` if it find one.
See [The Emacs Initialization File](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
for more info.

Emacs looks for the init.el file inside the directory identified by
the `user-emacs-directory` variable, one of many variable controls Emacs behaviour.

To see the current value of `user-emacs-directory`,
type `C-h o user-emacs-directory RET`.  That will open a buffer
describing the purpose of this  variable and
show it's current value.  It also has a link to the Emacs Lisp
code that defines it (which is part of Emacs and you should not modify).

The `user-emacs-directory` identifies the directory where Emacs
looks for the init.el file.  In Unix-like OS installations it is often
set to `"~/.emacs.d/"`.  Under Windows it will be located somewhere else.


## Install seed7-mode for plain-vanilla Emacs ##

- **Create the utils sub-directory** to store stand-alone utilities Emacs lisp files
  like seed7-mode.el.
  That directory should be located inside the directory
  identified by Emacs `user-emacs-directory`.

  - Under Unix-like OS, for example, you would normally create the `~/.emacs.d/utils` directory.

- **Create the init.el file and add the following code**

  - Emacs `user-emacs-directory` identifies the directory where the init.el file should be located.
    - Under Unix-like OS, the file is normally `~/.emacs.d/init.el`
  - Create the file if it does not already exist.

- **In init.el, write code to find files in utils and auto-load seed7-mode**

  - Inside your init.el file, write the following code:

  ```elisp
  ;;; -*-lexical-binding: t; -*-

  (push (expand-file-name "utils" user-emacs-directory) load-path)
  (autoload 'seed7-mode "seed7-mode" nil :interactive)
  (add-to-list 'auto-mode-alist '("\\.s\\(d7\\|7i\\)\\'" . seed7-mode))
  ```

  The first line activates lexical-binding.
  It **must** be the very first line of the file.

  The other lines can be anywhere (but must be executed).

- **Download seed7_mode.el file and copy it in the utils directory**

  - The utils directory is the one you created above.

- **Byte compile seed7-mode.el**

  - Open Emacs and edit (visit) the `seed7-mode.el` file located in your utils directory.
  - Byte compile it by typing the following command: `M-x emacs-lisp-byte-compile-and-load`

  Byte compiling is not absolutely necessary but it will verify that
  everything is ok inside the file and will also speed up Emacs startup.
  Just remember to byte-compile that file every time you modify it,
  otherwise Emacs will complain that it's using a byte-compile file
  that is older than the source file.


### To update seed7-mode in plain Emacs ###

To update to a later revision, erase the seed7-mode.el and
seed7-mode.elc files from the utils directory where you stored them
and download the new revision in the same directory.

Byte-compile the new file as described above.

## Install seed7-mode with PEL ##

You can also use my [PEL Emacs project](https://github.com/pierre-rouleau/pel)
which deals with all installation and
control details when the `pel-use-seed7` user-option is customized to `t`.
Once PEL is installed, use the <kbd>C-h o pel-use-seed7 RET</kbd> key
sequence to open the customization buffer to set this user option.  Then close
Emacs and restart it. PEL will down load and install the file in your
`~/.emacs.d/utils` directory and provide PEL commands available under the
<kbd> <f12></kbd> key prefix.

**To update**

With PEL, updating is a little simpler:
just delete your `~/.emacs.d/utils/seed7-mode.*` files and restart Emacs;
it will download the new version and byte-compile it.

# Future #

Once this code is stable I will add the logic to make it a proper Emacs
package and probably will include it under MELPA.  But the code is far from
being ready for that.

Any help, questions, suggestions are welcome!
