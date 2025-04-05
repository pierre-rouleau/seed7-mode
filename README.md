# seed7-mode
Emacs support for the Seed7 Programming language

⚠️  This is far from being complete  🚧  This is **work-in-progress**.


# How To Install seed7-mode #

## Install seed7 in plain-vanilla Emacs ##

- Create a directory to store Emacs lisp files.
  I recommend creating the `~/.emacs.d/utils` directory for that.
  Emacs `user-emacs-directory` should already be set to `~/.emacs.d`, it's default value.
  Add this directory to the beginning of Emacs `load-path`.

- Assuming that Emacs `user-emacs-directory` is set to `~/.emacs.d`,
  and that you do not have any init.el file, create the file
  `~/.emacs.d.init.el`.

- Inside the `~/.emacs.d.init.el` file write the following:

  ```elisp
  ;;; -*-lexical-binding: t; -*-

  (push (expand-file-name "utils" user-emacs-directory) load-path)
  (autoload 'seed7-mode "seed7-mode" nil :interactive)
  (add-to-list 'auto-mode-alist '("\\.s[di]7\\'" . seed7-mode))
  ```

- Download the seed7_mode.el file and copy it inside the
  `~/.emacs.d/utils` directory.

- With the file in place, open Emacs and edit (visit) the
  `~/.emacs.d/utils/seed7-mode.el` file to byte compile it.
  Byte compiling is not absolutely necessary but it will verify that
  everything is ok inside the file and will also speed up Emacs startup.
  Just remember to byte-compile that file every time you modify it,
  otherwise Emacs will complain that it's using a byte-compile file
  that is older than the source file.

  - To byte compile the file that is visited in the current buffer,
    type the following command:

    `M-x emacs-lisp-byte-compile-and-load`

**To update**

To update to a later revision, erase the seed7-mode.el and
seed7-mode.elc files from the directory where you stored them
(which would be `~/.emacs.d/utils` as described above),
and download the new revision in the same directory.

Byte-compile the new file.

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
