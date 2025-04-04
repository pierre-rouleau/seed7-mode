# seed7-mode
Emacs support for the Seed7 Programming language

⚠️  This is far from being complete  🚧  This is **work-in-progress**.


To install in your environment, just copy the seed7-mode.el inside a directory
that is located in your Emacs `load-path` and byte-compile it.

Then add the following inside your Emacs initialization file:

``` elisp
(autoload 'seed7-mode "seed7-mode" nil :interactive)
(add-to-list 'auto-mode-alist '("\\.s[di]7\\'" . seed7-mode))
```

You can also use my [PEL Emacs project](https://github.com/pierre-rouleau/pel)
which deals with all installation and
control details when the `pel-use-seed7` user-option is customized to `t`.
Once PEL is installed, use the <kbd>C-h o pel-use-seed7 RET</kbd> key
sequence to open the customization buffer to set this user option.  Then close
Emacs and restart it. PEL will down load and install the file in your
`~/.emacs.d/utils` directory and provide PEL commands available under the
<kbd> <f12></kbd> key prefix.

Once this code is stable I will add the logic to make it a proper Emacs
package and probably will include it under MELPA.  But the code is far from
being ready for that.

Any help, questions, suggestions are welcome!
