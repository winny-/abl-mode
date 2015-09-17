# abl-mode
An Emacs major mode for editing Progress OpenEdge/Advanced Business Language files.

#Installation

Download `abl-mode.el`, put it in your `~/.emacs.d/` directory (or somehwere on
your Emacs load path), and add `(load-file "abl-mode.el")` to your
`~/emacs.d/init.el` and you should open `*.p` files in ABL mode (or you can
invoke it explicitly with `M-x abl-mode`).


#Contributing

Improve `abl-mode` by

 * Using it (if you find yourself having to program in ABL)
 * Open [issues](https://github.com/neganp/abl-mode/issues) if you find bugs or
 want to suggest new features (better support for automatic indentation would
 be great!).
 * Open a pull request.


#Roadmap
New features that might be developed are

 * Automatic indentation
 * Skeletons for common ABL boilerplate
 * Better integration with Emacs's Package system

Copyright (C) 2015 by Nathaniel Knight.

Provided under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

See `license.txt` for copying permissions.

