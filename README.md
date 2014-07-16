# Company-AUCTeX

This is a group of backends for [company-mode](https://github.com/company-mode/company-mode/) providing auto-completion for [AUCTeX](https://www.gnu.org/software/auctex/).

It is adapted from [auto-complete-auctex](https://github.com/monsanto/auto-complete-auctex/).

## Installation

1. From MELPA (see http://melpa.milkbox.net/#/getting-started for enabling it, if required):

        M-x package-install RET company-auctex RET

2. From Github:

        git clone https://github.com/alexeyr/company-auctex.git

  In the initialization file (`~/.emacs`, `~/.emacs.d/init.el`, etc.):

        (add-to-list 'load-path "path/to/company-auctex.el")
        (require 'company-auctex)

Then require the package and initialize it:

    (company-auctex-init)
    
## Issues

If `company-backends` is set directly without using `push`, `add-to-list` or similar functions (e.g. by Customize interface), `(company-auctex-init)` must be run after this (or its backends added to the same place).

## To-do

1. Expand README (add features, screenshots).

2. Support inserting Unicode characters in non-TeX modes (similar to [ac-math](https://github.com/vitoshka/ac-math)).

3. Some commands (\begin, \emph, etc.) aren't getting completed. Need to check if they are in any lists provided by AUCTeX.
