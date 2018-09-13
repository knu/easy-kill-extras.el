# easy-kill-extras.el

This package contains extra functions for
[easy-kill/easy-mark](https://github.com/leoliu/easy-kill).

## Functions

Here is a list of the interactive commands provided by easy-kill-extras:

* easy-mark-word
* easy-mark-sexp
* easy-mark-to-char
* easy-mark-up-to-char

## Kill Targets

It also provides the following easy-kill/easy-mark targets:

* buffer

  This selects the whole buffer.

* buffer-before-point
* buffer-after-point

  These work like vi's gg/G commands, respectively.

* backward-line-edge
* forward-line-edge

  The former is like vi's `^`/`0` commands, and the latter is just
  like that in the opposite direction.

* string-to-char-forward
* string-to-char-backward
* string-up-to-char-forward
* string-up-to-char-backward

  These work like vi's `f`/`F`/`t`/`T` commands, respectively.

## Support for packages

Experimental ace-jump integration into easy-kill is enabled by
default.  `ace-jump-*-mode` can be invoked for selection when in
easy-kill/easy-mark mode.  You can disable this feature via a
customize variable `easy-kill-ace-jump-enable-p`.

Experimental multiple-cursors-mode support for easy-kill is enabled by
default.  `easy-kill` and `easy-mark` will mostly work in
`multiple-cursors-mode`.

## Configuration

Suggested settings are as follows:

``` elisp
;; Upgrade `mark-word' and `mark-sexp' with easy-mark
;; equivalents.
(global-set-key (kbd "M-@") 'easy-mark-word)
(global-set-key (kbd "C-M-@") 'easy-mark-sexp)

;; `easy-mark-to-char' or `easy-mark-up-to-char' could be a good
;; replacement for `zap-to-char'.
(global-set-key [remap zap-to-char] 'easy-mark-to-char)

;; Add the following tuples to `easy-kill-alist', preferrably by
;; using `customize-variable'.
(add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
(add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
(add-to-list 'easy-kill-alist '(?b buffer ""))
(add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
(add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
(add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
(add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
(add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
(add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))
```

## Installation

This package is available on [MELPA](http://melpa.org/).

## Configuration

Suggested key bindings are as follows:

```elisp
(define-key mc/keymap (kbd "C-. M-C-f") 'mc/mark-next-sexps)
(define-key mc/keymap (kbd "C-. M-C-b") 'mc/mark-previous-sexps)
(define-key mc/keymap (kbd "C-. <") 'mc/mark-all-above)
(define-key mc/keymap (kbd "C-. >") 'mc/mark-all-below)

(define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
(define-key mc/keymap (kbd "C-. C-k") 'mc/remove-cursors-at-eol)
(define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)
(define-key mc/keymap (kbd "C-. C-o") 'mc/remove-cursors-on-blank-lines)

(define-key mc/keymap (kbd "C-. C-.") 'mc/freeze-fake-cursors-dwim)

(define-key mc/keymap (kbd "C-. .")   'mc/move-to-column)
(define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)

;; Emacs 24.4+ comes with rectangle-mark-mode.
(define-key rectangle-mark-mode-map (kbd "C-. C-,") 'mc/rect-rectangle-to-multiple-cursors)

(define-key cua--rectangle-keymap   (kbd "C-. C-,") 'mc/cua-rectangle-to-multiple-cursors)
```

To enable interaction between multiple cursors and CUA rectangle copy
& paste:

```elisp
(mc/cua-rectangle-setup)
```

## Author

Copyright (c) 2014-2018 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for
details.

Visit [GitHub Repository](https://github.com/knu/easy-kill-extras.el) for the
latest information.
