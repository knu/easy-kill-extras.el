# easy-kill-extras.el

This package contains extra functions for
[easy-kill/easy-mark](https://github.com/leoliu/easy-kill).

## Functions

Here is a list of the interactive commands provided by easy-kill-extras:

* easy-mark-word
* easy-mark-sexp
* easy-mark-to-char
* easy-mark-up-to-char

  These are shorthand commands for easy-marking an aimed string at
  point.

* easy-kill-er-expand
* easy-kill-er-unexpand

  These work like `er/expand-region` and `er/contract-region`,
  respectively, integrating the functionality of the `expand-region`
  package into `easy-kill`.

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

## Extra "Things"

This package includes `extra-things.el` which defines various extra "things" that can be used with easy-kill/easy-mark.

``` elisp
(require 'extra-things)

;; example settings
(add-to-list 'easy-kill-alist '(?W  WORD " ") t)
(add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
(add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
(add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
(add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
(add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
(add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
(add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
(add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
(add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
(add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
(add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
(add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
(add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t)
```

* `WORD`: a sequence of non-whitespace characters

  This is much like Vim's WORD object.

* `squoted-string`: a sindle-quoted string ('...')
* `dquoted-string`: a double-quoted string ("...")
* `bquoted-string`: a back-quoted string (`...`)
* `quoted-string`: any of the above quoted strings

  The backslash character serves as escape character.  For performance
  reasons, it is assumed that the beginning of the current line is not
  inside of a quoted string.  In other words, multi-line quoted
  strings are not fully supported.

  These things are aware of the current syntax table, and the
  quotation marks that are a word constituent or an expression prefix
  in the current mode are ignored.  For example, `squoted-string`
  would only work in some specific programming language modes where
  the single quotation mark is a quotation character.

* `squoted-string-universal`
* `dquoted-string-universal`
* `bquoted-string-universal`
* `quoted-string-universal`

  These versions recognize all quotation pairs ignoring the current
  syntax table and support nesting of different quotations.

* `parentheses-pair`: the block between a parentheses pair including the opening and closing parentheses
* `brackets-pair`: the block between a brackets pair including the opening and closing brackets
* `curlies-pair`: the block between a curlies pair including the opening and closing curlies
* `angles-pair`: the block between an angles pair including the opening and closing angles
* `parentheses-pair-content`: the content inside of a parentheses pair without whitespace at both ends
* `brackets-pair-content`: the content inside of a brackets pair without whitespace at both ends
* `curlies-pair-content`: the content inside of a curlies pair without whitespace at both ends
* `angles-pair-content`: the content inside of an angles pair without whitespace at both ends

  Quotation marks or different types of pair characters are not
  taken into account.  Each type of things only cares about the
  nest level of their pair characters.

  Repeatedly moving forward or backward means to move to outer pairs.

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

;; Integrate `expand-region' functionality with easy-kill
(define-key easy-kill-base-map (kbd "o") 'easy-kill-er-expand)
(define-key easy-kill-base-map (kbd "i") 'easy-kill-er-unexpand)

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

Copyright (c) 2014-2023 Akinori MUSHA.

Licensed under the 2-clause BSD license.  See `LICENSE.txt` for
details.

Visit [GitHub Repository](https://github.com/knu/easy-kill-extras.el) for the
latest information.
