;;; easy-kill-extras.el --- Extra functions for easy-kill.

;; Copyright (c) 2014-2015 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 29 Jul 2014
;; Version: 0.9.3
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This package contains extra functions for easy-kill/easy-mark:
;;
;; * easy-mark-word
;; * easy-mark-sexp
;; * easy-mark-to-char
;; * easy-mark-up-to-char
;; * easy-kill-delete-region
;;
;; It also provides the following easy-kill/easy-mark targets:
;;
;; * `buffer'
;;
;;   This selects the whole buffer.
;;
;; * `buffer-before-point'
;; * `buffer-after-point'
;;
;;   These work like vi's gg/G commands, respectively.
;;
;; * `backward-line-edge'
;; * `forward-line-edge'
;;
;;   The former is like vi's ^/0 commands, and the latter is just like
;;   that in the opposite direction.
;;
;; * `string-to-char-forward'
;; * `string-to-char-backward'
;; * `string-up-to-char-forward'
;; * `string-up-to-char-backward'
;;
;;   These work like vi's f/F/t/T commands, respectively.
;;
;; Experimental ace-jump support in easy-kill/easy-mark mode is
;; enabled by default.  You can disable it via a customize variable
;; `easy-kill-ace-jump-enable-p'.
;;
;; Suggested settings are as follows:
;;
;;   ;; Upgrade `mark-word' and `mark-sexp' with easy-mark
;;   ;; equivalents.
;;   (global-set-key (kbd "M-@") 'easy-mark-word)
;;   (global-set-key (kbd "C-M-@") 'easy-mark-sexp)
;;
;;   ;; `easy-mark-to-char' or `easy-mark-up-to-char' could be a good
;;   ;; replacement for `zap-to-char'.
;;   (global-set-key [remap zap-to-char] 'easy-mark-to-char)
;;
;;   ;; `delete-region' is automatically remapped, but you'll want to
;;   ;; have these if you are in love with `delete-selection-mode'.
;;   (define-key easy-kill-base-map (kbd "C-d") 'easy-kill-delete-region)
;;   (define-key easy-kill-base-map (kbd "DEL") 'easy-kill-delete-region)
;;
;;   ;; Add the following tuples to `easy-kill-alist', preferrably by
;;   ;; using `customize-variable'.
;;   (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
;;   (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
;;   (add-to-list 'easy-kill-alist '(?b buffer ""))
;;   (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
;;   (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
;;   (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
;;   (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
;;   (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
;;   (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward ""))

;;; Code:

(require 'easy-kill)

;;;###autoload
(defadvice easy-mark
    (around per-thing activate)
  "Enable `easy-mark-word' and `easy-mark-sexp'."
  (let ((easy-mark-try-things
         (pcase this-command
           (`easy-mark-word
            (if (bound-and-true-p subword-mode)
                '(subword) '(word)))
           (`easy-mark-sexp
            '(sexp))
           (`easy-mark-to-char
            '(string-to-char-forward))
           (`easy-mark-up-to-char
            '(string-up-to-char-forward))
           (_
            easy-mark-try-things))))
    ad-do-it))

;;;###autoload
(defun easy-mark-word (n)
  "Start easy-mark with a word selected."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(defun easy-mark-sexp (n)
  "Start easy-mark with a sexp selected."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(defun easy-mark-to-char (n)
  "Start easy-mark with string-to-char-forward."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(defun easy-mark-up-to-char (n)
  "Start easy-mark with string-up-to-char-forward."
  (interactive "p")
  (easy-mark n))

;;;###autoload
(eval-after-load 'easy-kill
  '(put 'easy-kill-delete-region 'easy-kill-exit t))

;;;###autoload
(defun easy-kill-delete-region ()
  "Delete the easy-kill selection without modifying the kill ring."
  (interactive)
  (pcase (easy-kill-get bounds)
    (`(,beg . ,end) (delete-region beg end))))

;;;###autoload
(eval-after-load 'easy-kill
  '(define-key easy-kill-base-map [remap delete-region] 'easy-kill-delete-region))

;;;###autoload
(defun forward-line-edge (arg)
  "Move between line edges.  ARG specifies which edge to move to.

If ARG is -2 or less, move to the BOL.

If ARG is -1, move to the first non-whitespace character after
the point on the line, or BOL if there is none.

If ARG is 0, stay.

If ARG is 1, move to the position right after the last
non-whitespace character after the point on the line, or EOL if
there is none.

If ARG is 2 or greater, move to the EOL."
  (interactive "p")
  (pcase arg
    (0)
    (1
     (if (looking-at "\\(.*[^[:space:]]\\)[[:space:]]+$")
         (goto-char (match-end 1))
       (end-of-line)))
    ((pred (<= 2))
     (end-of-line))
    (-1
     (if (looking-back "^[[:space:]]*")
         (beginning-of-line)
       (back-to-indentation)))
    (_
     (beginning-of-line))))

;;;###autoload
(defun backward-line-edge (arg)
  "Equivalent to `forward-line-edge' with a negative ARG."
  (interactive "p")
  (forward-line-edge (- arg)))

;;;###autoload
(defun easy-kill-on-forward-line-edge (n)
  "Provide an easy-kill target `forward-line-edge', which works like vi's `^'/`0' commands in the opposite direction."
  (easy-kill-adjust-candidate 'forward-line-edge
                              (point)
                              (save-excursion
                                (forward-line-edge
                                 (pcase n
                                   (`+ 2)
                                   (`- 1)
                                   (1 (if (eq (easy-kill-get thing) 'forward-line-edge) 2 1))
                                   (_ n)))
                                (point))))

;;;###autoload
(defun easy-kill-on-backward-line-edge (n)
  "Provide an easy-kill target `backward-line-edge', which works like vi's `^'/`0' commands."
  (easy-kill-adjust-candidate 'backward-line-edge
                              (point)
                              (save-excursion
                                (backward-line-edge
                                 (pcase n
                                   (`+ 2)
                                   (`- 1)
                                   (1 (if (eq (easy-kill-get thing) 'backward-line-edge) 2 1))
                                   (_ n)))
                                (point))))

;;;###autoload
(defun easy-kill-on-buffer (n)
  "Provide an easy-kill target `buffer' which selects the whole buffer."
  (easy-kill-adjust-candidate 'buffer (point-min) (point-max)))

;;;###autoload
(defun easy-kill-on-buffer-after-point (n)
  "Provide an easy-kill target `buffer-after-point', which works like vi's `G' command.
The +/- operation determines inclusion/exclusion of the current line."
  (easy-kill-adjust-candidate 'buffer-after-point
                              (pcase n
                                (`+
                                 (point-at-bol))
                                (`-
                                 (point-at-bol 2))
                                (_
                                 (point)))
                              (point-max)))

;;;###autoload
(defun easy-kill-on-buffer-before-point (n)
  "Provide an easy-kill target `buffer-before-point', which works like vi's `gg' command.
The +/- operation determines inclusion/exclusion of the current line."
       (easy-kill-adjust-candidate 'buffer-before-point
                                   (point-min)
                                   (pcase n
                                     (`+
                                      (point-at-bol 2))
                                     (`-
                                      (point-at-bol))
                                     (_
                                      (point)))))

(defmacro easy-kill-defun-string-to-char (include backward)
  "Macro to define string-to-char functions."
  (let* ((command-prefix "easy-kill-on-")
         (format-name (lambda (include_ backward_ &optional prefix)
                        (format "string-%s-char-%s"
                                (if include_ "to" "up-to")
                                (if backward_ "backward" "forward"))))
         (name (funcall format-name include backward))
         (prompt (format "%s character %s: "
                         (if include "To" "Up to")
                         (if backward "backward" "forward")))
         (search-func (if backward 'search-backward 'search-forward)))
    `(defun ,(intern (concat command-prefix name)) (n)
       ,(format "Provide an easy-kill-target `%s', which works like vi's `%s' command."
                name
                (if include (if backward "F" "f")
                  (if backward "T" "t")))
       (interactive)
       (let* ((c (or (easy-kill-get zap-char)
                     (read-char ,prompt t)))
              (beg (point))
              (pos (easy-kill-get zap-pos))
              (pos (cond ((natnump n)
                          (cond ((and pos
                                      (not (eq this-command 'easy-kill-digit-argument)))
                                 ;; if called consecutively, expand or shrink
                                 (if ,(if backward '(<= pos beg) '(<= beg pos))
                                     (save-excursion
                                       (goto-char pos)
                                       ,@(unless backward '((forward-char)))
                                       (,search-func (char-to-string c) nil nil n)
                                       (point))
                                   (,(intern (concat command-prefix (funcall format-name include (not backward)))) '-)
                                   nil))
                                (t
                                 (save-excursion
                                   ,@(unless backward '((forward-char)))
                                   (,search-func (char-to-string c) nil nil n)
                                   (point)))))
                         ((eq n '+)
                          (save-excursion
                            (goto-char pos)
                            (,search-func (char-to-string c))
                            (point)))
                         ((eq n '-)
                          (save-excursion
                            (goto-char (,(if backward '1+ '1-) pos))
                            (,search-func (char-to-string c) beg nil -1)
                            (,(if backward '1- '1+) (point)))))))
         (when pos
           (easy-kill-adjust-candidate ',name
                                       beg ,(if include 'pos
                                              (if backward '(1+ pos) '(1- pos))))
           (overlay-put easy-kill-candidate 'zap-char c)
           (overlay-put easy-kill-candidate 'zap-pos pos))))))

(easy-kill-defun-string-to-char t nil)
(easy-kill-defun-string-to-char t t)
(easy-kill-defun-string-to-char nil nil)
(easy-kill-defun-string-to-char nil t)

;;;###autoload (autoload 'easy-kill-on-string-to-char-forward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-to-char-backward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-forward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-backward "easy-kill-extras")

(defgroup easy-kill-extras nil
  "Extras for easy-kill."
  :group 'killing) ;; No 'easy-kill yet

(defcustom easy-kill-ace-jump-enable-p t
  "If non-nil, ace-jump commands can be used in easy-kill/easy-mark mode for selection."
  :type 'boolean
  :group 'easy-kill-extras)

(eval-after-load 'ace-jump-mode
  #'(progn
     (defvar easy-kill-ace-jump-last-command nil)
     (defvar easy-kill-ace-jump-this-command nil)
     (defvar easy-kill-ace-jump-original-pos nil)

     (defun easy-kill-ace-jump-save-state ()
       (if (and easy-kill-ace-jump-enable-p
                easy-kill-candidate
                (eq (easy-kill-get buffer)
                    (current-buffer)))
           (progn
             (easy-kill-abort)
             (setq easy-kill-ace-jump-this-command this-command
                   easy-kill-ace-jump-last-command last-command
                   easy-kill-ace-jump-original-pos (point)))
         (setq easy-kill-ace-jump-this-command nil
               easy-kill-ace-jump-last-command nil
               easy-kill-ace-jump-original-pos nil)))

     (defadvice ace-jump-char-mode (before easy-kill-extras activate)
       (easy-kill-ace-jump-save-state))
     (defadvice ace-jump-word-mode (before easy-kill-extras activate)
       (easy-kill-ace-jump-save-state))
     (defadvice ace-jump-line-mode (before easy-kill-extras activate)
       (easy-kill-ace-jump-save-state))

     (defadvice ace-jump-move (around easy-kill-extras activate)
       (let ((mode easy-kill-ace-jump-this-command)
             (command easy-kill-ace-jump-last-command)
             (orig easy-kill-ace-jump-original-pos))
         ad-do-it
         (if command
             (or
              (eq easy-kill-ace-jump-last-command command) ;; branch mode
              (let* ((pos (point))
                     (backward (> orig pos))
                     (line-mode (eq mode 'ace-jump-line-mode))
                     (beg orig)
                     (end (if line-mode pos
                            (if backward pos (1+ pos)))))
                (goto-char orig)
                (funcall command)
                (if line-mode
                    (let ((lines (count-lines beg end)))
                      (easy-kill-thing 'line (if backward (- 1 lines)
                                               (1- lines))))
                  (easy-kill-adjust-candidate
                   (if backward 'string-to-char-backward 'string-to-char-forward)
                   beg end)
                  (overlay-put easy-kill-candidate 'zap-char (char-after pos))
                  (overlay-put easy-kill-candidate 'zap-pos pos)))))))

     (defadvice ace-jump-done (after easy-kill-extras activate)
       (setq easy-kill-ace-jump-this-command nil
             easy-kill-ace-jump-last-command nil
             easy-kill-ace-jump-original-pos nil))))

(provide 'easy-kill-extras)
;;; easy-kill-extras.el ends here
