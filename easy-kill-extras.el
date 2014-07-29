;;; easy-kill-extras.el --- Extra functions for easy-kill.

;; Copyright (c) 2014 Akinori MUSHA
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
;; Version: 0.9.0
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This package contains extra functions for easy-kill/easy-mark:
;;
;; * easy-mark-word
;; * easy-mark-sexp
;; * easy-kill-delete-region
;;
;; It also provides the following easy-kill/easy-mark targets:
;;
;; * `string-to-char-forward'
;; * `string-to-char-backward'
;; * `string-up-to-char-forward'
;; * `string-up-to-char-backward'
;;
;;   These work like vi's f/F/t/T commands, respectively.
;;
;; Suggested settings are as follows:
;;
;;   ;; Upgrade `mark-word' and `mark-sexp' with easy-mark
;;   ;; equivalents.
;;   (global-set-key (kbd "M-@") 'easy-mark-word)
;;   (global-set-key (kbd "C-M-@") 'easy-mark-sexp)
;;
;;   ;; `delete-region' is automatically remapped, but you'll want to
;;   ;; have these if you are in love with `delete-selection-mode'.
;;   (define-key easy-kill-base-map (kbd "C-d") 'easy-kill-delete-region)
;;   (define-key easy-kill-base-map (kbd "DEL") 'easy-kill-delete-region)
;;
;;   ;; Add the following tuples to `easy-kill-alist', preferrably by
;;   ;; using `customize-variable'.
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
         (cond ((eq this-command 'easy-mark-word)
                (if (bound-and-true-p subword-mode)
                    '(subword) '(word)))
               ((eq this-command 'easy-mark-sexp)
                '(sexp))
               (t
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
(put 'easy-kill-delete-region 'easy-kill-exit t)
;;;###autoload
(defun easy-kill-delete-region ()
  "Delete the easy-kill selection without modifying the kill ring."
  (interactive)
  (pcase (easy-kill-get bounds)
    (`(,beg . ,end) (delete-region beg end))))

;;;###autoload
(define-key easy-kill-base-map [remap delete-region] 'easy-kill-delete-region)

;;;###autoload
(defun easy-kill-on-buffer (n)
  "Provide an easy-kill target `buffer' which selects the whole buffer."
  (easy-kill-adjust-candidate 'buffer (point-min) (point-max)))

;;;###autoload
(defun easy-kill-on-buffer-after-point (n)
  "Provide an easy-kill target `buffer-after-point'.
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
  "Provide an easy-kill target `buffer-before-point'.
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

;;;###autoload
(defmacro easy-kill-defun-string-to-char (include backward)
  "Macro to define string-to-char functions."
  (let ((name (intern (format "string-%s-char-%s"
                              (if include "to" "up-to")
                              (if backward "backward" "forward"))))
        (prompt (format "%s character %s: "
                        (if include "To" "Up to")
                        (if backward "backward" "forward")))
        (search-func (if backward 'search-backward 'search-forward)))
    `(defun ,(intern (format "easy-kill-on-%s" name)) (n)
       ,(format "Provide an easy-kill-target `%s'." name)
       (interactive)
       (let* ((c (or (easy-kill-get zap-char)
                     (read-char ,prompt t)))
              (beg (point))
              (pos (cond ((natnump n)
                          (save-excursion
                            ;; expand if called consecutively
                            (and (setq pos (easy-kill-get zap-pos))
                                 (not (eq this-command 'easy-kill-digit-argument))
                                 (goto-char pos))
                            (,search-func (char-to-string c) nil nil n)
                            (point)))
                         ((eq n '+)
                          (save-excursion
                            (goto-char (easy-kill-get zap-pos))
                            (,search-func (char-to-string c))
                            (point)))
                         ((eq n '-)
                          (save-excursion
                            (goto-char (,(if backward '1+ '1-) (easy-kill-get zap-pos)))
                            (,search-func (char-to-string c) beg nil -1)
                            (,(if backward '1- '1+) (point)))))))
         (easy-kill-adjust-candidate ',name
                                     beg ,(if include 'pos
                                            (if backward '(1+ pos) '(1- pos))))
         (overlay-put easy-kill-candidate 'zap-char c)
         (overlay-put easy-kill-candidate 'zap-pos pos)))))

;;;###autoload (autoload 'easy-kill-on-string-to-char-forward "easy-kill-extras")
(easy-kill-defun-string-to-char t nil)
;;;###autoload (autoload 'easy-kill-on-string-to-char-backward "easy-kill-extras")
(easy-kill-defun-string-to-char t t)
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-forward "easy-kill-extras")
(easy-kill-defun-string-to-char nil nil)
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-backward "easy-kill-extras")
(easy-kill-defun-string-to-char nil t)

;;; easy-kill-extras.el ends here
