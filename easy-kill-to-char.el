;;; easy-kill-to-char.el --- vi-like to-char/up-to-char selectors for easy-kill.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 4 Mar 2015
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;; Copyright (c) 2014-2023 Akinori MUSHA
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

;;; Commentary:
;;
;; This tweak adds vi-like to-char/up-to-char selectors to easy-kill.
;;
;; This library is part of the easy-kill-extras package and not meant
;; to be used standalone.

;;; Code:

(require 'easy-kill)

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
       (let* (case-fold-search
              (c (or (easy-kill-get zap-char)
                     (read-char ,prompt t)))
              (beg (point))
              (pos (easy-kill-get zap-pos))
              (pos (cond ((natnump n)
                          (cond ((and pos
                                      (not (eq this-command 'easy-kill-digit-argument)))
                                 ;; if called consecutively
                                 (if (,(if backward '>= '<=) beg pos)
                                     ;; expand
                                     (save-excursion
                                       (goto-char pos)
                                       (and (,search-func (char-to-string c) nil t n)
                                            (point)))
                                   ;; shrink
                                   (unless (ignore-errors (,(intern (concat command-prefix (funcall format-name include (not backward)))) '-))
                                     ;; or cross over to the other side
                                      (overlay-put easy-kill-candidate 'zap-pos beg)
                                      (unless (ignore-errors (,(intern (concat command-prefix name)) 1))
                                        (overlay-put easy-kill-candidate 'zap-pos pos)))
                                   nil))
                                (t
                                 ;; first time or with a digit argument
                                 (save-excursion
                                   ;; find the next occurrence excluding the character at point
                                   ,@(unless backward '((forward-char 1)))
                                   ;; signal an error if not found to avoid fallback
                                   (,search-func (char-to-string c) nil nil n)
                                   (point)))))
                         ((eq n '+)
                          (save-excursion
                            (goto-char pos)
                            (and (,search-func (char-to-string c) nil t)
                                 (point))))
                         ((eq n '-)
                          (save-excursion
                            (goto-char (,(if backward '1+ '1-) pos))
                            (and (,search-func (char-to-string c) ,(if backward 'beg '(1+ beg)) t -1)
                                 (,(if backward '1- '1+) (point))))))))
         (when pos
           (easy-kill-adjust-candidate ',(intern name)
                                       beg ,(if include 'pos `(,(if backward '1+ '1-) pos)))
           (overlay-put easy-kill-candidate 'zap-char c)
           (overlay-put easy-kill-candidate 'zap-pos pos)
           t)))))

(easy-kill-defun-string-to-char t nil)
(easy-kill-defun-string-to-char t t)
(easy-kill-defun-string-to-char nil nil)
(easy-kill-defun-string-to-char nil t)

;;;###autoload (autoload 'easy-kill-on-string-to-char-forward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-to-char-backward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-forward "easy-kill-extras")
;;;###autoload (autoload 'easy-kill-on-string-up-to-char-backward "easy-kill-extras")

(provide 'easy-kill-to-char)
;;; easy-kill-to-char.el ends here
