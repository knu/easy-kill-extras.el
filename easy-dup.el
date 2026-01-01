;;; easy-dup.el --- duplicate functions for easy-kill.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 1 Jan 2026
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;; Copyright (c) 2026 Akinori MUSHA
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
;; This library adds easy-dup functions to easy-kill.
;;
;; This library is part of the easy-kill-extras package and not meant
;; to be used standalone.

;;; Code:

(require 'easy-kill)

(defcustom easy-dup-try-things '(line)
  "A list of things for `easy-dup-before' and `easy-dup-after' to try."
  :type '(repeat symbol)
  :group 'killing)

(declare-function rectangle--duplicate-right "rect" (n displacement))

;;;###autoload
(defun easy-dup (&optional n before)
  "Insert a copy of the current selection after it, or before it if BEFORE.
When not in easy-kill/easy-mark, use the active region if available, or
enter easy-mark using `easy-dup-try-things' to select something to
duplicate.  `rectangle-mark-mode' is also supported.  N specifies the
number of copies to insert."
  (interactive "*p")
  (or
   (pcase (if easy-kill-candidate (easy-kill-get bounds) '(nil . nil))
     (`(,x . ,x)
      (ignore x)
      (cond
       ((bound-and-true-p rectangle-mark-mode)
        (rectangle--duplicate-right n (if before n 0))
        t)
       ((use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (text (buffer-substring beg end)))
          (save-excursion
            (goto-char (if before beg end))
            (if before
                (dotimes (_ (or n 1)) (insert-before-markers text))
              (dotimes (_ (or n 1)) (insert text)))))
        t)
       (t (let ((easy-mark-try-things easy-dup-try-things))
            (easy-mark 1)
            nil)))))
   (pcase (easy-kill-get bounds)
     (`(,x . ,x) (ignore x) (easy-kill-echo "Empty region"))
     (`(,beg . ,end)
      (let ((text (buffer-substring beg end)))
        (save-excursion
          (goto-char (if before beg end))
          (if before
              (dotimes (_ (or n 1)) (insert-before-markers text))
            (dotimes (_ (or n 1)) (insert text))))
        (and before
             (setf (easy-kill-get origin) (easy-kill-get start)))))))
  (setq deactivate-mark nil))

;;;###autoload
(defalias 'easy-dup-after #'easy-dup)

;;;###autoload
(defun easy-dup-before (&optional n)
  "Insert a copy of the current selection before it.
When not in easy-kill/easy-mark, use the active region if available, or
enter easy-mark using `easy-dup-try-things' to select something to
duplicate.  `rectangle-mark-mode' is also supported.  N specifies the
number of copies to insert."
  (interactive "*p")
  (easy-dup n t))

(provide 'easy-dup)
;;; easy-dup.el ends here
