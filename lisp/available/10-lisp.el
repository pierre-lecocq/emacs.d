;;; 20-lisp.el --- Lisp

;; Time-stamp: <2016-06-16 16:34:07>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package slime-company :ensure t)

(use-package slime :ensure t
             :init (progn
                     ;; Assuming (ql:quickload "quicklisp-slime-helper") has be ran in quicklisp before
                     (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
                       (if (file-exists-p helper-file)
                           (load helper-file)
                         (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))

                     (if (eq system-type 'darwin)
                         (setq inferior-lisp-program "/usr/local/bin/sbcl")
                       (setq inferior-lisp-program "sbcl"))

                     (slime-setup '(slime-company))))

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;;; 20-lisp.el ends here
