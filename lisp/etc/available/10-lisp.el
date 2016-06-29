;;; 10-lisp.el --- Lisp

;; Time-stamp: <2016-06-29 11:04:50>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package slime-company :ensure t)

(use-package slime :ensure t
  :init (setq inferior-lisp-program (if (eq system-type 'darwin) "/usr/local/bin/sbcl" "sbcl"))
  :config (progn
            ;; Assuming (ql:quickload "quicklisp-slime-helper") has be ran in quicklisp before
            (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
              (if (file-exists-p helper-file)
                  (load helper-file)
                (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))
            (slime-setup '(slime-company))))

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;;; 10-lisp.el ends here
