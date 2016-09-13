;;; 10-lisp.el --- Lisp

;; Time-stamp: <2016-09-13 23:31:55>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(defun hook-emacs-lisp-mode ()
  "Hook for Emacs Lisp mode."
  (eldoc-mode)
  (global-prettify-symbols-mode 1))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (global-prettify-symbols-mode 1))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(use-package eldoc-mode
  :diminish eldoc-mode)

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

;;; 10-lisp.el ends here
