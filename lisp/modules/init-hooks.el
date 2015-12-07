;;; init-hooks.el --- Emacs config - hooks

;; Time-stamp: <2015-12-07 11:51:15>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun hook-minibuffer-setup ()
  "Hook for Minibuffer setup."
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook #'hook-minibuffer-setup)

(defun hook-mail-mode ()
  "Hook for Mail mode."
  (setq show-trailing-whitespace nil))

(add-hook 'mail-mode-hook #'hook-mail-mode)

(defun hook-shell-mode ()
  "Hook for Shell mode."
  (setq show-trailing-whitespace nil)
  (eshell/alias "l" "ls -l")
  (eshell/alias "la" "ls -la"))

(add-hook 'shell-mode-hook #'hook-shell-mode)
(add-hook 'eshell-mode-hook #'hook-shell-mode)

(defun hook-dired-mode ()
  "Hook for Dired mode."
  (put 'dired-find-alternate-file 'disabled nil))

(add-hook 'dired-mode-hook #'hook-dired-mode)

(defun hook-text-mode ()
  "Hook  for Text mode."
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d "))

(add-hook 'text-mode-hook #'hook-text-mode)

(defun hook-prog-mode ()
  "Hook for Prog mode."
  (idle-highlight-mode t)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t)
  (rainbow-delimiters-mode)
  (rainbow-mode))

(add-hook 'prog-mode-hook #'hook-prog-mode)

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(defun hook-php-mode ()
  "Hook for PHP mode."
  ;;  (require 'php-extras)
  (setq comment-start "// "
        comment-end "")
  (set (make-local-variable 'indent-tabs-mode) nil))

(add-hook 'php-mode-hook #'hook-php-mode)

(defun hook-emacs-lisp-mode ()
  "Hook for Emacs Lisp mode."
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

(defun hook-before-save ()
  "Hook before save."
  (time-stamp)
  (delete-trailing-whitespace)
  (whitespace-cleanup))

(add-hook 'before-save-hook #'hook-before-save)

(defun hook-after-save ()
  "Hook after save."
  ;; Byte compile only my elisp files
  (when (and (string-match (expand-file-name "~/") buffer-file-name)
             (string= major-mode "emacs-lisp-mode"))
    (emacs-lisp-byte-compile)))

;; (add-hook 'after-save-hook #'hook-after-save)

(provide 'init-hooks)

;;; init-hooks.el ends here
