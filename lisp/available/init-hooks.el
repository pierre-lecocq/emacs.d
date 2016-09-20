;;; init-hooks.el --- Hooks

;; Time-stamp: <2016-09-15 13:32:14>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun hook-minibuffer-setup ()
  "Hook for Minibuffer setup."
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook #'hook-minibuffer-setup)

(defun hook-dired-mode ()
  "Hook for Dired mode."
  (put 'dired-find-alternate-file 'disabled nil))

(add-hook 'dired-mode-hook #'hook-dired-mode)

(defun hook-prog-mode ()
  "Hook for Prog mode."
  (idle-highlight-mode t)
  (rainbow-delimiters-mode)
  (rainbow-mode)
  ;; (git-gutter-mode)
  (set-face-underline 'font-lock-warning-face "red")
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'hook-prog-mode)

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(defun hook-php-mode ()
  "Hook for PHP mode."
  (when (string= (pl-clean-system-name) "lecocq-s")
    (pl-set-locale 'latin-1)) ;; don't ask
  (set (make-local-variable 'company-backends) '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end "")
  (set (make-local-variable 'indent-tabs-mode) nil))

(add-hook 'php-mode-hook #'hook-php-mode)

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

(provide 'init-hooks)

;;; init-hooks.el ends here
