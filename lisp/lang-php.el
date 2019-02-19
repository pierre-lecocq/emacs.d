;;; lang-php.el --- PHP language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-02-14 11:06:13>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Auto insert

(add-to-list 'auto-insert-alist
             '((php-mode . "PHP script") nil
               "<?php\n\n"))

(use-package php-extras :ensure t :defer t)

(use-package php-mode :ensure t
  :after (:all php-extras)
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)))

(defvar php-auto-lint-timer nil)

(defun php-auto-lint ()
  "Run PHP autolint."
  (interactive)
  (message (shell-command-to-string (concat "php -l " buffer-file-name))))

(defun enable-php-auto-lint ()
  "Enable PHP autolint."
  (unless php-auto-lint-timer
    (setq php-auto-lint-timer (run-with-idle-timer 2 t #'php-auto-lint))))

(defun hook-php-mode ()
  "Hook for PHP mode."
  ;; (enable-php-auto-lint)
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

;;; lang-php.el ends here
