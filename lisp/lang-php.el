;;; lang-php.el --- PHP language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:20:37>
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

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

;;; lang-php.el ends here
