;;; feat-php.el --- PHP feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-30 15:16:56>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package php-extras :ensure t)

(use-package php-mode :ensure t
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)
         ("\\.php-dev'" . php-mode)))

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

(provide 'feat-php)

;;; feat-php.el ends here
