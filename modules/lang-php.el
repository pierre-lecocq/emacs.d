;;; lang-php.el --- Lang-Php -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 14:42:53>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package php-extras :ensure t)

(use-package php-mode :ensure t
  :init (setq comment-start "// "
              comment-end "")
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode))
  :hook (php-mode . (lambda ()
                      (php-enable-default-coding-style)
                      (set (make-local-variable 'company-backends)
                           '((php-extras-company company-dabbrev-code)
                             company-capf company-files)))))

(provide 'lang-php)

;;; lang-php.el ends here
