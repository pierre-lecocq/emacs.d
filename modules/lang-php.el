;;; lang-php.el --- PHP language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-09-03 15:55:14>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - composer global require "squizlabs/php_codesniffer=*"
;; - ln -s ~/.composer/vendor/bin/phpcs ~/bin/phpcs
;; - ln -s ~/.composer/vendor/bin/phpcbf ~/bin/phpcbf

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
                           '((php-extras-company company-dabbrev-code) company-capf company-files)))))

(provide 'lang-php)

;;; lang-php.el ends here
