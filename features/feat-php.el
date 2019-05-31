;;; feat-php.el --- PHP feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-31 14:15:31>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - composer global require "squizlabs/php_codesniffer=*"
;; - ln -s ~/.composer/vendor/bin/phpcs ~/bin/phpcs
;; - ln -s ~/.composer/vendor/bin/phpcbf ~/bin/phpcbf

;;; Code:

(use-package php-extras :ensure t)

(use-package php-mode :ensure t
  :init (progn
          (setq comment-start "// "
                comment-end "")
          (set (make-local-variable 'company-backends)
               '((php-extras-company company-dabbrev-code) company-capf company-files)))
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode))
  :hook (php-mode . php-enable-default-coding-style))

(provide 'feat-php)

;;; feat-php.el ends here
