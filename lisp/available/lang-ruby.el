;;; lang-ruby.el --- Ruby

;; Time-stamp: <2016-12-11 18:13:11>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package ruby-mode :ensure t)

(use-package inf-ruby :ensure t)

(use-package robe :ensure t
  :config (push 'company-robe company-backends))

(use-package rubocop :ensure t)

(add-hook 'ruby-mode-hook #'rubocop-mode)

(provide 'lang-ruby)

;;; lang-ruby.el ends here
