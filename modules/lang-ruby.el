;;; lang-ruby.el --- Lang-Ruby -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 14:46:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package inf-ruby :ensure t :defer t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe :ensure t :defer t
  :after (company inf-ruby)
  :hook (ruby-mode . (lambda ()
                       (robe-mode 1)
                       (add-to-list 'company-backends 'company-robe)
                       ;; (when (executable-find "rvm")
                       ;;   (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
                       ;;     (rvm-activate-corresponding-ruby)))
                       )))

(use-package rubocop :ensure t :defer t
  :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools :ensure t :defer t
  :hook (ruby-mode . ruby-tools-mode))

(use-package ruby-mode :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(use-package yard-mode :ensure t
  :hook (ruby-mode . yard-mode))

(provide 'lang-ruby)

;;; lang-ruby.el ends here
