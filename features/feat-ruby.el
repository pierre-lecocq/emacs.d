;;; feat-ruby.el --- Ruby feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-30 16:11:02>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - Start inf with `M-x inf-ruby-console-auto`
;; - Start robe with `M-x robe-start`
;;
;; - Load a file in the REPL: `C-c C-l` (or, in a Rails project `C-c C-k`)

;;; Code:

(use-package inf-ruby :ensure t :defer t)

(use-package robe :ensure t :defer t
  :init (eval-after-load 'company
          '(push 'company-robe company-backends))
  :hook (ruby-mode . robe-mode))

(use-package rubocop :ensure t :defer t)

(use-package ruby-tools :ensure t :defer t)

(use-package ruby-mode :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(use-package yard-mode :ensure t)

(provide 'feat-ruby)

;;; feat-ruby.el ends here
