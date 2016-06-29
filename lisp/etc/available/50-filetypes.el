;;; 01-filetypes.el --- Filetypes

;; Time-stamp: <2016-02-29 00:00:50>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.log\\'"         . auto-revert-mode))
(add-to-list 'auto-mode-alist '("\\.js[on]\\'"      . js2-mode))
(add-to-list 'auto-mode-alist '("\\.asd\\'"         . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'"          . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'"        . lisp-mode))
(add-to-list 'auto-mode-alist '("/tmp/mutt.*\\'"    . mail-mode))
(add-to-list 'auto-mode-alist '("\\.php-dev\\'"     . php-mode))
(add-to-list 'auto-mode-alist '("Dockerfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile"       . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"           . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'"        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'"     . ruby-mode))
(add-to-list 'auto-mode-alist '(".bashrc"           . shell-script-mode))
(add-to-list 'auto-mode-alist '(".zshrc"            . shell-script-mode))
(add-to-list 'auto-mode-alist '(".gnus"             . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"         . web-mode))
(add-to-list 'auto-mode-alist '("\\.erubis\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'"       . yaml-mode))

;;; 01-filetypes.el ends here
