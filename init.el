;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Time-stamp: <2020-07-21 15:19:45>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(let ((dir (expand-file-name ".cache" user-emacs-directory)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

(require 'core-defaults)
(require 'core-keybindings)
(require 'core-packages)
(require 'core-visuals)
(require 'core-utils)
(require 'core-scratch)
(require 'core-search)
(require 'core-shell)

(require 'dev-completion)
(require 'dev-navigation)
(require 'dev-interface)
(require 'dev-git)
(require 'dev-projects)
(require 'dev-snippets)
(require 'dev-syntax)

(require 'lang-elisp)
(require 'lang-makefile)
(require 'lang-text)

;; (require 'lang-c)
(require 'lang-go)
(require 'lang-javascript)
;; (require 'lang-lisp)
(require 'lang-php)
(require 'lang-python)
(require 'lang-ruby)
(require 'lang-web)

(require 'core-menu)

;;; init.el ends here
