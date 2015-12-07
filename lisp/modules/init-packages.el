;;; init-packages.el --- Emacs config - packages

;; Time-stamp: <2015-12-08 00:00:35>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Package manager settings
(require 'package)

(setq package-user-dir config-dir-packages
      package-enable-at-startup nil
      package-archives '(("melpa"        . "http://melpa.org/packages/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Packages settings
(use-package anzu :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package autopair :ensure t
  :init (autopair-global-mode t))

(use-package bonjourmadame :ensure t)

(use-package browse-kill-ring :ensure t)

(use-package htmlize :ensure t)

(use-package idle-highlight-mode :ensure t)

(use-package js2-mode :ensure t)

(use-package markdown-mode :ensure t)

(use-package php-mode :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package rainbow-mode :ensure t)

(use-package ruby-mode :ensure t)

(use-package symon :ensure t
  :init (progn
          (setq symon-delay 5)
          (symon-mode t)))

(use-package web-mode :ensure t)

(use-package yaml-mode :ensure t)

(provide 'init-packages)

;;; init-packages.el ends here
