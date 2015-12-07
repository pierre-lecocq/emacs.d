;;; init-packages.el --- Emacs config - packages

;; Time-stamp: <2015-12-07 11:31:45>
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
(use-package anzu
  :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package autopair
  :ensure t
  :init (autopair-global-mode t))

(use-package bonjourmadame
  :ensure t)

(use-package browse-kill-ring
  :ensure t)

(use-package company
  :ensure t
  :init (progn
          (setq company-auto-complete nil
                company-tooltip-flip-when-above t
                company-minimum-prefix-length 2
                company-tooltip-limit 10
                company-idle-delay 0.5)
          (global-company-mode 1)))

(use-package darkmine-theme
  :ensure t
  :init (load-theme 'darkmine t))

(use-package htmlize
  :ensure t)

(use-package flx-ido
  :ensure t)

(use-package ido-hacks
  :ensure t)

(use-package ido-vertical-mode
  :ensure t)

(use-package ido
  :ensure t
  :init (progn
          (require 'ido)
          (require 'ido-hacks)
          (setq ido-save-directory-list-file (concat config-dir-files "ido.last")
                ido-case-fold t
                ido-enable-flex-matching t
                ido-use-filename-at-point 'guess
                ido-create-new-buffer 'always
                ido-use-virtual-buffers t)
          (ido-everywhere 1)
          (flx-ido-mode 1)
          (ido-mode t)
          (ido-hacks-mode)
          (ido-vertical-mode)))

(use-package idle-highlight-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t
  :init (progn
          ;; (setq org-directory (expand-file-name "~/org-files/")
          ;;       org-default-notes-file (expand-file-name (concat org-directory "notes.org"))
          ;;       org-agenda-files (expand-file-name (concat org-directory "agenda.org")))
          (setq org-hide-leading-stars t
                org-hide-emphasis-markers t
                org-fontify-done-headline t
                org-src-fontify-natively t)))

(use-package php-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package ruby-mode
  :ensure t)

(use-package slime-company
  :ensure t)

(use-package slime
  :ensure t
  :init (progn

          (if (eq system-type 'darwin)
              (setq inferior-lisp-program "/usr/local/bin/sbcl")
            (setq inferior-lisp-program "sbcl"))
          (slime-setup '(slime-company))))

(use-package symon
  :ensure t
  :init (progn
          (setq symon-delay 5)
          (symon-mode t)))

(use-package web-mode
  :ensure t)

(use-package whitespace
  :ensure t
  :init (progn
          (setq whitespace-line-column 80
                whitespace-style '(tabs tab-mark face)
                whitespace-global-modes '(not org-mode web-mode))
          (global-whitespace-mode)))

(use-package yaml-mode
  :ensure t)

(provide 'init-packages)

;;; init-packages.el ends here
