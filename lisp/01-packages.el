;;; 01-packages.el --- Emacs Config - Packages

;;; Commentary:
;; Time-stamp: <2015-02-25 23:36:33 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

(unless (file-accessible-directory-p package-user-dir)
  (make-directory package-user-dir))

(require 'package)

(setq package-archives
      '(("melpa"       . "http://melpa.org/packages/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

;; Install packages

(use-package anzu
  :ensure anzu
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)))

(use-package autopair
  :ensure autopair
  :init (autopair-global-mode t))

(use-package bbdb
  :ensure bbdb)

(use-package company
  :ensure company
  :init (progn
          (add-hook 'after-init-hook 'global-company-mode)
          (setq company-auto-complete nil)
          (global-company-mode 1)
          (add-to-list 'company-backends 'company-dabbrev t)
          (add-to-list 'company-backends 'company-ispell t)
          (add-to-list 'company-backends 'company-files t)))

(use-package cycle-resize
  :ensure cycle-resize
  :init (progn
          (global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
          (global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)))

(use-package darkmine-theme
  :ensure darkmine-theme)

(use-package flycheck
  :ensure flycheck)

(use-package flx-ido
  :ensure flx-ido)

(use-package gnus-desktop-notify
  :ensure gnus-desktop-notify)

(use-package htmlize
  :ensure htmlize)

(use-package idle-highlight-mode
  :ensure idle-highlight-mode
  :init (progn
          (add-hook 'c-mode-hook (lambda () (idle-highlight-mode t)))
          (add-hook 'emacs-lisp-mode-hook (lambda () (idle-highlight-mode t)))
          (add-hook 'lisp-mode-hook (lambda () (idle-highlight-mode t)))
          (add-hook 'ruby-mode-hook (lambda () (idle-highlight-mode t)))
          (add-hook 'js2-mode-hook (lambda () (idle-highlight-mode t)))
          (add-hook 'php-mode-hook (lambda () (idle-highlight-mode t)))))

(use-package ido
  :init (progn
          (require 'ido)
          (ido-mode t)
          (ido-everywhere 1)
          (setq ido-enable-flex-matching t)
          (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))))

(use-package ido-hacks
  :ensure ido-hacks
  :init (ido-hacks-mode))

(use-package ido-vertical-mode
  :ensure ido-vertical-mode
  :init (ido-vertical-mode))

(use-package indent-guide
  :ensure indent-guide
  :init (indent-guide-global-mode))

(use-package js2-mode
  :ensure js2-mode)

(use-package markdown-mode
  :ensure markdown-mode)

(use-package php-extras
  :ensure php-extras)

(use-package php-mode
  :ensure php-mode
  :init (progn
          (add-hook 'php-mode-hook
                    (lambda ()
                      (require 'php-extras)
                      (setq comment-start "// ")
                      (setq comment-end "")
                      (set (make-local-variable 'indent-tabs-mode) nil)
                      (c-set-style "custom-four-indent")))))

(use-package projectile
  :ensure projectile
  :init (progn
          (projectile-global-mode)
          (setq projectile-enable-caching t)
          (setq projectile-file-exists-remote-cache-expire nil)))

(use-package rainbow-mode
  :ensure rainbow-mode
  :init (add-hook 'css-mode-hook (lambda () (rainbow-mode 1))))

(use-package recentf
  :init (progn
          (recentf-mode 1)
          (setq recentf-max-menu-items 50)))

(use-package robe
  :ensure robe
  :init (progn
          (add-hook 'ruby-mode-hook 'robe-mode)
          (push 'company-robe company-backends)))

(use-package ruby-mode
  :ensure ruby-mode
  :init (setq ruby-deep-indent-paren nil))

(use-package slime
  :ensure slime
  :init (setq inferior-lisp-program "sbcl"))

(use-package symon
  :ensure symon
  :init (symon-mode t))

(use-package switch-window
  :ensure switch-window)

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward uniquify-separator "/"))

(use-package visual-regexp
  :ensure visual-regexp)

(use-package web-mode
  :ensure web-mode)

(use-package yaml-mode
  :ensure yaml-mode)

;;; 01-packages.el ends here
