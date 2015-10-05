
(setq debug-on-error t)

(setq config-dir "~/work/src/emacs.d/")

(setq gc-cons-threshold 100000000
      ;; Identity
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      ;; Formats
      frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      ;; Scratch and Splash
      initial-scratch-message (format ";; Scratch buffer - started at %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      ;; General behaviour
      kill-whole-line t
      require-final-newline t
      next-line-add-newlines nil
      ;; Backup
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      ;; VC
      vc-follow-symlinks t
      ;; Recentf
      recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      recentf-keep '(file-remote-p file-readable-p)
      ;; Password cache
      password-cache-expiry nil
      ;; Uniquify
      uniquify-buffer-name-style 'forward uniquify-separator "/"
      ;; Paths
      packages-dir (expand-file-name (concat config-dir "packages"))
      custom-file (expand-file-name (concat config-dir "custom.el"))
      bookmark-default-file (expand-file-name (concat config-dir "bookmarks"))
      org-directory (expand-file-name "~/org-files")
      host-file (expand-file-name (concat config-dir (format "host-%s.el" (downcase (car (split-string (system-name) "\\.")))))))

(mapc #'require '(autoinsert
                  bookmark
                  linum
                  org
                  paren
                  recentf
                  time-stamp
                  whitespace))

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode 1)))
      '(auto-compression-mode
        auto-insert-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        global-hl-line-mode
        line-number-mode
        recentf-mode
        show-paren-mode
        transient-mark-mode
        which-function-mode))

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(menu-bar-mode
        tool-bar-mode
        scroll-bar-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              indent-tabs-mode nil)

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(auto-insert)

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((emacs-lisp-mode . "Emacs lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         " * Description: " _ "\n"
         " */\n\n")
        ((shell-mode . "Shell script") nil
         "#!/bin/bash\n\n"
         " # File: " (file-name-nondirectory buffer-file-name) "\n"
         " # Time-stamp: <>\n"
         " # Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         " # Description: " _ "\n\n")))

(require 'package)

(setq package-user-dir packages-dir)
(setq package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

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
                     (setq ido-case-fold t
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

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*eshell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*eshell*") (buffer-list))
          (switch-to-buffer "*eshell*")
        (eshell)))))

(defun pl-kill-buffers-by-mode (&optional mode-name)
  "Kill buffers by mode.  Ask which mode if MODE-NAME is not provided."
  (interactive)
  (unless mode-name
    (setq mode-name (read-from-minibuffer "Mode to kill: ")))
  (let ((killed-buffers 0)
        (mode-to-kill (intern mode-name)))
    (dolist (buffer (buffer-list))
      (when (eq mode-to-kill (buffer-local-value 'major-mode buffer))
        (setq killed-buffers (1+ killed-buffers))
        (kill-buffer buffer)))
    (message "%d buffer(s) killed" killed-buffers)))

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when (display-graphic-p)
    (set-frame-parameter (selected-frame) 'alpha value)))
