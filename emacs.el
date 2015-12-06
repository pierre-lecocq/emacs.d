
(setq debug-on-error t)

(setq config-dir "~/src/emacs.d/")

(setq gc-cons-threshold 100000000
      ;; Identity
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      ;; Formats
      frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      ;; Scratch and Splash
      initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string))
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
      host-file (expand-file-name (concat config-dir (format "host-%s.el" (downcase (car (split-string (system-name) "\\.")))))))

(mapc #'require '(autoinsert
                  bookmark
                  em-alias
                  linum
                  paren
                  recentf
                  time-stamp
                  whitespace))

(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode 1)))
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
        (when (fboundp mode)
          (funcall mode -1)))
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

(setq bookmark-default-file (expand-file-name (concat config-dir "bookmarks")))
(setq bookmark-sort-flag nil)
(setq bookmark-alist '(("Home" (filename . "~/"))
                       ("Emacs folder" (filename . "~/src/emacs.d"))
                       ;; ("Qsdfgh home" (filename . "/scp:pierre@qsdfgh.com#38170:~/"))
                       ("Fotolia dev" (filename . "/scp:eqx-dev1:/home/plecocq/www/fotolia"))))

(setq-default show-trailing-whitespace t
              highlight-tabs t
              mode-line-format
              (list
               '(:eval (if (buffer-modified-p)
                           (propertize "  %b" 'face 'bold-italic)
                         (propertize "  %b" 'face 'bold)))
               " (%l:%c) %p/%I - %m";; (format " %s" minor-mode-alist)
               '(which-function-mode (" " which-func-format))))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(when (display-graphic-p)
  (progn
    (toggle-frame-maximized)
    (setq show-paren-style 'expression
          select-enable-clipboard t))
  (set-fringe-mode 10))

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

(auto-insert)

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "# Description: " _ "\n\n")
        ((lisp-mode . "Lisp program") nil
         ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
        ((emacs-lisp-mode . "Emacs lisp program") nil
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

(add-to-list 'recentf-exclude "emacs\\.d/packages")
(add-to-list 'recentf-exclude "ido\\.last")

(require 'package)

(setq package-user-dir packages-dir)

(setq package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

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

(defun hook-minibuffer-setup ()
  "Hook for Minibuffer setup."
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook #'hook-minibuffer-setup)

(defun hook-mail-mode ()
  "Hook for Mail mode."
  (setq show-trailing-whitespace nil))

(add-hook 'mail-mode-hook #'hook-mail-mode)

(defun hook-shell-mode ()
  "Hook for Shell mode."
  (setq show-trailing-whitespace nil)
  (eshell/alias "l" "ls -l")
  (eshell/alias "la" "ls -la"))

(add-hook 'shell-mode-hook #'hook-shell-mode)
(add-hook 'eshell-mode-hook #'hook-shell-mode)

(defun hook-dired-mode ()
  "Hook for Dired mode."
  (put 'dired-find-alternate-file 'disabled nil))

(add-hook 'dired-mode-hook #'hook-dired-mode)

(defun hook-text-mode ()
  "Hook  for Text mode."
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d "))

(add-hook 'text-mode-hook #'hook-text-mode)

(defun hook-prog-mode ()
  "Hook for Prog mode."
  (idle-highlight-mode t)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t)
  (rainbow-delimiters-mode)
  (rainbow-mode))

(add-hook 'prog-mode-hook #'hook-prog-mode)

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(defun hook-php-mode ()
  "Hook for PHP mode."
;;  (require 'php-extras)
  (setq comment-start "// "
        comment-end "")
  (set (make-local-variable 'indent-tabs-mode) nil))

(add-hook 'php-mode-hook #'hook-php-mode)

(defun hook-emacs-lisp-mode ()
  "Hook for Emacs Lisp mode."
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

(defun hook-before-save ()
  "Hook before save."
  (time-stamp)
  (delete-trailing-whitespace)
  (whitespace-cleanup))

(add-hook 'before-save-hook #'hook-before-save)

(defun hook-after-save ()
  "Hook after save."
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (emacs-lisp-byte-compile)))

;; (add-hook 'after-save-hook #'hook-after-save)

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

(defun pl-join-lines ()
  "Join lines."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (kill-line)
    (just-one-space)))

(defun pl-join-lines-on-region (start end)
  "Join lines on region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (funcall #'pl-join-lines)
      (forward-line 1))))

(defun org-font-lock-ensure (beg end)
  "Org font lock ensure from BEG to END."
  (font-lock-ensure))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        select-enable-clipboard t))

(global-set-key [delete] 'delete-char)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "M-y") 'browse-kill-ring)
(global-set-key (kbd "M-o") 'occur)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-S-s") 'find-grep)
(global-set-key (kbd "C-S-f") 'imenu)

(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f12] 'pl-get-shell)

(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

;; Credits to emacsfodder

(define-key occur-mode-map (kbd "<down>")
  (lambda ()
    (interactive)
    (occur-next)
    (occur-mode-goto-occurrence-other-window)
    (recenter)
    (other-window 1)))

(define-key occur-mode-map (kbd "<up>")
  (lambda ()
    (interactive)
    (occur-prev)
    (occur-mode-goto-occurrence-other-window)
    (recenter)
    (other-window 1)))

(dolist (f (list host-file))
  (when (file-exists-p f)
    (load f 'noerror)))

(if (>= emacs-major-version 25)
  (message "Config successfully loaded in %s" (emacs-init-time)))
