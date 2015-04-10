;;; emacs.el --- Emacs config

;; Time-stamp:  <2015-04-10 17:14:43 lecocq>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;; This is yet another step to the biggest lie in the world:
;;  a perfect Emacs configuration file.

;;; Code:

(defvar yak/base-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

;;;; core - Yet Another Konfig-helper

(defun yak/pkg-initialize (name)
  "Initialize and refresh the package manager if needed."
  (unless (boundp 'yak/pkg-initialized)
    (require 'package)
    (setq package-archives
          '(("melpa"        . "http://melpa.org/packages/")
            ("gnu"          . "http://elpa.gnu.org/packages/")
            ("marmalade"    . "http://marmalade-repo.org/packages/")))
    (package-initialize)
    (setq yak/pkg-initialized t))
  (unless (or (package-built-in-p name)
              (package-installed-p name)
              (boundp 'yak/pkg-refreshed))
    (package-refresh-contents)
    (setq yak/pkg-refreshed t)))

(defun yak/pkg-install (name)
  "Install a package."
  (yak/pkg-initialize name)
  (unless (or (package-built-in-p name)
              (package-installed-p name))
    (package-install name)))

(defmacro yak/pkg (name &rest body)
  "Install and configure and package."
  `(progn
     (yak/pkg-install ,name)
     (eval-after-load ,name
       (progn ,@body))))

(defun yak/mkpath (path &optional is-directory create forced-base-dir)
  "Make PATH and eventually CREATE it on file system."
  (unless (boundp 'yak/base-dir) (setq yak/base-dir user-emacs-directory))
  (let* ((l-base-dir (if forced-base-dir forced-base-dir yak/base-dir))
         (path (expand-file-name (concat (file-name-as-directory l-base-dir) path))))
    (when create
      (if is-directory
          (unless (file-accessible-directory-p path)
            (make-directory path t))
        (unless (file-exists-p path)
          (write-region "" nil path))))
    path))

;;;; functions

(defun pl/set-locale (locale)
  "Set locale."
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (setq locale-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(defun pl/set-indentation ()
  "Set indentation."
  (unless (string= major-mode "GNUmakefile")
    (setq-default
     tab-width 4
     c-basic-offset 4
     c-hanging-comment-ender-p nil
     indent-tabs-mode nil)))

(pl/set-indentation)

(defun pl/get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*shell*") (buffer-list))
          (switch-to-buffer "*shell*")
        (shell)))))

(defun pl/transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when window-system
    (set-frame-parameter (selected-frame) 'alpha value)))

(defun pl/rb-require ()
  "Insert required rubygems."
  (interactive "*")
  (let ((gems (read-from-minibuffer "Rubygems to require: ")))
    (when gems
      (mapcar (lambda (gem)
                (insert (format "require \"%s\"\n" gem)))
              (split-string gems nil t)))))

;;;; display

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when window-system (set-fringe-mode '(1 . 1)))
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))

;;;; internals

(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(show-paren-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)
(line-number-mode t)
(column-number-mode t)
(global-auto-revert-mode 1)
(which-function-mode)

(setq
 user-full-name "Pierre Lecocq"
 user-mail-address "pierre.lecocq@gmail.com"
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 inhibit-startup-message t
 inhibit-splash-screen t
 initial-scratch-message ";; Scratch buffer\n\n"
 kill-whole-line t
 require-final-newline t
 next-line-add-newlines nil
 show-paren-style 'expression
 recentf-max-menu-items 50
 uniquify-buffer-name-style 'forward uniquify-separator "/"
 frame-title-format "Emacs %f"
 auto-insert-copyright (user-full-name)
 bookmark-default-file (yak/mkpath "bookmarks")
 package-user-dir (yak/mkpath "vendor/packages" t t)
 org-directory (yak/mkpath "org-files" t t "~/")
 custom-file (yak/mkpath "custom.el")
 machine-file (yak/mkpath (format "%s.el" (downcase (car (split-string system-name "\\."))))))

(setq-default
 show-trailing-whitespace t
 highlight-tabs t
 mode-line-format
 (list
  '(:eval (if (buffer-modified-p)
              (propertize "  %b" 'face 'bold-italic)
            (propertize "  %b" 'face 'bold)))
  " (%l:%c) %p/%I - %m";; (format " %s" minor-mode-alist)
  '(which-function-mode (" " which-func-format))))

;;;; packages

(yak/pkg 'anzu
         (global-anzu-mode +1)
         (set-face-attribute 'anzu-mode-line nil :foreground "yellow"))

(yak/pkg 'autopair
         (autopair-global-mode t))

(yak/pkg 'company
         (setq company-auto-complete nil)
         (global-company-mode 1))

(yak/pkg 'cycle-resize)

(yak/pkg 'darkmine-theme
         (load-theme 'darkmine t))

(yak/pkg 'flx-ido)

(yak/pkg 'htmlize)

(yak/pkg 'idle-highlight-mode)

(yak/pkg 'ido-hacks)
(yak/pkg 'ido-vertical-mode)
(yak/pkg 'ido
         (require 'ido)
         (ido-mode t)
         (ido-everywhere 1)
         (flx-ido-mode 1)
         (setq ido-enable-flex-matching t)
         (setq ido-use-faces nil)
         (require 'ido-hacks)
         (ido-hacks-mode)
         (ido-vertical-mode))

(yak/pkg 'js2-mode)
(yak/pkg 'markdown-mode)
(yak/pkg 'php-extras)
(yak/pkg 'php-mode)
(yak/pkg 'rainbow-mode)
(yak/pkg 'ruby-mode)

(yak/pkg 'symon
         (setq symon-delay 5)
         (symon-mode t))

(yak/pkg 'visual-regexp)
(yak/pkg 'web-mode)

(yak/pkg 'whitespace
         (require 'whitespace)
         (setq whitespace-line-column 80)
         ;; (setq whitespace-style '(tabs tab-mark face lines-tail))
         (setq whitespace-style '(tabs tab-mark face))
         (setq whitespace-global-modes '(not org-mode web-mode))
         (global-whitespace-mode))

(yak/pkg 'yaml-mode)

;;;; hooks

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (setq show-trailing-whitespace nil)))

(add-hook 'text-mode-hook
          '(lambda ()
             (global-visual-line-mode 1)
             (linum-mode 1)
             (make-local-variable 'linum-format)
             (setq linum-format " %d ")))

(add-hook 'prog-mode-hook
          '(lambda ()
             (idle-highlight-mode t)
             (local-set-key (kbd "C-c <right>") 'hs-show-block)
             (local-set-key (kbd "C-c <left>")  'hs-hide-block)
             (local-set-key (kbd "C-c <up>")    'hs-hide-all)
             (local-set-key (kbd "C-c <down>")  'hs-show-all)
             (hs-minor-mode t)))

(add-hook 'c-mode-common-hook
          '(lambda()
             (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'ruby-mode-hook
          '(lambda()
             (global-set-key (kbd "C-c C-r") 'pl/rb-require)))

(add-hook 'php-mode-hook
          '(lambda ()
             (require 'php-extras)
             (setq comment-start "// ")
             (setq comment-end "")
             (set (make-local-variable 'indent-tabs-mode) nil)))

(add-hook 'before-save-hook
          '(lambda()
             (time-stamp)
             (delete-trailing-whitespace)
             (whitespace-cleanup)))

(add-hook 'find-file-hook
          '(lambda ()
             (auto-insert)
             (if (string= major-mode "php-mode")
                 (pl/set-locale 'latin-1) ;; Fuck you, PHP. Just Fuck you.
               (pl/set-locale 'utf-8))
             (if (and buffer-file-name
                      (string-match "/gnulib\\>" (buffer-file-name))
                      (not (string-equal mode-name "Change Log"))
                      (not (string-equal mode-name "Makefile")))
                 (setq indent-tabs-mode nil))))

;;;; files

(add-to-list 'auto-mode-alist '("\\.log\\'"         . auto-revert-mode))
(add-to-list 'auto-mode-alist '("\\.js[on]\\'"      . js2-mode))
(add-to-list 'auto-mode-alist '("Dockerfile"        . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile"       . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"           . ruby-mode))
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

;;;; autoinsert

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         "# Description: " _ "\n\n")
        ((emacs-lisp-mode . "Emacs lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n\n"
         ";;; Commentary:\n\n"
         ";;; Code:\n\n"
         ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
        ((c-mode . "C program") nil
         "/*\n"
         " * File: " (file-name-nondirectory buffer-file-name) "\n"
         " * Time-stamp: <>\n"
         " * Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         " * Description: " _ "\n"
         " */\n\n")
        ((shell-mode . "Shell script") nil
         "#!/bin/bash\n\n"
         " # File: " (file-name-nondirectory buffer-file-name) "\n"
         " # Time-stamp: <>\n"
         " # Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         " # Description: " _ "\n\n")))

;;;; org-mode

(setq
 org-hide-leading-stars t
 org-hide-emphasis-markers t
 org-fontify-done-headline t
 org-src-fontify-natively t
 org-default-notes-file (yak/mkpath "notes.org" nil t org-directory)
 org-agenda-files (list
                   ;; Add other files here byt duplicating the below line.
                   (yak/mkpath "agenda.org" nil t org-directory)))

(defun org-font-lock-ensure ()
  "Org font lock ensure."
  (font-lock-ensure))

;;;; keybindings

(when (eq system-type 'darwin)
  (setq
   mac-option-modifier nil
   mac-command-modifier 'meta
   select-enable-clipboard t))

(global-set-key [delete] 'delete-char)

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

(global-set-key (kbd "M-j") (join-line -1))

(global-set-key (kbd "C-S-s") 'find-grep)
(global-set-key (kbd "C-S-f") 'imenu)

(global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
(global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)

(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f12] 'pl/get-shell)

;;;; extrafiles

(load custom-file 'noerror)
(load machine-file 'noerror)

;;; emacs.el ends here
