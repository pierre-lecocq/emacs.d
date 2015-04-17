;;; emacs.el --- Emacs config

;; Time-stamp:  <2015-04-17 19:35:25>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;; This is yet another step to the biggest lie in the world:
;;  a perfect Emacs configuration file.

;;; Code:

;;;; core - Yet Another Konfig-helper

(defvar yak/base-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

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

(defun yak/mkpath (&rest args)
  "Build a path and eventually create it on file system."
  (unless (boundp 'yak/base-dir)
    (setq yak/base-dir user-emacs-directory))
  (let* ((name (plist-get args :name))
         (base (or (plist-get args :base) yak/base-dir))
         (directory (plist-get args :directory))
         (create (plist-get args :create))
         (path (expand-file-name (concat (file-name-as-directory base) name))))
    (when create
      (if directory
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

(defun pl/kill-buffers-by-mode (&optional mode-name)
  "Kill buffers by mode"
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

;;;; display

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when window-system (set-fringe-mode '(1 . 1)))

;; Set Inconsolata font but falls back to DejaVu when unicode chars fail
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 120
                      :weight 'normal
                      :width 'normal))
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12
                               :weight 'normal)))

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
 frame-title-format "Emacs %f"
 time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
 auto-insert-copyright (user-full-name)
 initial-scratch-message ";; Scratch buffer\n\n"
 inhibit-startup-message t
 inhibit-splash-screen t
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 kill-whole-line t
 require-final-newline t
 next-line-add-newlines nil
 show-paren-style 'expression
 recentf-max-menu-items 50
 uniquify-buffer-name-style 'forward uniquify-separator "/"
 bookmark-default-file (yak/mkpath :name "bookmarks")
 package-user-dir (yak/mkpath :name "vendor/packages" :directory t :create t)
 org-directory (yak/mkpath :name "org-files" :directory t :create t :base "~/")
 custom-file (yak/mkpath :name "custom.el")
 machine-file (yak/mkpath :name (format "%s.el" (downcase (car (split-string system-name "\\."))))))

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

(defun hook-minibuffer-setup ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook 'hook-minibuffer-setup)

(defun hook-text-mode ()
  (global-visual-line-mode 1)
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d "))

(add-hook 'text-mode-hook 'hook-text-mode)

(defun hook-prog-mode ()
  (idle-highlight-mode t)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t))

(add-hook 'prog-mode-hook 'hook-prog-mode)

(defun hook-c-mode-common ()
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'hookc-mode-common)

(defun hook-ruby-mode ()
  (global-set-key (kbd "C-c C-r") 'pl/rb-require))

(add-hook 'ruby-mode-hook 'hook-ruby-mode)

(defun hook-php-mode ()
  (require 'php-extras)
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'indent-tabs-mode) nil))

(add-hook 'php-mode-hook 'hook-php-mode)

(defun hook-emacs-lisp-mode ()
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'hook-emacs-lisp-mode)

(defun hook-compilation-filter ()
  (require 'ansi-color)
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'hook-compilation-filter)

(defun hook-before-save ()
  (time-stamp)
  (delete-trailing-whitespace)
  (whitespace-cleanup))

(add-hook 'before-save-hook 'hook-before-save)
(add-hook 'kill-buffer-hook 'hook-before-save)

(defun hook-find-file ()
  (auto-insert)
  (if (string= major-mode "php-mode")
      (pl/set-locale 'latin-1) ;; Fuck you, PHP. Just Fuck you.
    (pl/set-locale 'utf-8))
  (if (and buffer-file-name
           (string-match "/gnulib\\>" (buffer-file-name))
           (not (string-equal mode-name "Change Log"))
           (not (string-equal mode-name "Makefile")))
      (setq indent-tabs-mode nil)))

(add-hook 'find-file-hook 'hook-find-file)


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
 org-default-notes-file (yak/mkpath :name "notes.org" :create t :base org-directory)
 org-agenda-files (list
                   ;; Add other files here byt duplicating the below line.
                   (yak/mkpath :name "agenda.org" :create t :base org-directory)))

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

(global-set-key (kbd "C-S-x k") 'pl/kill-buffers-by-mode)

(global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
(global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)

(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f12] 'pl/get-shell)

;;;; extrafiles

(load custom-file 'noerror)
(load machine-file 'noerror)

;;; emacs.el ends here
