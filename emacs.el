;;; emacs.el --- Emacs Config - Main file

;; Time-stamp: <2015-03-19 09:09:30 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;
;; * Introduction
;;
;; This file is another step into the world of emacs configuration holy grail:
;;
;; - Step 1: Use emacs like rms just the time to realize that emacs is amazing.
;; - Step 2: Be amazed by lots of things you do not even understand.
;; - Step 3: Copy & paste from everywhere in a fucking messy config file.
;; - Step 4: Split your config into many files but don't figure out which part goes where.
;; - Step 5: Begin to write your own 5-lines lisp functions that are, in fact, already implemented.
;; - Step 6: Try to organize the whole mess and learn some real emacs lisp basics.
;; - Step 7: Be sick & tired of all the code you don't understand and don't use even 10% of the time.
;; - Step 8: Be honest and realize that you don't know more than 60% of elisp's power.
;; - Step 9: Empty your config files and try to make a single file one to figure out what's going on.
;; - Step 10: Clean the code you stole elsewhere but don't use.
;; - Step 11: Rewrite you config by keeping the things you understand.
;; - Step 12: Be honest and realize that you don't know more than 40% of elisp's power.
;; - Step 13: Read more blogs, youtube chats or reddit posts.
;; - Step 14: Be honest and realize that you don't know more than 30% of elisp's power.
;; - Step 15: Be honest and realize that you don't know more than 25% of elisp's power.
;; - Step 16: Stick with your config but think about making it better.
;; - Step 17: Buy books about lisp or emacs lisp.
;; - Step 18: Be honest and realize that you don't know more than 20% of elisp's power.
;; - Step 19: Discover package managers and macros like use-package.
;; - Step 20: Feel cool with a clean and mastered config files.
;; - Step 21: Try to make it cleaner and smaller.
;; - Step 22: Try to figure out what's going on behind package managers. Be mind fucked.
;; - Step 23: Be honest and realize that you don't know more than 15% of elisp's power.
;; - Step 24: Finally, be confident and realize that you know more than 15% of elisp's power.
;; - Step 25: Write your own functions and macros to manage your packages, initializations and keybindings.
;; - Step 26: Realize that you understand any line of your configuration, but it is only "configuration", not programs.
;; - Step 27: Be confident and realize that you MIGHT know more than 40% of elisp's power if you read more documentation.
;; - Step 28: write steps you crossed to finally be here, but know that the path is still long. But funny. But long ...
;;
;; * Purpose
;;
;; The idea is to get a single configuration file that will handle:
;;
;; - File system preparation (create necessary folders and files)
;; - Packages with the help of macros (wrappers for `package.el')
;; - Initialization of modes and packages
;; - Basic configuration
;; - Set of functions
;; - Keybindings
;;
;; * Install
;;
;; Warning: You might save your existing configuration before installing this one
;;
;; mkdir -p ~/src
;; git clone https://github.com/pierre-lecocq/emacs.d ~/src/emacs.d
;; cd  ~/src/emacs.d
;; make && make compile && make install
;;
;; * Credits
;;
;; Pierre Lecocq, version <BIGINT>
;;
;; * Disclaimer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Code:

;;;; core

(defun mkpath (path &optional is-directory create forced-base-dir)
  "Make path and eventually create it on file system."
  (unless (boundp 'base-dir) (setq base-dir user-emacs-directory))
  (if forced-base-dir
      (setq mkpath-base-dir forced-base-dir)
    (setq mkpath-base-dir base-dir))
  (let ((path (expand-file-name (concat (file-name-as-directory mkpath-base-dir) path))))
    (when create
      (if is-directory
          (progn
            (unless (file-accessible-directory-p path)
              (make-directory path t)))
        (progn
          (unless (file-exists-p path)
            (write-region "" nil path)))))
    path))

(defun init-package-manager (name)
  "Initialize the package manager. If package NAME is not installed, refresh it."
  (unless (boundp 'pm-initialized)
    (require 'package)
    (setq package-archives
          '(("melpa" . "http://melpa.org/packages/")
            ("gnu" . "http://elpa.gnu.org/packages/")
            ("marmalade" . "http://marmalade-repo.org/packages/")))
    (package-initialize)
    (setq pm-initialized t))
  (unless (or (package-built-in-p name)
              (package-installed-p name)
              (boundp 'pm-refreshed))
    (package-refresh-contents)
    (setq pm-refreshed t)))

(defmacro pkg-add (name &rest body)
  "Macro for package installation and intialization."
  `(progn
     (init-package-manager ,name)
     (unless (package-built-in-p ,name)
       (package-install ,name))
     (eval-after-load ,name
       (progn ,@body))))

;;;; internals

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(show-paren-mode t)
(setq show-paren-style 'expression)
(global-font-lock-mode t)
(transient-mark-mode t)
(line-number-mode t)
(column-number-mode t)
(global-auto-revert-mode 1)
(which-function-mode)

(setq
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 inhibit-startup-message t
 inhibit-splash-screen t
 initial-scratch-message ";; Scratch buffer\n\n"
 kill-whole-line t
 require-final-newline t
 next-line-add-newlines nil
 recentf-max-menu-items 50
 uniquify-buffer-name-style 'forward uniquify-separator "/"
 frame-title-format "Emacs %f"
 auto-insert-copyright (user-full-name))

(setq-default
 show-trailing-whitespace t
 highlight-tabs t)

(when window-system
  (set-fringe-mode '(1 . 1)))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))

(setq-default
 mode-line-format
 (list
  '(:eval (if (buffer-modified-p)
              (propertize "  %b" 'face 'bold-italic)
            (propertize "  %b" 'face 'bold)))
  " (%l:%c) %p/%I - %m";; (format " %s" minor-mode-alist)
  '(which-function-mode (" " which-func-format))))

;;;; variables

(setq
 user-full-name "Pierre Lecocq"
 user-mail-address "pierre.lecocq@gmail.com")

;; Comment this if you want to use `user-emacs-directory'
(setq base-dir (file-name-directory (or load-file-name (buffer-file-name))))

(setq
 bookmark-default-file (mkpath "bookmarks")
 package-user-dir (mkpath "vendor/packages" t t)
 org-dir (mkpath "org-files" t t "~/")
 custom-file (mkpath "custom.el")
 machine-file (mkpath (format "%s.el" (downcase (car (split-string system-name "\\."))))))

;;;; packages

(pkg-add 'anzu
         (global-anzu-mode +1)
         (set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold))

(pkg-add 'autopair (autopair-global-mode t))

(pkg-add 'company
         (setq company-auto-complete nil)
         (global-company-mode 1))

(pkg-add 'cycle-resize
         (global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
         (global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally))

(pkg-add 'darkmine-theme (load-theme 'darkmine t))
(pkg-add 'flycheck)
(pkg-add 'flx-ido)
(pkg-add 'htmlize)
(pkg-add 'idle-highlight-mode)

(pkg-add 'ido
         (require 'ido)
         (ido-mode t)
         (ido-everywhere 1)
         (flx-ido-mode 1)
         (setq ido-enable-flex-matching t)
         (setq ido-use-faces nil))

(pkg-add 'ido-hacks
         (require 'ido-hacks)
         (ido-hacks-mode))

(pkg-add 'ido-vertical-mode (ido-vertical-mode))
(pkg-add 'js2-mode)
(pkg-add 'markdown-mode)
(pkg-add 'php-extras)
(pkg-add 'php-mode)
(pkg-add 'rainbow-mode)
(pkg-add 'ruby-mode (setq ruby-deep-indent-paren nil))

(pkg-add 'symon
         (setq symon-delay 5)
         (symon-mode t))

(pkg-add 'visual-regexp)
(pkg-add 'web-mode)
(pkg-add 'yaml-mode)

;; (pkg-add 'indent-guide (indent-guide-global-mode))
;; (pkg-add 'switch-window (global-set-key (kbd "C-x o") 'switch-window))

;;;; private functions

(defun pl--set-locale (locale)
  "Initialize locale.
Argument LOCALE the locale to set."
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (setq locale-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(defun pl--init-indentation ()
  "Initialize indentation rules."
  (unless (string= major-mode "GNUmakefile")
    (setq-default
     tab-width 4
     c-basic-offset 4
     c-hanging-comment-ender-p nil
     indent-tabs-mode nil))
  (c-add-style
   "custom-four-indent"
   '((c-offsets-alist
      (arglist-close . 0)
      (arglist-intro . 4)
      (case-label . 4)))))

(defun pl--text-mode ()
  "Set up for text modes."
  (interactive)
  ;; Visual line mode
  (global-visual-line-mode 1)
  ;; Linum mode
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d "))

(defun pl--init-hooks ()
  "Initialize hooks."
  ;; After init
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Minibuffer mode
  (add-hook 'minibuffer-setup-hook '(lambda () (setq show-trailing-whitespace nil)))
  ;; Text modes
  (add-hook 'org-mode-hook 'pl--text-mode)
  (add-hook 'markdown-mode-hook 'pl--text-mode)
  (add-hook 'text-mode-hook 'pl--text-mode)
  ;; Lisp mode hook
  (add-hook 'lisp-mode-hook (lambda () (idle-highlight-mode t)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (idle-highlight-mode t)))
  ;; C mode hook
  (add-hook 'c-mode-hook (lambda () (idle-highlight-mode t)))
  (add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))
  ;; Ruby mode hook
  (add-hook 'ruby-mode-hook
            (lambda()
              (idle-highlight-mode t)
              (global-set-key (kbd "C-c C-r") 'pl-rb-require)))

  ;; Before save hook
  (add-hook 'before-save-hook
            (lambda()
              (time-stamp)
              (delete-trailing-whitespace)
              (whitespace-cleanup)))

  ;; PHP mode hook
  (add-hook 'php-mode-hook
            (lambda ()
              (require 'php-extras)
              (idle-highlight-mode t)
              (setq comment-start "// ")
              (setq comment-end "")
              (set (make-local-variable 'indent-tabs-mode) nil)
              (c-set-style "custom-four-indent")))

  ;; CSS mode hook
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
  ;; JS mode hook
  (add-hook 'js2-mode-hook (lambda () (idle-highlight-mode t)))
  ;; Find file hook
  (add-hook 'find-file-hook
            '(lambda ()
               (auto-insert)
               ;; Set locale (a.k.a FUCK you, PHP)
               (if (string= major-mode "php-mode")
                   (pl--set-locale 'latin-1)
                 (pl--set-locale 'utf-8))
               ;; Special indentation
               (if (and buffer-file-name
                        (string-match "/gnulib\\>" (buffer-file-name))
                        (not (string-equal mode-name "Change Log"))
                        (not (string-equal mode-name "Makefile")))
                   (setq indent-tabs-mode nil)))))

(defun pl--init-files-modes ()
  "Initialize files modes."
  (add-to-list 'auto-mode-alist '(".bashrc" . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".zshrc" . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".gnus" . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".rake" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".ru" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".gemspec" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".erb" . rhtml-mode))
  (add-to-list 'auto-mode-alist '(".erubis" . rhtml-mode))
  (add-to-list 'auto-mode-alist '(".ya?ml" . yaml-mode))
  (add-to-list 'auto-mode-alist '(".js" . js2-mode))
  (add-to-list 'auto-mode-alist '(".json" . js2-mode))
  (add-to-list 'auto-mode-alist '(".html?" . web-mode)))

;;;; public functions

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*shell*") (buffer-list))
          (switch-to-buffer "*shell*")
        (shell)))))

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when window-system
    (set-frame-parameter (selected-frame) 'alpha value)))

(defun pl-rb-require ()
  "Insert required rubygems."
  (interactive "*")
  (let ((gems (read-from-minibuffer "Rubygems to require: ")))
    (when gems
      (mapcar (lambda (gem)
                (insert (format "require \"%s\"\n" gem)))
              (split-string gems nil t)))))

;;;; keybindings

(when (eq system-type 'darwin)
  (setq
   mac-option-modifier nil
   mac-command-modifier 'meta
   select-enable-clipboard t))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-S-s") 'find-grep)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "C-S-f") 'imenu)
(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f12] 'pl-get-shell)

;;;; org-mode

(setq
 org-fontify-done-headline t
 org-src-fontify-natively t
 org-agenda-files (list
                   (mkpath "agenda.org" nil t org-dir)
                   ;; Add other files here ...
                   ))

(defun org-font-lock-ensure ()
  "Org font lock ensure."
  (font-lock-ensure))

;;;; autoinserts

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
         "#!/usr/bin/env ruby\n\n"
         "# File: " (file-name-nondirectory buffer-file-name) "\n"
         "# Time-stamp: <>\n"
         "# Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n"
         "# Description: " _ "\n\n")
        ((emacs-lisp-mode . "Emacs lisp mode") nil
         ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
         ";;; Commentary:\n"
         ";; Time-stamp: <>\n"
         ";; Copyright (C) " (substring (current-time-string) -4) " " auto-insert-copyright "\n\n"
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

;;;; init

(pl--init-indentation)
(pl--init-files-modes)
(pl--init-hooks)

;;;; load files

(load custom-file 'noerror)
(load machine-file 'noerror)

;;; init.el ends here
