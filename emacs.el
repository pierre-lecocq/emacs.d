;;; init.el --- Emacs Config - Main file

;;; Commentary:
;; Time-stamp: <2015-02-26 13:34:31 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;
;; This file is another step into the world of emacs configuration holy grail.
;;
;; Step 1: Use emacs like rms just the time to realize that emacs is amazing
;; Step 2: Be amazed by lots of things you do not even understand
;; Step 3: Copy & paste from everywhere in a fucking messy config file
;; Step 4: Split your config into many files but don't figure out which part goes where
;; Step 5: Begin to write your own 5-lines lisp functions that are, in fact, already implemented
;; Step 6: Try to organize the whole mess and learn some real emacs lisp basics
;; Step 7: Be sick & tired of all the code you don't understand and don't use even 10% of the time
;; Step 8: Be honest and realize that you don't know more than 60% of elisp's power
;; Step 9: Empty your config files and try to make a single file one to figure out what's going on
;; Step 10: Clean the code you stole elsewhere but don't use
;; Step 11: Rewrite you config by keeping the things you understand
;; Step 12: Be honest and realize that you don't know more than 40% of elisp's power
;; Step 13: Read more blogs, youtube chats or reddit posts
;; Step 14: Be honest and realize that you don't know more than 30% of elisp's power
;; Step 15: Be honest and realize that you don't know more than 25% of elisp's power
;; Step 16: Stick with your config but think about making it better
;; Step 17: Buy books about lisp or emacs lisp
;; Step 18: Be honest and realize that you don't know more than 20% of elisp's power
;; Step 19: Discover package managers and macros like use-package
;; Step 20: Feel cool with a clean and mastered config files
;; Step 21: Try to make it cleaner and smaller
;; Step 22: Try to figure out what's going on behind package managers. Be mind fucked.
;; Step 23: Be honest and realize that you don't know more than 15% of elisp's power
;; Step 24: Finally, be confident and realize that you know more than 15% of elisp's power
;; Step 25: Write your own functions and macros to manage your packages, initializations and keybindings
;; Step 26: Realize that you understand any line of your configuration, but it is only "configuration", not programs
;; Step 27: Be confident and realize that you MIGHT know more than 40% of elisp's power if you read more documentation
;; Step 28: write steps you crossed to finally be here, but know that the path is still hard and long. Without any pun.
;;

;;; Code:

(setq debug-on-error t)

;;;; core

(defun mkpath (path &optional create forced-base-dir)
  "Make path and eventually creat it on file system."
  (unless (boundp 'base-dir)
    (setq base-dir user-emacs-directory))
  (if forced-base-dir
      (setq mkpath-base-dir forced-base-dir)
    (setq mkpath-base-dir base-dir))
  (let ((path (expand-file-name (concat (file-name-as-directory mkpath-base-dir) path))))
    (when create
      (unless (file-accessible-directory-p path)
	(make-directory path t)))
    path))

(defun init-package-manager (name)
  "Initialize the package manager. If package NAME is not installed, refresh it."
  (unless (boundp 'pm-initialized)
    (message " + Initializing package.el ...")
    (require 'package)
    (setq package-archives '(("melpa" . "http://melpa.org/packages/")))
    (package-initialize)
    (setq pm-initialized))
  (unless (or (package-built-in-p name)
	      (package-installed-p name)
	      (boundp 'pm-refreshed))
    (message " + Refreshing packages list ...")
    (package-refresh-contents)
    (setq pm-refreshed t)))

(defmacro pkg-add (name &rest body)
  "Macro for package installation."
  (init-package-manager name)
  (unless (package-built-in-p name)
    `(package-install ,name))
  ;; `(eval-after-load ,name
  ;;    '(progn ,@body))
  )

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
 kill-whole-line t
 require-final-newline t
 next-line-add-newlines nil
 recentf-max-menu-items 50
 uniquify-buffer-name-style 'forward uniquify-separator "/"
 frame-title-format "Emacs %f")

(setq-default
 show-trailing-whitespace t
 highlight-tabs t)

(add-hook 'before-save-hook
	  (lambda()
	    (delete-trailing-whitespace)
	    (whitespace-cleanup)))

(when window-system
  (set-fringe-mode '(1 . 1)))

(when (member "Inconsolata-g" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-g-10"))

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

(setq base-dir "~/emacs.d-single-file/")
(setq
 package-user-dir (mkpath "vendor/packages" t)
 orgs-dir (mkpath "org" t "~/")
 custom-file (mkpath "custom.el")
 bookmark-default-file (mkpath "bookmarks"))

(load custom-file 'noerror)

;;;; packages

(pkg-add 'ido)

(pkg-add 'anzu)

(pkg-add 'darkmine-theme
	 (load-theme 'darkmine t))

(pkg-add 'symon
	 (message "En voiture symon")
	 (symon-mode t))

;;;; functions

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*shell*") (buffer-list))
	  (switch-to-buffer "*shell*")
	(shell)))))

;;;; keybindings

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

;;;; autoinserts

;;; init.el ends here
