;;; emacs.el --- Emacs Config - Main file

;; Time-stamp: <2015-02-26 13:34:31 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

;; dev mode
(setq debug-on-error t)

;; semi-dev mode (will be naturally `user-emacs-directory')
(setq base-dir "~/emacs.d-single-file/")

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
    (require 'package)
    (setq package-archives
	  '(("melpa" . "http://melpa.org/packages/")
	    ("gnu" . "http://elpa.gnu.org/packages/")
	    ("marmalade" . "http://marmalade-repo.org/packages/")))
    (package-initialize)
    (setq pm-initialized))
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
	 (message "Set DAT theme")
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
