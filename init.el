;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-09 00:13:30>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set directories paths (note: trailing slash is mandatory)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar config-dir
  (file-name-as-directory (file-truename (file-name-directory load-file-name))))

(defvar config-dir-files
  (concat config-dir "lisp/files/"))

(defvar config-dir-hosts
  (concat config-dir "lisp/hosts/"))

(defvar config-dir-modules
  (concat config-dir "lisp/modules/"))

(defvar config-dir-packages
  (concat config-dir "lisp/packages/"))

(unless (file-exists-p config-dir-files)
  (make-directory config-dir-files))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir-hosts)
(add-to-list 'load-path config-dir-modules)

;; Activate internal modes
(auto-compression-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(global-hl-line-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(which-function-mode 1)

;; Deactivate internal modes
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Require modules
(require 'linum)
(require 'paren)
(require 'time-stamp)

(require 'init-package)
(require 'init-autoinsert)
(require 'init-bookmark)
(require 'init-completion)
(require 'init-elfeed)
(require 'init-erc)
(require 'init-filetypes)
(require 'init-ido)
(require 'init-indent)
(require 'init-lisp)
(require 'init-locale)
(require 'init-mode-line)
(require 'init-recentf)
(require 'init-ruby)
(require 'init-shell)
(require 'init-spelling)
(require 'init-text)
(require 'init-web)

(require 'init-looknfeel)
(require 'init-hooks)
(require 'init-functions)
(require 'init-keybindings)

;; Define internal variables
(setq debug-on-error t
      gc-cons-threshold 100000000
      sentence-end-double-space nil
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      custom-file (concat config-dir-files "custom.el")
      tramp-persistency-file-name (concat config-dir-files "tramp")
      host-file (concat config-dir-hosts (downcase (car (split-string (system-name) "\\."))) ".el"))

(fset 'yes-or-no-p 'y-or-n-p)

;; Load host specific file at the end to eventually override defaults
(load host-file 'noerror)
