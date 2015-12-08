;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-08 22:05:15>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set directories paths (note: trailing slash is mandatory)

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

;; Require modules
(mapc #'require
      '(init-internals
        init-packages
        ;; Modes
        init-autoinsert
        init-bookmark
        init-completion
        init-elfeed
        init-erc
        init-filetypes
        init-ido
        init-indent
        init-lisp
        init-locale
        init-recentf
        init-ruby
        init-spelling
        init-text
        init-web
        ;; Settings
        init-looknfeel
        init-hooks
        init-functions
        init-keybindings))

;; Load host specific file at the end to eventually override defaults
(load host-file 'noerror)
