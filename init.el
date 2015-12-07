;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-07 11:45:37>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set directories paths (note: trailing slash is mandatory)
(setq config-dir (file-name-as-directory (file-truename (file-name-directory load-file-name))))
(setq config-dir-files (concat config-dir "lisp/files/")
      config-dir-hosts (concat config-dir "lisp/hosts/")
      config-dir-modules (concat config-dir "lisp/modules/")
      config-dir-packages (concat config-dir "lisp/packages/"))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir-hosts)
(add-to-list 'load-path config-dir-modules)

;; Require modules
(mapc #'require
      '(my-internals
        my-packages
        my-hooks
        my-functions
        my-keybindings
        my-indent
        my-locale
        my-bookmark
        my-filetypes
        my-autoinsert
        my-recentf
        my-looknfeel))

;; Load host specific file at the end to eventually override defaults
(load host-file 'noerror)

;; Byte compile config directories
(byte-recompile-directory config-dir-hosts 0)
(byte-recompile-directory config-dir-modules 0)
