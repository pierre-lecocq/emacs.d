;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-07 23:22:34>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set directories paths (note: trailing slash is mandatory)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq config-dir (file-name-as-directory (file-truename (file-name-directory load-file-name))))

(setq config-dir-files      (concat config-dir "lisp/files/")
      config-dir-hosts      (concat config-dir "lisp/hosts/")
      config-dir-modules    (concat config-dir "lisp/modules/")
      config-dir-packages   (concat config-dir "lisp/packages/"))

(unless (file-exists-p config-dir-files)
  (make-directory config-dir-files))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir-hosts)
(add-to-list 'load-path config-dir-modules)

;; Require modules
(mapc #'require
      '(init-internals
        init-packages
        ;; Settings
        init-looknfeel
        init-hooks
        init-functions
        init-keybindings
        ;; Modes
        init-org-mode
        init-completion
        init-indent
        init-locale
        init-bookmark
        init-filetypes
        init-autoinsert
        init-recentf
        init-erc
        init-elfeed))

;; Load host specific file at the end to eventually override defaults
(load host-file 'noerror)
