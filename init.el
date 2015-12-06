;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-07 00:29:59>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set config directory and add relevant subdirectories to load-path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)

(setq config-dir (file-name-as-directory (file-truename (file-name-directory load-file-name))))
(add-to-list 'load-path (concat config-dir "lisp/modules"))
(add-to-list 'load-path (concat config-dir "lisp/hosts"))

(mapc #'(lambda (name)
          (let ((dir (format "%slisp/%s/" config-dir name)))
            (unless (file-exists-p dir)
              (make-directory dir))))
      '("files"
        "hosts"))

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
        my-looknfeel
        my-autoinsert
        my-recentf))

;; Load host specific file
(load host-file 'noerror)
