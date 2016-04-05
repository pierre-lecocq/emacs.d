;;; init.el --- Emacs init file

;; Time-stamp: <2016-04-05 11:42:22>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Install:

;; ln -s ~/src/emacs.d ~/.emacs.d

;;; Code:

;; Directories
(defvar root-dir        "~/src/emacs.d/")
(defvar lisp-dir        (concat root-dir "lisp/"))
(defvar config-dir      (concat lisp-dir "config/"))
(defvar host-dir        (concat lisp-dir "host/"))
(defvar version-dir     (concat lisp-dir "version/"))
(defvar vendor-dir      (concat root-dir "vendor/"))
(defvar files-dir       (concat vendor-dir "files/"))
(defvar packages-dir    (concat vendor-dir "packages/"))

(unless (file-exists-p files-dir)
  (make-directory files-dir t))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir)
(add-to-list 'load-path files-dir)

;; Secret file
(let ((secret-file (concat files-dir "secret.el")))
  (if (file-exists-p secret-file)
      (require 'secret)
    (error "%s not found.  Please copy and adapt it from %ssecret.el-sample" secret-file lisp-dir)))

;; Early requires
(require 'pl-bootstrap)
(require 'pl-theme)
(require 'pl-looknfeel)
(require 'pl-modeline)

;; Standard requires
(require 'pl-autoinsert)
(require 'pl-bookmark)
(require 'pl-completion)
(require 'pl-encrypt)
(require 'pl-execute)
(require 'pl-ffip)
(require 'pl-filetypes)
(require 'pl-ido)
(require 'pl-indent)
(require 'pl-lisp)
(require 'pl-locale)
(require 'pl-newsticker)
(require 'pl-recentf)
(require 'pl-ruby)
(require 'pl-shell)
(require 'pl-spelling)
;; (require 'pl-swiper)
(require 'pl-text)
(require 'pl-web)

;; Late requires
(require 'pl-hooks)
(require 'pl-functions)
(require 'pl-keybindings)

;; Load host and version specific file at the end to eventually override defaults
(let ((host-file (concat host-dir (pl-clean-system-name) ".el"))
      (version-file (concat version-dir (number-to-string emacs-major-version) ".el")))
  (load host-file 'noerror)
  (load version-file 'noerror))

;;; init.el ends here
