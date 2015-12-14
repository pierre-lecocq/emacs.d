;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-14 14:59:33>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Set directories paths (note: trailing slash is mandatory)
(defvar root-dir (file-name-as-directory (file-truename (file-name-directory load-file-name))))

(defvar hosts-dir (concat root-dir "lisp/hosts/"))
(defvar versions-dir (concat root-dir "lisp/versions/"))
(defvar config-dir (concat root-dir "lisp/config/"))

(defvar files-dir (concat root-dir "vendor/files/"))
(defvar packages-dir (concat root-dir "vendor/packages/"))

(unless (file-exists-p files-dir)
  (make-directory files-dir t))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir)
(add-to-list 'load-path hosts-dir)
(add-to-list 'load-path versions-dir)

;; Early requires
(require 'init-bootstrap)
(require 'init-looknfeel)

;; Standard requires
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
(require 'init-recentf)
(require 'init-ruby)
(require 'init-shell)
(require 'init-spelling)
(require 'init-text)
(require 'init-web)

;; Late requires
(require 'init-hooks)
(require 'init-functions)
(require 'init-keybindings)

;; Load host and version specific file at the end to eventually override defaults
(when (file-exists-p host-file)
  (load host-file 'noerror))

(when (file-exists-p version-file)
  (load version-file 'noerror))

;;; init.el ends here
