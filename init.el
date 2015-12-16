;;; init.el --- Emacs init file

;; Time-stamp: <2015-12-15 17:06:16>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar please-compile-lisp nil)

;; Directories
(defvar root-dir   "~/src/emacs.d/") ;; (file-name-as-directory (file-truename (file-name-directory load-file-name)))
(defvar lisp-dir    (concat root-dir "lisp/"))
(defvar vendor-dir  (concat root-dir "vendor/"))

(defvar config-dir (concat lisp-dir "config/"))
(defvar hosts-dir (concat lisp-dir "hosts/"))
(defvar versions-dir (concat lisp-dir "versions/"))

(defvar files-dir (concat vendor-dir "files/"))
(defvar packages-dir (concat vendor-dir "packages/"))

(unless (file-exists-p files-dir)
  (setq please-compile-lisp t)
  (make-directory files-dir t))

;; Add some config directories to load-path
(add-to-list 'load-path config-dir)
(add-to-list 'load-path hosts-dir)
(add-to-list 'load-path versions-dir)

;; Secret file
(defvar secret-file (concat config-dir "secret.el"))

(if (file-exists-p secret-file)
    (require 'secret)
  (error "%s not found" secret-file))

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

;; Compile if needed
;; (when please-compile-lisp
;;   (byte-recompile-directory lisp-dir 0))

;;; init.el ends here
