;;; config-bootstrap.el --- Emacs configuration - bootstrap

;; Time-stamp: <2016-02-24 10:39:42>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Package manager settings
(require 'package)

(setq package-user-dir packages-dir
      package-enable-at-startup nil
      package-archives '(("melpa"        . "http://melpa.org/packages/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Define internal variables
(setq debug-on-error t
      gc-cons-threshold 100000000
      sentence-end-double-space nil
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      kill-whole-line t
      require-final-newline t
      next-line-add-newlines nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      vc-follow-symlinks t
      password-cache-expiry nil
      uniquify-buffer-name-style 'forward uniquify-separator "/"
      custom-file (concat files-dir "custom.el")
      tramp-persistency-file-name (concat files-dir "remote"))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'config-bootstrap)

;;; config-bootstrap.el ends here
