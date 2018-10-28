;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Time-stamp: <2018-10-28 11:28:38>
;; Copyright (C) 2017 Pierre Lecocq
;; Version: <insert your bigint here>

;;; Commentary:

;; Code name: "Yet another rewrite"

;;; Code:

;; Laziness

(fset 'yes-or-no-p 'y-or-n-p)

;; Optimizations for config loading

(defvar -file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook #'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold 16777216
                        gc-cons-percentage 0.1
                        file-name-handler-alist -file-name-handler-alist)))

;; Enable modes

(mapc (lambda (mode) (when (fboundp mode) (funcall mode 1)))
      '(auto-compression-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        ;; global-hl-line-mode
        line-number-mode
        show-paren-mode
        subword-mode
        transient-mark-mode))

;; Disable modes

(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode
        tooltip-mode))

(when (or (not window-system) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

;; Default variables

(setq user-full-name "Pierre Lecocq"
      debug-on-error t
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      load-prefer-newer t
      sentence-end-double-space nil
      frame-title-format "%b (%m) - %F"
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      case-fold-search t
      require-final-newline t
      next-line-add-newlines nil
      select-enable-clipboard t
      show-trailing-whitespace t
      custom-file (concat (file-name-directory load-file-name) "local/my-custom.el")
      uniquify-buffer-name-style 'forward uniquify-separator "/")

;; Indentation

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              electric-indent-inhibit t
              backward-delete-char-untabify-method 'hungry)

;; Locale

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Full screen

(when (display-graphic-p)
  (toggle-frame-maximized))

;; Load host file.

;; This file should load other files from the `lisp' folder.
;;
;; Sample content:
;;
;;     (let ((dir (file-name-directory load-file-name)))
;;       (load-file (concat dir "lisp/defaults.el"))
;;       (load-file (concat dir "lisp/feat-theme.el")))

(let ((host-file (concat (file-name-directory load-file-name) "host.el")))
  (if (file-exists-p host-file)
      (load-file host-file)
    (message "WARNING: %s file not found, no extention will be loaded" host-file)))

;;; init.el ends here
