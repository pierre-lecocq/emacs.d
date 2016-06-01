;;; init.el --- Minimal Emacs config file

;; Time-stamp: <2016-03-18 08:18:33>
;; Copyright (C) 2015 Pierre Lecocq
;; Version: <insert a bigint here>

;;; Commentary:
;;
;; A return to a minimal Emacs config
;;
;; This file is made to be used as a fast and standalone minimal config file
;; but can load some extensions from a given directory by creating symlinks
;; from `lisp-available-dir' to `lisp-enabled-dir'.
;;

;;; Code:

;; Internals

(mapcar 'require '(linum paren time-stamp))

(mapcar (lambda (mode) (funcall mode 1))
	'(auto-compression-mode
	  column-number-mode
	  global-auto-revert-mode
	  global-font-lock-mode
	  ido-mode
	  line-number-mode
	  show-paren-mode
	  transient-mark-mode
	  which-function-mode))

(setq debug-on-error t
      gc-cons-threshold 100000000
      sentence-end-double-space nil
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      frame-title-format "Emacs - %b"
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
      uniquify-buffer-name-style 'forward uniquify-separator "/")

(fset 'yes-or-no-p 'y-or-n-p)

;; Look'n'feel

(set-background-color "#222222")
(set-foreground-color "#e5e5e5")
(set-face-background 'region "#444444")

(mapcar (lambda (mode) (funcall mode -1))
	'(menu-bar-mode
	  scroll-bar-mode
	  tool-bar-mode
	  tooltip-mode))

(when (display-graphic-p)
  (set-fringe-mode 10)
  (toggle-frame-maximized)
  (setq select-enable-clipboard t))

;; Autoload config

(defvar lisp-available-dir "~/.emacs.d/lisp/available")
(defvar lisp-enabled-dir "~/.emacs.d/lisp/enabled")

(defun enable-lisp-file ()
  "Enable a lisp file."
  (interactive)
  (unless (file-exists-p lisp-available-dir) (error "%s not found" lisp-available-dir))
  (unless (file-exists-p lisp-enabled-dir) (error "%s not found" lisp-enabled-dir))
  (let ((file (ido-completing-read "Select a file: " (directory-files lisp-available-dir nil "\\.el"))))
    (shell-command (format "ln -s %s/%s %s/%s" lisp-available-dir file lisp-enabled-dir file))))

(if (file-exists-p lisp-enabled-dir)
    (mapcar 'load-file (directory-files lisp-enabled-dir t "\\.el"))
  (message "No lisp files enabled in %s" lisp-enabled-dir))

;;; init.el ends here
