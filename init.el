;;; init.el --- Minimal Emacs config file

;; Time-stamp: <2016-12-11 18:05:10>
;; Copyright (C) 2015 Pierre Lecocq
;; Version: <insert your bigint here>

;;; Commentary:
;;
;; A return to a minimal Emacs config
;;
;; This `init.el' file is made to be used as a fast and standalone minimal config
;; file but can load some extensions from a given directory by creating symlinks
;; from `lisp-available-dir' to `lisp-enabled-dir'.

;;; Code:

;; (package-initialize)

(defvar lisp-dir (expand-file-name (convert-standard-filename "lisp") user-emacs-directory))
(defvar lisp-available-dir (concat (file-name-as-directory lisp-dir) "available"))
(defvar lisp-enabled-dir (concat (file-name-as-directory lisp-dir) "enabled"))
(defvar files-dir (concat (file-name-as-directory lisp-dir) "files"))

;; Internals

(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (mode) (funcall mode 1))
      '(auto-compression-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        ido-mode
        line-number-mode
        show-paren-mode
        subword-mode
        transient-mark-mode
        which-function-mode))

(setq debug-on-error t
      gc-cons-threshold 100000000
      load-prefer-newer t
      sentence-end-double-space nil
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
      frame-title-format "Emacs - %b"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      case-fold-search t
      kill-whole-line t
      require-final-newline t
      next-line-add-newlines nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      vc-follow-symlinks t
      password-cache-expiry nil
      highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face lines-tail)
      whitespace-global-modes '(not org-mode web-mode)
      uniquify-buffer-name-style 'forward uniquify-separator "/")

(setq custom-file (concat (file-name-as-directory files-dir) "pl-custom.el")
      abbrev-file-name (concat (file-name-as-directory files-dir) "pl-abbrev.el")
      bookmark-default-file (concat (file-name-as-directory files-dir) "pl-bookmarks.el")
      nsm-settings-file (concat (file-name-as-directory files-dir) "pl-nsm-settings.el")
      recentf-save-file (concat (file-name-as-directory files-dir) "pl-recentf.el")
      ido-save-directory-list-file (concat (file-name-as-directory files-dir) "pl-ido.el")
      url-configuration-directory (concat (file-name-as-directory files-dir) "url")
      tramp-persistency-file-name (concat (file-name-as-directory files-dir) "pl-tramp.el"))

;; Locale

(defun pl-set-locale (locale)
  "Set locale to LOCALE."
  (interactive)
  (set-charset-priority 'unicode)
  (setq locale-coding-system locale)
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(pl-set-locale 'utf-8)

;; Indentation

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil)

;; Look'n'feel

(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode
        tooltip-mode))

(setq select-enable-clipboard t)

(if (display-graphic-p)
    (progn
      (set-background-color "grey13")
      (set-foreground-color "grey93")
      (set-face-background 'region "DodgerBlue")
      (set-face-foreground 'region "white")
      (setq x-select-enable-clipboard t))
  (setq frame-background-mode 'dark))

(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; Autoload config

(if (file-exists-p lisp-enabled-dir)
    (progn
      (when init-file-debug
        (require 'benchmark))
      (add-to-list 'load-path lisp-enabled-dir)
      (mapc (lambda (fname)
              (let ((feat (intern (file-name-base fname)) ))
                (if init-file-debug
                    (message "Feature '%s' loaded in %.2fs" feat (benchmark-elapse (require feat fname)))
                  (require feat fname))))
            (directory-files lisp-enabled-dir t "\\.el")))
  (message "Directory \"%s\" not found. No extension have been loaded." lisp-enabled-dir))

;;; init.el ends here
