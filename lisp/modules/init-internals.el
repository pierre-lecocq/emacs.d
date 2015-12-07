;;; init-internals.el --- Emacs config - internals

;; Time-stamp: <2015-12-07 14:40:20>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Require internal packages
(mapc #'require
      '(autoinsert
        bookmark
        em-alias
        linum
        paren
        recentf
        time-stamp
        whitespace))

;; Activate internal modes
(mapc #'(lambda (mode)
          (when (fboundp mode)
            (funcall mode 1)))
      '(auto-compression-mode
        auto-insert-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        global-hl-line-mode
        line-number-mode
        recentf-mode
        show-paren-mode
        transient-mark-mode
        which-function-mode))

;; Deactivate internal modes
(mapc #'(lambda (mode)
          (when (fboundp mode)
            (funcall mode -1)))
      '(menu-bar-mode
        tool-bar-mode
        scroll-bar-mode))

;; Laziness...
(fset 'yes-or-no-p 'y-or-n-p)

;; Define internal variables
(setq debug-on-error t
      gc-cons-threshold 100000000

      ;; Identity
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"

      ;; Scratch and Splash
      initial-scratch-message (format ";; Scratch buffer - started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t

      ;; General behaviour
      kill-whole-line t
      require-final-newline t
      next-line-add-newlines nil

      ;; Backup
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil

      ;; VC
      vc-follow-symlinks t

      ;; Password cache
      password-cache-expiry nil

      ;; Uniquify
      uniquify-buffer-name-style 'forward uniquify-separator "/"

      ;; Other paths
      custom-file (concat config-dir-files "custom.el")
      tramp-persistency-file-name (concat config-dir-files "tramp")
      nsm-settings-file (concat config-dir-files "network-security.data")
      host-file (concat config-dir-hosts (downcase (car (split-string (system-name) "\\."))) ".el"))

(provide 'init-internals)

;;; init-internals.el ends here
