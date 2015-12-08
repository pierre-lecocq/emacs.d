;;; init-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-08 23:05:31>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package anzu :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package autopair :ensure t
  :init (autopair-global-mode t))

(use-package buffer-move :ensure t
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package bonjourmadame :ensure t)

(use-package browse-kill-ring :ensure t)

(use-package darkmine-theme :ensure t
  :init (load-theme 'darkmine t))

(use-package idle-highlight-mode :ensure t)

(use-package symon :ensure t
  :init (progn
          (setq symon-delay 5)
          (symon-mode t)))

(setq frame-title-format "Emacs %f"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
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
      auto-save-list-file-prefix nil
      ;; VC
      vc-follow-symlinks t
      ;; Password cache
      password-cache-expiry nil
      ;; Uniquify
      uniquify-buffer-name-style 'forward uniquify-separator "/")

(when (display-graphic-p)
  (set-fringe-mode 10)
  (toggle-frame-maximized)
  (setq show-paren-style 'expression
        select-enable-clipboard t))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(provide 'init-looknfeel)

;;; init-looknfeel.el ends here
