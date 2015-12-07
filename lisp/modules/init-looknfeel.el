;;; init-looknfeel.el --- Emacs config - looknfeel

;; Time-stamp: <2015-12-08 00:09:25>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:
(use-package anzu :ensure t
  :init (progn
          (global-anzu-mode +1)
          (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(use-package autopair :ensure t
  :init (autopair-global-mode t))

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
      ;; VC
      vc-follow-symlinks t
      ;; Password cache
      password-cache-expiry nil
      ;; Uniquify
      uniquify-buffer-name-style 'forward uniquify-separator "/")

(setq-default mode-line-format (list
                                '(:eval (if (buffer-modified-p)
                                            (propertize " %b" 'face 'bold-italic)
                                          (propertize " %b" 'face 'bold)))
                                " | %l:%c %p:%I | %m";; (format " %s" minor-mode-alist)
                                '(which-function-mode (" " which-func-format))
                                '(vc-mode vc-mode)))

(when (display-graphic-p)
  (progn
    (toggle-frame-maximized)
    (setq show-paren-style 'expression
          select-enable-clipboard t))
  (set-fringe-mode 10))

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata" ;; "DejaVu Sans Mono"
                      :height 120
                      :weight 'normal
                      :width 'normal))

(provide 'init-looknfeel)

;;; init-looknfeel.el ends here
