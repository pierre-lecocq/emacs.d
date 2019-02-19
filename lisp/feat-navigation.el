;;; feat-navigation.el --- Navigation support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-02-19 08:00:51>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flx-ido :ensure t)

(use-package ido-hacks :ensure t)

(use-package ido-vertical-mode :ensure t)

(use-package ido :ensure t
  :after (:all flx-ido ido-hacks ido-vertical-mode)
  :config (progn
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode))
  :init (setq ido-save-directory-list-file (concat (file-name-directory load-file-name) "../local/my-ido.el")
              ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always
              ido-vertical-show-count t))

(use-package find-file-in-project :ensure t
  :bind (("C-S-x C-S-f" . find-file-in-project))
  :init (setq ffip-prefer-ido-mode t
              ffip-prune-patterns '("*/.git/*"
                                    "*/packages/*"
                                    "*/vendor/*"
                                    "*/node_modules/*")))

(use-package dumb-jump
  :config (dumb-jump-mode)
  :bind (("C-M-g" . dumb-jump-quick-look)
         ("C-M-o" . dumb-jump-go-other-window)))

;;; feat-navigation.el ends here
