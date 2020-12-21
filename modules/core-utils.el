;;; core-utils.el --- Utils -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-21 13:45:10>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package autopair :ensure t
  :config (autopair-global-mode t))

(use-package bm :ensure t
  :config (setq bm-repository-file (expand-file-name ".cache/bm-repository" user-emacs-directory))
  :bind (("C-c b t" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous)
         ("C-c b s" . bm-show-all)
         ("C-c b r" . bm-remove-all-current-buffer)
         ("C-c b R" . bm-remove-all-all-buffers)))

(use-package dired :ensure nil :demand t
  :init (setq dired-recursive-copies 'always
              dired-recursive-deletes 'always
              delete-by-moving-to-trash t
              dired-listing-switches "-aFlv"
              wdired-allow-to-change-permissions t
              wdired-create-parent-directories t
              dired-use-ls-dired (if (eq system-type 'darwin) nil t)))

(use-package editorconfig :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package epa-file :ensure nil :demand t
  ;; :config (epa-file-enable)
  :init (setq epa-gpg-program "gpg2"))

(use-package exec-path-from-shell :ensure t
  :when (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package ibuffer :ensure nil :demand t
  :init (setq ibuffer-expert t
              ibuffer-display-summary nil
              ibuffer-use-other-window nil
              ibuffer-show-empty-filter-groups nil)
  :bind ("C-x C-b" . ibuffer))

(use-package idle-highlight-mode :ensure t
  :hook (prog-mode . idle-highlight-mode))

(use-package ido :ensure t
  :config (progn
            (use-package flx-ido :ensure t)
            (use-package ido-hacks :ensure t)
            (use-package ido-vertical-mode :ensure t)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode))
  :init (setq ido-save-directory-list-file (expand-file-name ".cache/ido.el" user-emacs-directory)
              ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always
              ido-vertical-show-count t))

(use-package string-inflection :ensure t
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(use-package time-stamp :ensure t :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save . time-stamp))

(use-package uniquify :ensure nil :demand t
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-separator "/"
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"))

(use-package which-func :ensure t :demand t
  :init (setq which-func-unknown "?")
  :hook (prog-mode . which-function-mode))

(use-package which-key :demand t :ensure t
  :init (setq which-key-popup-type 'side-window
              which-key-side-window-location 'bottom
              which-key-side-window-max-height 0.5
              which-key-max-description-length 200
              which-key-add-column-padding 2)
  :config (which-key-mode 1))

(use-package whitespace :demand t :ensure nil
  :init (setq whitespace-line-column 80
              whitespace-style '(tabs tab-mark face trailing))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

(provide 'core-utils)

;;; core-utils.el ends here
