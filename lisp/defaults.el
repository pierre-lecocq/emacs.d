;;; defaults.el --- Defaults -*- lexical-binding: t; -*-

;; Time-stamp: <2019-01-04 10:45:26>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Package manager

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (concat (file-name-directory load-file-name) "../local/packages")
      package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("org" . 20)
                                   ("melpa" . 15)
                                   ("melpa-stable" . 12)
                                   ("marmalade" . 10)
                                   ("gnu" . 5)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Packages

(use-package diminish :ensure t)

(use-package autoinsert :demand t :ensure nil
  :init (progn
          (auto-insert-mode 1)
          (auto-insert)))

(use-package time-stamp :demand t :ensure nil
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save))

(use-package editorconfig :ensure t
  :config (editorconfig-mode 1))

(use-package autopair :ensure t :diminish autopair-mode
  :config (autopair-global-mode t))

(use-package which-func :demand t :ensure nil
  :config (progn (which-function-mode 1)
                 (set-face-attribute 'which-func nil :foreground "red")))

(use-package which-key :demand t
  :config (which-key-mode 1))

;; Key bindings

(require 'bind-key)

(when (and window-system
           (eq system-type 'darwin))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-S-f") 'imenu)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "M-;") 'other-frame)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; defaults.el ends here
