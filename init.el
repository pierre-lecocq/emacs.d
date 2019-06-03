;;; init.el --- Emacs config -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 21:59:55>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (mode) (when (fboundp mode) (funcall mode 1)))
      '(column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        global-hl-line-mode
        line-number-mode
        show-paren-mode
        subword-mode
        transient-mark-mode))

(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode
        tooltip-mode
        menu-bar-mode))

(setq gc-cons-threshold 50000000 ; 50MB
      large-file-warning-threshold 100000000 ; 100MB
      debug-on-error t
      frame-title-format "%b (%m) - %F"
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      load-prefer-newer t
      auto-save-default nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      make-backup-files nil
      next-line-add-newlines nil
      require-final-newline t
      show-trailing-whitespace t
      select-enable-clipboard t
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com")

(setq custom-file (expand-file-name ".local/files/custom.el" user-emacs-directory)
      nsm-settings-file (expand-file-name ".local/files/network-security.data" user-emacs-directory))

(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; -- Package manager ----------------------------------------------------------

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (expand-file-name ".local/packages" user-emacs-directory)
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package bind-key :ensure t :demand t)
(use-package diminish :ensure t :demand t)

;; -- Keybindings --------------------------------------------------------------

(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-S-f") 'imenu)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun bind-split-window-and-switch (kbd-seq func)
  "Bind KBD-SEQ to split window FUNC and switch to the newly opened."
  (global-set-key (kbd kbd-seq) (lambda ()
                                  (interactive)
                                  (funcall func)
                                  (other-window 1))))

(bind-split-window-and-switch "C-x 2" 'split-window-vertically)
(bind-split-window-and-switch "C-x 3" 'split-window-horizontally)

(use-package which-key :demand t :ensure t :diminish
  :config (which-key-mode 1))

;; -- Utils --------------------------------------------------------------------

(use-package aggressive-indent :ensure t :diminish
  :config (setq aggressive-indent-excluded-modes
                (append aggressive-indent-excluded-modes '(html-mode
                                                           sql-mode
                                                           web-mode)))
  :hook (prog-mode . global-aggressive-indent-mode))

(use-package anzu :ensure t :diminish
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode)
  :custom-face (anzu-mode-line ((t (:foreground "yellow")))))

(use-package autopair :ensure t :diminish
  :config (autopair-global-mode t))

(use-package editorconfig :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package epa-file :ensure nil :demand t
  :init (setq epa-gpg-program "gpg2")
  :config (epa-file-enable))

(use-package exec-path-from-shell :ensure t
  :when (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package idle-highlight-mode :ensure t :diminish
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
  :init (setq ido-save-directory-list-file (expand-file-name ".local/files/ido.el" user-emacs-directory)
              ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always
              ido-vertical-show-count t))

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package time-stamp :ensure t :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save . time-stamp))

(use-package which-func :ensure t :demand t
  :config (setq which-func-unknown "?")
  :custom-face (which-func ((t (:foreground "DeepSkyBlue"))))
  :hook (prog-mode . which-function-mode))

(use-package whitespace :demand t :ensure nil :diminish
  :init (setq whitespace-line-column 80
              whitespace-style '(tabs tab-mark face trailing))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

;; -- Modules ------------------------------------------------------------------

(let ((modules-file (expand-file-name "modules.el" user-emacs-directory)))
  (when (file-exists-p modules-file)
    (add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
    (load-file modules-file)))

;;; init.el ends here
