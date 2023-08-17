;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Time-stamp: <2023-08-17 17:09:37>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; Please refer to the cheatsheet.md file to install dependencies

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(global-subword-mode 1)
(line-number-mode 1)
(show-paren-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(when (and window-system (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      large-file-warning-threshold (* 500 1024 1024)
      debug-on-error t
      frame-title-format "%b (%m) - %F"
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      auto-revert-verbose nil
      load-prefer-newer t
      auto-save-default nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      make-backup-files nil
      next-line-add-newlines nil
      require-final-newline t
      show-trailing-whitespace t
      select-enable-clipboard t
      scroll-conservatively 100
      startup-redirect-eln-cache (expand-file-name ".cache/eln-cache" user-emacs-directory)
      custom-file (expand-file-name ".cache/custom.el" user-emacs-directory)
      nsm-settings-file (expand-file-name ".cache/network-security.data" user-emacs-directory))

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

(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "<f11>") 'global-display-line-numbers-mode)

(advice-add 'split-window-right :after #'(lambda (&rest _) (other-window 1)))
(advice-add 'split-window-below :after #'(lambda (&rest _) (other-window 1)))

;;; Packages

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (expand-file-name ".cache/packages" user-emacs-directory)
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; Theme

(toggle-frame-maximized)

(when (and window-system (eq system-type 'darwin))
  (setq frame-title-format nil
        ns-use-proxy-icon nil
        frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(use-package ef-themes :ensure t
  :init (setq ef-themes-to-toggle '(ef-light ef-maris-dark))
  :config (load-theme 'ef-light t)
  :bind ("<f9>" . 'ef-themes-toggle))

(use-package simple-modeline :ensure t
  :config (simple-modeline-mode)
  :custom (simple-modeline-segments
           '((simple-modeline-segment-modified
              simple-modeline-segment-buffer-name
              simple-modeline-segment-position)
             (simple-modeline-segment-vc
              simple-modeline-segment-process
              simple-modeline-segment-misc-info
              simple-modeline-segment-major-mode))))

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; -----------------------------------------------------------------------------

;;; IDE

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

(use-package imenu :ensure t)

(use-package imenu-list :ensure t
  :init (setq imenu-list-size 0.15)
  :bind ("<f12>" . 'imenu-list-smart-toggle))

(use-package string-inflection :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle))

(use-package time-stamp :ensure t :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save . time-stamp))

(use-package which-func :ensure t :demand t
  :init (setq which-func-unknown ""
              which-func-format '(:propertize which-func-current face which-func))
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

(use-package git-gutter+ :ensure t
  :hook (prog-mode . global-git-gutter+-mode)
  :bind (("C-c g n" . git-gutter+-next-hunk)
         ("C-c g p" . git-gutter+-previous-hunk)
         ("C-c g s" . git-gutter+-show-hunk))
  :config (setq git-gutter+-modified-sign "~ "
                git-gutter+-added-sign "+ "
                git-gutter+-deleted-sign "- "
                transient-history-file (expand-file-name ".cache/transient-history.el" user-emacs-directory)
                transient-levels-file (expand-file-name ".cache/transient-levels.el" user-emacs-directory)
                transient-values-file (expand-file-name ".cache/transient-values.el" user-emacs-directory))
  :custom-face
  (git-gutter+-modified ((t (:foreground "orange")))))

(use-package projectile :ensure t
  :init (setq ;; projectile-project-search-path '("~/src/")
              projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory)
              projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package ibuffer-projectile :ensure t
  :after projectile
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-projectile-set-filter-groups))))

(use-package isearch :ensure nil :demand t
  :config (setq search-highlight t
                search-whitespace-regexp ".*?"
                isearch-lax-whitespace t
                isearch-regexp-lax-whitespace nil
                isearch-lazy-highlight t
                isearch-lazy-count t
                lazy-count-prefix-format nil
                lazy-count-suffix-format " (%s/%s)")
  (advice-add 'isearch-update :before 'recenter))

;;; Complete

(use-package company :ensure t
  :config (global-company-mode)
  :init (setq company-auto-complete nil
              company-minimum-prefix-length 1
              company-tooltip-limit 20
              company-idle-delay 0.0
              company-dabbrev-downcase nil))

(use-package company-quickhelp :ensure t
  :config (company-quickhelp-mode))

;;; Correct

(use-package flycheck :ensure t
  :hook (prog-mode . flycheck-mode))

;;; Terminal

(defvar terminal-buffer-name "vterm")

(defun toggle-terminal ()
  "Toggle terminal."
  (interactive)
  (if (string= (buffer-name) terminal-buffer-name)
      (switch-to-buffer (other-buffer))
    (if (get-buffer terminal-buffer-name)
        (switch-to-buffer terminal-buffer-name)
      (vterm terminal-buffer-name))))

(use-package vterm :ensure t
  :init (setq confirm-kill-processes nil)
  :bind ("<C-return>" . toggle-terminal))

;;; LSP

(use-package eglot :ensure t
  :config (progn
            (add-to-list 'eglot-server-programs '(js2-mode . ("/usr/local/bin/typescript-language-server" "--stdio")))
            (add-to-list 'eglot-server-programs '(php-mode . ("/usr/local/bin/intelephense" "--stdio"))))
  :hook ((js2-mode . eglot-ensure)
         (php-mode . eglot-ensure)))

;;; Autoinsert

(use-package autoinsert
  :init (progn
          (setq auto-insert-query nil)
          (add-hook 'find-file-hook 'auto-insert)
          (auto-insert-mode 1)))

;; -----------------------------------------------------------------------------

;;; Elisp

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(define-auto-insert "\\.el$"
  (lambda ()
    (let ((filename (file-name-nondirectory buffer-file-name))
          (description (read-string "Enter description: ")))
      (insert ";;; " filename " --- " description " -*- lexical-binding: t; -*-\n\n"
              ";; File: " filename "\n"
              ";; Creation: " (current-time-string) "\n"
              ";; Time-stamp: <>\n"
              ";; Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
              ";;; Commentary:\n\n"
              ";;; Code:\n\n\n\n"
              ";;; " filename " ends here.\n")
      (forward-line -3))))

;;; Makefile

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs tab-mark))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)
(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))

;;; Text

(use-package dockerfile-mode :ensure t)

(use-package terraform-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ("C-c C-e" . markdown-export-and-preview)
  :init (setq markdown-enable-wiki-links t
              markdown-italic-underscore t
              markdown-asymmetric-header t
              markdown-make-gfm-checkboxes-buttons t
              markdown-gfm-uppercase-checkbox t
              markdown-fontify-code-blocks-natively t))

(use-package markdown-toc :ensure t
  :after markdown)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(use-package xit-mode :ensure t)

;;; Web

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config (flycheck-add-mode 'javascript-eslint 'web-mode)
  :init (setq web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2))

(use-package emmet-mode :ensure t
  :bind (:map php-mode-map
              ("M-." . xref-find-definitions)
              ("M-?" . xref-find-references)))

(add-hook 'text-mode-hook #'hook-text-mode)

;;; Python

(use-package python-mode :ensure t)

(define-auto-insert "\\.py$"
  (lambda ()
    (let ((filename (file-name-nondirectory buffer-file-name)))
      (insert "#!/usr/bin/env python3\n\n"
              "# File: " filename "\n"
              "# Creation: " (current-time-string) "\n"
              "# Time-stamp: <>\n"
              "# Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
      (goto-char (point-max)))))

;;; Golang

(use-package go-mode :ensure t
  :config (add-hook 'before-save-hook 'gofmt-before-save)
  :init (setq whitespace-style '(face trailing)))

(define-auto-insert "\\.go$"
  (lambda ()
    (let ((filename (file-name-nondirectory buffer-file-name)))
      (insert "// File: " filename "\n"
              "// Creation: " (current-time-string) "\n"
              "// Time-stamp: <>\n"
              "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
      (goto-char (point-max)))))

;;; PHP

(use-package php-mode :ensure t)

(define-auto-insert "\\.php$"
  (lambda ()
    (let ((filename (file-name-nondirectory buffer-file-name)))
      (insert "// File: " filename "\n"
              "// Creation: " (current-time-string) "\n"
              "// Time-stamp: <>\n"
              "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
      (goto-char (point-max)))))

;;; JS

(use-package js2-mode :ensure t
  :mode (("\\.js$" . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-default tab-width 2)
                      (setq js-indent-level 2
                            js2-basic-offset 2)
                      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
                      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
                      (js2-imenu-extras-mode)
                      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                      (flycheck-select-checker 'javascript-eslint)
                      (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                      (setq-default flycheck-temp-prefix ".flycheck"
                                    flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                                       '(javascript-jshint json-jsonlist)))))
  :bind ((:map js2-mode-map
               ("M-." . xref-find-definitions)
               ("M-?" . xref-find-references))))

(use-package rjsx-mode :ensure t
  :after js2-mode)

(use-package typescript-mode :ensure t)

(define-auto-insert "\\.[j|t]sx?$"
  (lambda ()
    (let ((filename (file-name-nondirectory buffer-file-name)))
      (insert "// File: " filename "\n"
              "// Creation: " (current-time-string) "\n"
              "// Time-stamp: <>\n"
              "// Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
      (goto-char (point-max)))))

;;; init.el ends here
