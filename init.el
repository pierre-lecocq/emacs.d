;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; File: init.el
;; Creation: Thu Oct 19 12:19:54 2023
;; Time-stamp: <2025-08-26 08:24:20>
;; Copyright (C): 2023 Pierre Lecocq

;;; Commentary:

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(global-font-lock-mode 1)
(global-subword-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(global-eldoc-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(when (and window-system (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      large-file-warning-threshold (* 500 1024 1024)
      debug-on-error t
      help-at-pt-display-when-idle t
      frame-title-format "%b (%m) - %F"
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-buffer-menu t
      initial-major-mode 'fundamental-mode
      load-prefer-newer t
      auto-save-default nil
      create-lockfiles nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      mode-line-format nil
      make-backup-files nil
      next-line-add-newlines nil
      require-final-newline t
      show-trailing-whitespace t
      select-enable-clipboard t
      scroll-conservatively 100
      use-dialog-box nil
      use-short-answers t
      custom-file (expand-file-name ".cache/custom.el" user-emacs-directory)
      project-list-file (expand-file-name ".cache/projects" user-emacs-directory))

;; To avoid "assignment to free variable" warnings
(setq-default auto-revert-verbose nil
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
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "C-c :") 'ns-do-show-character-palette)))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
(global-set-key (kbd "M-<left>") 'other-window)
(global-set-key (kbd "M-<right>") 'other-window)

(advice-add 'split-window-right :after #'(lambda (&rest _) (other-window 1)))
(advice-add 'split-window-below :after #'(lambda (&rest _) (other-window 1)))

(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

(defun display-startup-echo-area-message ()
  "Replace the defaut welcome message in the minibuffer."
  (message "Emacs init time: %s" (emacs-init-time)))

;;; Packages

(require 'package)

(setq package-quickstart t
      package-enable-at-startup nil
      package-user-dir (expand-file-name ".cache/packages" user-emacs-directory)
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t
      use-package-verbose nil)

;;; Theme

(toggle-frame-maximized)
(select-frame-set-input-focus (selected-frame))

(set-frame-font
 (if (>= (display-pixel-width) 2500)
     "Menlo 16"
   "Menlo 14"))

(when (and window-system (eq system-type 'darwin))
  (setq frame-title-format nil
        ns-use-proxy-icon nil
        frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(load-file (expand-file-name "dark-default-theme.el" user-emacs-directory))
(load-theme 'dark-default t)

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified
      simple-modeline-segment-buffer-name
      simple-modeline-segment-position)
     (simple-modeline-segment-vc
      simple-modeline-segment-process
      simple-modeline-segment-misc-info
      simple-modeline-segment-major-mode))))

;;; Look'n'feel

(use-package move-text :defer t
  :config
  (move-text-default-bindings)) ;; Set M-<up> and M-<down> to move text up and down

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package dired :ensure nil
  :custom
  (dired-listing-switches "-alFh"))

;;; Buffers

(setq split-height-threshold 80
      split-width-threshold 125
      display-buffer-alist '(("\\*compilation\\*"
                              (display-buffer-reuse-mode-window display-buffer-below-selected)
                              (dedicated . t)
                              (window-height . fit-window-to-buffer))))

;;; IDO

(use-package ido :ensure nil :demand t
  :custom
  (ido-use-faces t)
  (ido-enable-flex-matching t)
  (ido-auto-merge-work-directories-length -1)
  (ido-use-virtual-buffers t)
  (ido-save-directory-list-file (expand-file-name ".cache/ido.el" user-emacs-directory))
  (ido-max-prospects 100)
  (ido-case-fold t)
  (ido-use-filename-at-point nil)
  (ido-use-url-at-point 'never)
  (ido-create-new-buffer 'always)
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package ido-vertical-mode
  :after ido
  :custom
  (ido-vertical-show-count t)
  (ido-vertical-define-keys 'C-n-C-p)
  :config
  (ido-vertical-mode 1))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(with-eval-after-load 'ido
  (set-face-attribute 'ido-subdir nil :foreground "#6395EE"))

;;; Search

(use-package isearch :ensure nil
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " (%s/%s)")
  :config
  (advice-add 'isearch-update :before 'recenter))

;;; Syntax

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;;; Completion

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-backends '(company-semantic
                      company-capf
                      company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet)
                      company-dabbrev))
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-other-buffers t)
  (completion-styles '(basic flex))
  (completions-max-height 40)
  (completion-ignore-case t)
  (company-files-exclusions '(".git/" ".DS_Store")))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;; IDE

(use-package exec-path-from-shell :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(use-package whitespace :ensure nil
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(tabs tab-mark face trailing))
  (whitespace-action '(cleanup auto-cleanup)))

(use-package which-func :ensure nil
  :hook (prog-mode . which-function-mode)
  :custom
  (which-func-unknown "")
  (which-func-format '(:propertize which-func-current face which-func)))

(use-package time-stamp :ensure nil
  :hook (before-save . time-stamp)
  :custom
  (time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"))

(use-package which-key :defer t
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.5)
  (which-key-max-description-length 200)
  (which-key-add-column-padding 2)
  :config
  (which-key-mode 1))

(use-package imenu-list :defer t
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package string-inflection
  :defer t
  :bind ("C-c C-u" . string-inflection-all-cycle))

;; Git

(use-package magit :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch))

(use-package git-gutter
  :hook (after-init . global-git-gutter-mode)
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  :config
  (set-face-background 'git-gutter:modified "purple")
  (set-face-background 'git-gutter:added "green")
  (set-face-background 'git-gutter:deleted "red")
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows))

;;; Snippets

(use-package yasnippet :defer t
  :custom
  (yas-prompt-functions '(yas-ido-prompt))
  (yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))
  :config
  (yas-global-mode t))

;;; Shell

(use-package vterm :defer t
  :commands (vterm vterm-other-window)
  :init
  (defun spawn-shell(&optional dir)
    "Take an existing shell in DIR or create a new one."
    (interactive)
    (let ((buf (get-buffer "*vterm*"))
          ;; (default-directory (if dir dir (file-name-directory buffer-file-name)))
          (default-directory (if dir dir default-directory)))
      (if buf
          (switch-to-buffer buf)
        (vterm))))
  :bind
  (("C-<return>" . spawn-shell)
   ("C-x p <return>" . (lambda()
                         (interactive)
                         (spawn-shell (project-root (project-current t)))))))

;;; IA

(use-package aider :defer t
  :custom
  (aider-args '("--model" "sonnet" "--no-auto-accept-architect --no-auto-commits"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  :bind
  ("C-c i a" . aider-transient-menu)) ;; wider screen
                                       ;; 'aider-transient-menu-1col ;; narrow screen
                                       ;; 'aider-transient-menu-2cols

;;; LSP

(use-package eglot :ensure nil :defer t
  :hook ((php-mode . eglot-ensure)
         (js2-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider))
  :config
  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(js2-mode "typescript-language-server" "--stdio")) ;; npm i -g typescript-language-server typescript
  (add-to-list 'eglot-server-programs '(typescript-mode "typescript-language-server" "--stdio")) ;; npm i -g typescript-language-server typescript
  (add-to-list 'eglot-server-programs '(go-mode "gopls"))
  :bind
  (("C-c l ?" . eldoc)
   ("C-c l a" . eglot-code-actions)
   ("C-c l r" . eglot-rename)
   ("C-c l e" . flycheck-list-errors)
   ("C-c l f d" . xref-find-definitions)
   ("C-c l f r" . xref-find-references)
   ("C-c l p" . xref-go-back)
   ("C-c l n" . xref-go-forward)))

(use-package flycheck-eglot
  :after (flycheck eglot))

;;; Languages

(use-package dockerfile-mode :defer t
  :mode "Dockerfile\\'")

(use-package terraform-mode :defer t
  :mode "\\.tf\\'")

(use-package json-mode :defer t
  :mode "\\.json\\'")

(use-package yaml-mode :defer t
  :mode "\\.ya?ml\\'")

(use-package php-mode :defer t
  :mode "\\.php\\'")

(use-package make-mode :ensure nil
  :mode ("Makefile.*\\'" . makefile-mode)
  :hook (makefile-mode . (lambda()
                           (whitespace-toggle-options '(tabs tab-mark))
                           (setq indent-tabs-mode t))))

(use-package markdown-mode :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

(use-package dotenv-mode :defer t
  :mode "\\.env\\..*\\'")

(use-package restclient :defer t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(use-package restclient-jq
  :after restclient)

(use-package graphql-mode :defer t
  :hook (graphql-mode . (lambda ()
                          (setq-default tab-width 2))))

(use-package js2-mode :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :interpreter (("node" . js2-mode)
                ("nodejs" . js2-mode))
  :hook (js2-mode . (lambda ()
                      (make-local-variable 'js-indent-level)
                      (setq-default tab-width 2
                                    js2-basic-offset 2)
                      (setq js-indent-level 2)
                      (js2-imenu-extras-mode)
                      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                      ;; (flycheck-select-checker 'javascript-eslint)
                      (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                      (setq-default flycheck-temp-prefix ".flycheck"
                                    flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                                       '(javascript-jshint json-jsonlist))))))

(use-package rjsx-mode :defer t)

(use-package typescript-mode :defer t
  :mode "\\.ts\\'")

(use-package go-mode :defer t
  :mode "\\.go\\'"
  :hook (go-mode . (lambda()
                     (whitespace-toggle-options '(tabs tab-mark))
                     (setq indent-tabs-mode t)
                     (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package company-go
  :after (company go-mode))

;;; init.el ends here.
