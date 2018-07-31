;;; init.el --- Emacs configuration

;; Time-stamp: <2018-07-31 10:36:45>
;; Copyright (C) 2017 Pierre Lecocq
;; Version: <insert your bigint here>

;;; Commentary:

;; Code name: "Yet another rewrite"

;;; Code:

(defvar that-directory (file-name-directory load-file-name))

;;;;;;;;;;;;;;;;;;;;;
;; Package manager ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (concat that-directory "packages")
      package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("org"           . 20)
                                   ("melpa"         . 15)
                                   ("melpa-stable"  . 12)
                                   ("marmalade"     . 10)
                                   ("gnu"           . 5)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;;;;;;;;;;;;;;
;; Defaults ;;
;;;;;;;;;;;;;;

;; Laziness

(fset 'yes-or-no-p 'y-or-n-p)

;; Deactivate modes

(mapc (lambda (mode) (funcall mode 1))
      '(auto-compression-mode
        column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        ido-mode
        line-number-mode
        show-paren-mode
        subword-mode
        transient-mark-mode))

;; Activate modes

(mapc (lambda (mode) (funcall mode -1))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode
        tooltip-mode))

;; Default variables

(setq user-full-name "Pierre Lecocq"
      debug-on-error t
      gc-cons-threshold 100000000
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      load-prefer-newer t
      sentence-end-double-space nil
      frame-title-format "%b (%m) - %F"
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      inhibit-startup-message t
      inhibit-splash-screen t
      case-fold-search t
      require-final-newline t
      next-line-add-newlines nil
      select-enable-clipboard t
      show-trailing-whitespace t
      uniquify-buffer-name-style 'forward uniquify-separator "/")

;; Indentation

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil)

;; Locale

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;
;; Behaviour ;;
;;;;;;;;;;;;;;;

;; Auto insert

(require 'autoinsert)

(auto-insert-mode 1)
(auto-insert)

;; Time-stamp

(use-package time-stamp :demand t :ensure nil
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"))

;; Bookmarks

(use-package bookmark :demand t :ensure nil
  :init (setq bookmark-sort-flag nil
              bookmark-alist '(("home"     (filename . "~/"))
                               ("emacs.d"  (filename . "~/src/emacs.d/"))
                               ("sources"  (filename . "~/src/")))))

;; Recent files

(use-package recentf :demand t :ensure nil
  :init (setq recentf-auto-cleanup 'never
              recentf-max-menu-items 20)
  :config (progn (add-to-list 'recentf-exclude package-user-dir)
                 (recentf-mode 1)))

;; Editor config

(use-package editorconfig :ensure t
  :config (editorconfig-mode 1))

;; Local data files

(setq host-file (concat that-directory "host.el") ;; placed at root level since it is host-related file written by the user, not by a package
      custom-file (concat that-directory "local/my-custom.el")
      abbrev-file-name (concat that-directory "local/my-abbrev.el")
      bookmark-default-file (concat that-directory "local/my-bookmarks.el")
      nsm-settings-file (concat that-directory "local/my-nsm-settings.el")
      recentf-save-file (concat that-directory "local/my-recentf.el")
      ido-save-directory-list-file (concat that-directory "local/my-ido.el")
      url-configuration-directory (concat that-directory "local/url")
      tramp-persistency-file-name (concat that-directory "local/my-tramp.el")
      eshell-directory-name (file-name-as-directory "~/.eshell"))

;;;;;;;;;;;;;;;
;; Interface ;;
;;;;;;;;;;;;;;;

;; Full screen

(when (display-graphic-p)
  (toggle-frame-maximized))

;; Theme

(use-package darkokai-theme :ensure t
  :config (load-theme 'darkokai t)
  :init (progn (setq darkokai-mode-line-padding 1)
               (setq-default left-fringe-width 10
                             right-fringe-width 10)
               (when (and window-system
                          (eq system-type 'darwin)
                          (not (version< emacs-version "26.1")))
                 (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
                 (add-to-list 'default-frame-alist '(ns-appearance . dark)))))

(set-face-background 'region "DodgerBlue")
(set-face-foreground 'region "white")

;; Diminish

(use-package diminish :ensure t)

;; Autopair

(use-package autopair :ensure t :diminish autopair-mode
  :config (autopair-global-mode t))

;; Anzu

(use-package anzu :ensure t :diminish anzu-mode
  :config (progn (global-anzu-mode +1)
                 (set-face-attribute 'anzu-mode-line nil :foreground "grey13")))

;; Completion

(use-package company :ensure t :diminish company-mode
  :config (global-company-mode 1)
  :init (setq company-auto-complete nil
              company-tooltip-flip-when-above t
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.5))

;; Git gutter

(use-package git-gutter :ensure t :diminish git-gutter-mode
  :config (progn (set-face-background 'git-gutter:added nil)
                 (set-face-foreground 'git-gutter:added "green")
                 (set-face-background 'git-gutter:modified nil)
                 (set-face-foreground 'git-gutter:modified "yellow")
                 (set-face-background 'git-gutter:deleted nil)
                 (set-face-foreground 'git-gutter:deleted "red")
                 (global-git-gutter-mode +1)))

;; (use-package magit :ensure t)

;; Idle highlight

(use-package idle-highlight-mode :ensure t :diminish idle-highlight-mode)

;; Rainbow

(use-package rainbow-mode :ensure t :diminish rainbow-mode)

;; Rainbow delimiters

(use-package rainbow-delimiters :ensure t)

;; Which func

(use-package which-func :demand t :ensure nil
  :config (progn (which-function-mode 1)
                 (set-face-attribute 'which-func nil :foreground "red")))

;; Whitespace

(use-package whitespace :demand t :ensure nil :diminish global-whitespace-mode
  :config (progn (global-whitespace-mode)
                 (let ((color (face-attribute 'default :background)))
                   (set-face-attribute 'whitespace-space nil
                                       :background color
                                       :foreground color)))
  :init (progn (setq whitespace-line-column 80
                     whitespace-style '(spaces space-mark tabs tab-mark face); lines-tail)
                     whitespace-global-modes '(not org-mode web-mode yaml-mode json-mode markdown-mode))))

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

;; Ag

(use-package ag :ensure t
  :bind (("C-S-s" . ag)))

;; Ido family

(use-package flx-ido :ensure t)

(use-package ido-hacks :ensure t)

(use-package ido-vertical-mode :ensure t)

(use-package ido :ensure t
  :config (progn
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode))
  :init (setq ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always))

;; Find file in project

(use-package find-file-in-project :ensure t
  :bind (("C-S-x C-S-f" . find-file-in-project))
  :init (setq ffip-prefer-ido-mode t
              ffip-prune-patterns '("*/.git/*"
                                    "*/packages/*"
                                    "*/vendor/*"
                                    "*/node_modules/*")))

;;;;;;;;;;;;;
;; Project ;;
;;;;;;;;;;;;;

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package neotree :ensure t
  :bind (("C-c t" . neotree-toggle)
         ("C-c p" . neotree-project-dir)
         ("C-c h" . neotree-hidden-file-toggle))
  :init (setq neo-smart-open t
              neo-window-fixed-size nil
              neo-theme (if (display-graphic-p) 'icons 'nerd)))

;;;;;;;;;;;;;;
;; Snippets ;;
;;;;;;;;;;;;;;

;; Yasnippets

(let ((snippets-dir (concat that-directory "snippets")))
  (use-package yasnippet :ensure t
    :if (file-accessible-directory-p snippets-dir)
    :diminish yas-minor-mode
    :config (yas-global-mode 1)
    :init (setq yas-snippet-dirs '(snippets-dir))))

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

(use-package flycheck :ensure t :diminish flycheck-mode
  :bind (("<f8>" . flycheck-list-errors)))

;;;;;;;;;;;;
;; Eshell ;;
;;;;;;;;;;;;

;; Functions

(defun eshell-other-frame ()
  "Open eshell in another maximized frame."
  (interactive)
  (with-selected-frame (make-frame)
    (eshell)
    (when (display-graphic-p)
      (toggle-frame-maximized))))

(defun eshell-window-vertical ()
  "Open eshell in another vertically splitted window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (eshell (string-to-number (format-time-string "%s"))))

(defun eshell-window-horizontal ()
  "Open eshell in another horizontally splitted window."
  (interactive)
  (split-window-bottom)
  (other-window 1)
  (eshell (string-to-number (format-time-string "%s"))))

;; Packages

(use-package eshell :demand t :ensure nil
  :bind (("<f9>" . eshell-other-frame)))

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(when (and window-system
           (eq system-type 'darwin))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs))

(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "<f10>") 'bookmark-bmenu-list)
(global-set-key (kbd "<f11>") 'recentf-open-files)
(global-set-key (kbd "C-S-f") 'imenu)
(global-set-key (kbd "M-g") 'goto-line)

;; (global-set-key (kbd "C-c r") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'comment-dwim)

(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "M-;") 'other-frame)

(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (split-window-vertically)
                  (other-window 1)))

(global-set-key (kbd "C-x 3")
                (lambda ()
                  (interactive)
                  (split-window-horizontally)
                  (other-window 1)))

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(defun hook-before-save ()
  "Hook before save."
  (time-stamp)
  (whitespace-cleanup)
  (delete-trailing-whitespace))

(add-hook 'before-save-hook #'hook-before-save)

(defun hook-after-save ()
  "Hook after save."
  ;; Auto byte compile init.el
  (let* ((raw-file (file-truename user-init-file))
         (compiled-file (concat raw-file "c")))
    (when (string= buffer-file-name raw-file)
      (when (file-exists-p compiled-file)
        (delete-file compiled-file))
      (byte-compile-file raw-file))))

;; (add-hook 'after-save-hook #'hook-after-save)

(defun hook-prog-mode ()
  "Hook for prog mode."
  (flycheck-mode 1)
  (idle-highlight-mode t)
  (rainbow-delimiters-mode)
  (rainbow-turn-on)
  (set-face-underline 'font-lock-warning-face "red")
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'hook-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Lang :: Emacs Lisp ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages

(use-package eros :ensure t)

;; Auto insert

(add-to-list 'auto-insert-alist
             '((emacs-lisp-mode . "Emacs lisp program") nil
               ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
               ";;; Commentary:\n\n"
               ";;; Code:\n\n"
               ";;; " (file-name-nondirectory buffer-file-name) " ends here\n"))

;; Hook

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;;;;;;;;;;;;;;;;;;
;; Lang :: Lisp ;;
;;;;;;;;;;;;;;;;;;

;; Packages

(use-package slime-company :ensure t)

(use-package slime :ensure t
  :mode (("\\.lisp'"    . lisp-mode) ;; enables lazy loading in place of :defer
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :init (setq inferior-lisp-program
              (if (eq system-type 'darwin)
                  "/usr/local/bin/sbcl --noinform"
                "sbcl --noinform"))
  :config (slime-setup '(slime-company)))

;; Auto insert

(add-to-list 'auto-insert-alist
             '((lisp-mode . "Lisp program") nil
               ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"))

;; Hook

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (message "LISP MODE hook!")
  (global-prettify-symbols-mode 1)
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before"))))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(add-hook 'inferior-lisp-mode-hook #'hook-inferior-lisp-mode)

;;;;;;;;;;;;;;;
;; Lang :: C ;;
;;;;;;;;;;;;;;;

;; Packages

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (add-to-list 'company-backends 'company-c-headers))

;; Auto insert

(add-to-list 'auto-insert-alist
             '((c-mode . "C program") nil
               "/*\n"
               " * File: " (file-name-nondirectory buffer-file-name) "\n"
               " * Time-stamp: <>\n"
               " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               " * Description: " _ "\n"
               " */\n\n"))

;; Hook

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

;;;;;;;;;;;;;;;;
;; Lang :: Go ;;
;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :init (progn (exec-path-from-shell-initialize)
               (exec-path-from-shell-copy-env "GOPATH")))

(use-package go-mode :ensure t)

(use-package go-eldoc :ensure t)

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (setq whitespace-style '(spaces space-mark face)))

(add-hook 'go-mode-hook #'hook-go-mode)

;;;;;;;;;;;;;;;;;;
;; Lang :: Ruby ;;
;;;;;;;;;;;;;;;;;;

;; Packages

(use-package ruby-mode :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(use-package inf-ruby :ensure t)

(use-package robe :ensure t
  :init (push 'company-robe company-backends))

(use-package rubocop :ensure t)

(use-package ruby-tools :ensure t)

;; Auto insert

(add-to-list 'auto-insert-alist
             '((ruby-mode . "Ruby program") nil
               "#!/usr/bin/env ruby\n"
               "# -*- mode: ruby; -*-\n\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               "# Description: " _ "\n\n"))

;; Functions

(defun ruby-transform-hash-keys (regexp-string match-string)
  "Transform hash keys from with REGEXP-STRING and MATCH-STRING."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (while (re-search-forward regexp-string nil t)
          (replace-match match-string)))))

(defun ruby-hash-symbols-to-strings ()
  "Transform hash keys from symbols to strings in a given region."
  (interactive)
  (ruby-transform-hash-keys ":\\([a-zA-Z0-9_-]+\\)" "'\\1'"))

(defun ruby-hash-strings-to-symbols ()
  "Transform hash keys from strings to symbols in a given region."
  (interactive)
  (ruby-transform-hash-keys "'\\([a-zA-Z0-9_-]+\\)'" ":\\1"))

;; Hook

(defun hook-ruby-mode ()
  "Hook for ruby mode."
  (robe-mode)
  (rubocop-mode)
  (ruby-tools-mode))

(add-hook 'ruby-mode-hook #'hook-ruby-mode)

;;;;;;;;;;;;;;;;
;; Lang :: SH ;;
;;;;;;;;;;;;;;;;

;; Auto insert

(add-to-list 'auto-insert-alist
             '((sh-mode . "Shell script") nil
               "#!/usr/bin/env bash\n"
               "# -*- mode: sh; -*-\n\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               "# Description: " _ "\n\n"
               "set -o errexit\n\n"
               "[ -z $BASH ] && (echo \"Not in a BASH sub process\"; exit 1)\n"
               "BASE_DIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)\n\n"))

;;;;;;;;;;;;;;;;;
;; Lang :: PHP ;;
;;;;;;;;;;;;;;;;;

;; Auto insert

(add-to-list 'auto-insert-alist
             '((php-mode . "PHP script") nil
               "<?php\n\n"))

;; Packages

(use-package php-mode :ensure t
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)))

(use-package php-extras :ensure t)

;; Hook

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Lang :: Jajascript ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages

(use-package js2-mode :ensure t
  :mode "\\.js\\'")

(use-package js2-refactor :ensure t)

;; Hook

(defun hook-js2-mode ()
  "Hook for js2 mode."
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t))

(add-hook 'js2-mode-hook #'hook-js2-mode)

;;;;;;;;;;;;;;;;;
;; Lang :: Web ;;
;;;;;;;;;;;;;;;;;

;; Packages

(use-package htmlize :ensure t)

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(use-package scss-mode :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

;; Auto insert

(add-to-list 'auto-insert-alist
             '((restclient-mode . "REST client") nil
               "# -*- restclient -*-\n\n"))

;;;;;;;;;;;;;;;;;
;; Lang :: SQL ;;
;;;;;;;;;;;;;;;;;

;; Packages

(use-package sqlup-mode :ensure t)

(use-package sql-indent :ensure t)

;; Hook

(defun hook-sql-mode ()
  "Hook for SQL mode."
  (sqlup-mode t)
  (toggle-truncate-lines t))

(add-hook 'sql-mode-hook #'hook-sql-mode)
(add-hook 'sql-interactive-mode-hook #'hook-sql-mode) ;; When connected to a server within Emacs

;;;;;;;;;;;;;;;;;;
;; Lang :: Text ;;
;;;;;;;;;;;;;;;;;;

;; Packages

(use-package dockerfile-mode :ensure t)

(use-package terraform-mode :ensure t)

(use-package json-mode :ensure t)

(use-package org :ensure t :defer t :pin "org"
  :init (setq org-hide-leading-stars t
              org-hide-emphasis-markers t
              org-fontify-done-headline t
              org-src-fontify-natively t))

(use-package markdown-mode :ensure t)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

;; Auto insert

(add-to-list 'auto-insert-alist
             '((org-mode . "Org mode") nil
               "#+TITLE: " _ "\n"
               "#+AUTHOR: " (user-full-name) "\n"
               "#+DATE: " (current-time-string) "\n"
               "#+STARTUP: showall\n\n"))

;; Hook

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load host specific file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p host-file)
  (load host-file))

;;; init.el ends here
