;;; init.el --- Init file -*- lexical-binding: t; -*-

;; Time-stamp: <2019-04-22 23:36:17>
;; Copyright (C) 2019 Pierre Lecocq
;; Version: <insert your big int here>
;; Code name: Yet another rewrite

;;; Commentary:

;;; Code:

;; -- Config loading optimizations ---------------------------------------------

(defvar -file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          '(lambda () (setq gc-cons-threshold 16777216
                       gc-cons-percentage 0.1
                       file-name-handler-alist -file-name-handler-alist)))

;; -- Default configuration ----------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (mode) (when (fboundp mode) (funcall mode 1)))
      '(auto-compression-mode
        column-number-mode
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
        tooltip-mode))

(when (or (not window-system) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

(setq user-full-name "Pierre Lecocq"
      debug-on-error t
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
      custom-file (concat (file-name-directory load-file-name) ".local/files/my-custom.el")
      uniquify-buffer-name-style 'forward uniquify-separator "/"
      use-dialog-box nil)

(setq-default fill-column 80)

;; -- Charset ------------------------------------------------------------------

(setq locale-coding-system      'utf-8)
(set-language-environment       'utf-8)
(set-terminal-coding-system     'utf-8)
(set-default-coding-systems     'utf-8)
(set-selection-coding-system    'utf-8)
(prefer-coding-system           'utf-8)
(set-charset-priority           'unicode)

;; -- Keybindings --------------------------------------------------------------

(when (and window-system (eq system-type 'darwin))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)))

(global-set-key [delete]            'delete-char)
(global-set-key (kbd "C-S-f")       'imenu)
(global-set-key (kbd "M-g")         'goto-line)
(global-set-key (kbd "C-c r")       'comment-dwim)
(global-set-key (kbd "C-;")         'other-window)
(global-set-key (kbd "M-;")         'other-frame)
(global-set-key (kbd "M-/")         'hippie-expand)
(global-set-key (kbd "C-x C-b")     'ibuffer)

(defun bind-split-window-and-switch (kbd-seq func)
  "Bind KBD-SEQ to split window FUNC and switch to the newly opened."
  (global-set-key (kbd kbd-seq) (lambda ()
                                  (interactive)
                                  (funcall func)
                                  (other-window 1))))

(bind-split-window-and-switch "C-x 2" 'split-window-vertically)
(bind-split-window-and-switch "C-x 3" 'split-window-horizontally)

;; -- Package manager ----------------------------------------------------------

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (concat (file-name-directory load-file-name) ".local/packages")
      package-archives '(("melpa"           . "https://melpa.org/packages/")
                         ("melpa-stable"    . "https://stable.melpa.org/packages/")
                         ("marmalade"       . "https://marmalade-repo.org/packages/")
                         ("gnu"             . "https://elpa.gnu.org/packages/"))
      package-archive-priorities '(("melpa"         . 15)
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

(setq use-package-always-ensure t)

;; -- Utilities ----------------------------------------------------------------

(use-package diminish)

(use-package autoinsert :demand t
  :init (progn
          (auto-insert-mode 1)
          (auto-insert)))

(use-package autopair :diminish
  :config (autopair-global-mode t))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package epa-file :ensure nil :demand t
  :init (setq epa-gpg-program "gpg2")
  :config (epa-file-enable))

(use-package idle-highlight-mode :diminish
  :hook (prog-mode . idle-highlight-mode))

(use-package rainbow-mode :diminish
  :hook (prog-mode . rainbow-turn-on))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package time-stamp :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save))

(use-package which-func :demand t
  :config (progn
            (which-function-mode 1)
            (set-face-attribute 'which-func nil :foreground "green")))

(use-package which-key :demand t :diminish
  :config (which-key-mode 1))

;; -- Indentation --------------------------------------------------------------

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              electric-indent-inhibit t
              backward-delete-char-untabify-method 'hungry)

(use-package aggressive-indent :diminish
  :config (progn
            (global-aggressive-indent-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'web-mode)))

;; -- Look'n'feel --------------------------------------------------------------

(when (display-graphic-p)
  (toggle-frame-maximized))

(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package darkokai-theme
  :config (progn
            (load-theme 'darkokai t)
            (set-face-background hl-line-face "#303435")
            (set-face-background 'region "DodgerBlue")
            (set-face-foreground 'region "white")
            (set-face-underline 'font-lock-warning-face "red")
            (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|HINT\\)" 1 font-lock-warning-face t))))
  :init (progn
          (setq darkokai-mode-line-padding 1)
          (setq-default left-fringe-width 10
                        right-fringe-width 10)
          (when (and window-system
                     (eq system-type 'darwin)
                     (not (version< emacs-version "26.1")))
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            (add-to-list 'default-frame-alist '(ns-appearance . dark)))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(defun set-font-size (wanted-size)
  "Change font size to WANTED-SIZE."
  (interactive "nFont size: ")
  (let ((wanted-font "Source Code Pro"))
    (if (member wanted-font (font-family-list))
        (set-frame-font (format "%s %d" wanted-font wanted-size) nil t)
      (warn "Font %s not found" wanted-font))))

(set-font-size 12)

(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      scroll-conservatively 101)

(use-package all-the-icons) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :diminish
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

;; -- Whitespace ---------------------------------------------------------------

(use-package whitespace :demand t :ensure nil :diminish
  ;; :config (when (display-graphic-p)
  ;;           (let ((color (face-attribute 'default :background)))
  ;;             (set-face-attribute 'whitespace-space nil
  ;;                                 :background color
  ;;                                 :foreground color)))
  :init (setq whitespace-line-column 80
              whitespace-style '(tabs tab-mark face trailing))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

;; -- Dashboard ----------------------------------------------------------------

(use-package bookmark :demand t :ensure nil
  :init (setq bookmark-sort-flag nil
              bookmark-alist `(("Sources"       (filename . "~/src"))
                               ("Emacs.d" (filename . "~/src/emacs.d"))
                               ("Config.d" (filename . "~/src/config.d"))
                               ("Docker stack" (filename . "~/src/docker-stack"))
                               ("Fotolia" (filename . "~/src/fotolia-web"))
                               ("Hello PHP" (filename . "~/src/hellophp-service"))
                               ("Mass - Castor" (filename . "~/src/mass/beaver-service"))
                               ("Mass - Dojo" (filename . "~/src/mass/dojo-service"))
                               ("Mass - Octopus" (filename . "~/src/mass/octopus-service"))
                               ("Microservice base PHP" (filename . "~/src/microservice-base-php"))
                               ("Microservice lib PHP" (filename . "~/src/microservice-lib-php"))
                               ("PR Report" (filename . "~/src/pr-report"))
                               ("Snitchit" (filename . "~/src/snitchit"))
                               ("StockWeb" (filename . "~/src/stock-web")))))

(use-package dashboard
  :init (setq dashboard-items '((bookmarks . (length bookmark-alist)))
              dashboard-banner-logo-title (format "Emacs %s" emacs-version)
              dashboard-startup-banner 'logo
              dashboard-center-content nil)
  :config (dashboard-setup-startup-hook))

;; -- File tree ----------------------------------------------------------------

(defun neotree-project-dir ()
  "Open NeoTree using the git root of the current project."
  (interactive)
  (let ((project-dir (ffip-project-root)))
    (neotree-dir project-dir)
    (neotree-find (buffer-file-name))))

(use-package neotree
  :after (:all all-the-icons)
  :bind (("C-c f t" . neotree-toggle)
         ("C-c f p" . neotree-project-dir)
         ("C-c f h" . neotree-hidden-file-toggle))
  :init (setq neo-smart-open t
              neo-window-fixed-size nil
              neo-theme (if (display-graphic-p) 'icons 'nerd)))

;; -- Syntax -------------------------------------------------------------------

(use-package flycheck :diminish
  :bind (("<f8>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

;; -- Completion ---------------------------------------------------------------

(use-package company :diminish
  :config (global-company-mode 1)
  :init (setq company-auto-complete nil
              company-tooltip-flip-when-above t
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.5
              company-dabbrev-downcase nil
              company-backends '((company-files
                                  company-keywords
                                  company-capf
                                  company-etags
                                  company-gtags)
                                 (company-abbrev
                                  company-dabbrev
                                  company-dabbrev-code))))

;; -- Navigation ---------------------------------------------------------------

(defvar project-directories-blacklist
  '(".git" "vendor" "packages" "node_modules" "tmp" "log" "html" "doc"))

(use-package flx-ido)
(use-package ido-hacks)
(use-package ido-vertical-mode)
(use-package ido
  :config (progn
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode))
  :init (setq ido-save-directory-list-file (concat (file-name-directory load-file-name) ".local/files/my-ido.el")
              ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always
              ido-vertical-show-count t))

(use-package find-file-in-project
  :bind (("C-S-x C-S-f" . find-file-in-project))
  :init (setq ffip-prefer-ido-mode t
              ffip-prune-patterns  (mapc (lambda (d) (format "*/%s/*" d))
                                         project-directories-blacklist)))

;; -- Search -------------------------------------------------------------------

(use-package anzu :diminish
  :config (global-anzu-mode +1)
  :custom-face (anzu-mode-line ((t (:foreground "yellow")))))

(use-package grep :demand t
  :config (progn
            (mapc (lambda (d)
                    (add-to-list 'grep-find-ignored-directories d))
                  project-directories-blacklist)
            (bind-keys :map occur-mode-map
                       ("n" . occur-next)
                       ("p" . occur-prev)
                       ("o" . occur-mode-display-occurrence)))
  :bind (("C-c s g" . vc-git-grep)
         ("C-c s r" . rgrep)
         ("C-c s o" . occur)))

(use-package dumb-jump
  :config (dumb-jump-mode)
  :bind (("C-c q l" . dumb-jump-quick-look)
         ("C-c q g" . dumb-jump-go)
         ("C-c q b" . dumb-jump-back)
         ("C-c q o" . dumb-jump-other-window)
         ("C-c q p" . dumb-jump-go-prompt)))

;; -- Tags ---------------------------------------------------------------------

(defun refresh-tags ()
  "Refresh tags table of the current project."
  (interactive)
  (let* ((root (locate-dominating-file default-directory ".git"))
         (cmd (format "ctags -f TAGS %s -e -R ."
                      (mapconcat (lambda (d)
                                   (format "--exclude=%s" d))
                                 project-directories-blacklist
                                 " ")))
         (default-directory (if root root ".")))
    (shell-command cmd)
    (visit-tags-table "./TAGS")))

(defun list-tags-for-current-file ()
  "List tags for current file."
  (interactive)
  (let* ((rootdir (expand-file-name (ffip-project-root)))
         (filename (file-relative-name buffer-file-name (expand-file-name rootdir)))
         (tagfile (concat (file-name-as-directory rootdir) "TAGS")))
    (visit-tags-table tagfile)
    (list-tags filename)))

(use-package etags-select
  :init (set-default 'case-fold-search t)
  :bind (("C-c t r" . refresh-tags)
         ("C-c t s" . etags-select-find-tag)))

;; -- Version Control ----------------------------------------------------------

(use-package git-gutter :diminish
  :config (progn
            (set-face-background 'git-gutter:added nil)
            (set-face-foreground 'git-gutter:added "green")
            (set-face-background 'git-gutter:modified nil)
            (set-face-foreground 'git-gutter:modified "yellow")
            (set-face-background 'git-gutter:deleted nil)
            (set-face-foreground 'git-gutter:deleted "red")
            (global-git-gutter-mode +1)))

;; -----------------------------------------------------------------------------
;; -- Languages                                                                -
;; -----------------------------------------------------------------------------

;; -- Emacs Lisp ---------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((emacs-lisp-mode . "Emacs lisp program") nil
               ";;; " (file-name-nondirectory buffer-file-name) " --- " _ " -*- lexical-binding: t; -*-\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
               ";;; Commentary:\n\n"
               ";;; Code:\n\n"
               ";;; " (file-name-nondirectory buffer-file-name) " ends here\n"))

(use-package eros )

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; -- Common Lisp --------------------------------------------------------------

(setq inferior-lisp-program
      (if (eq system-type 'darwin)
          "/usr/local/bin/sbcl"
        "sbcl"))

(add-to-list 'auto-insert-alist
             '((lisp-mode . "Lisp program") nil
               ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
               ";; Time-stamp: <>\n"
               ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"))

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before"))))

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(use-package slime-company :defer t)

(use-package slime
  :mode (("\\.lisp'"    . lisp-mode)
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :config (slime-setup '(slime-company))
  :init (setq slime-contribs '(slime-fancy))
  :hook ((lisp-mode . hook-lisp-mode)
         (inferior-lisp-mode . hook-inferior-lisp-mode)))

;; -- SH -----------------------------------------------------------------------

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

;; -- Makefile -----------------------------------------------------------------

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

;; -- C ------------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((c-mode . "C program") nil
               "/*\n"
               " * File: " (file-name-nondirectory buffer-file-name) "\n"
               " * Time-stamp: <>\n"
               " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               " * Description: " _ "\n"
               " */\n\n"))

(use-package cc-mode
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers
  :init (add-to-list 'company-backends 'company-c-headers))

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

;; -- Go -----------------------------------------------------------------------

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init (progn (exec-path-from-shell-initialize)
               (exec-path-from-shell-copy-env "GOPATH")))

(use-package go-eldoc :defer t)

(use-package go-mode :defer t)

(use-package company-go :defer t)

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook #'hook-go-mode)

;; -- Ruby ---------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((ruby-mode . "Ruby program") nil
               "#!/usr/bin/env ruby\n"
               "# -*- mode: ruby; -*-\n\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               "# Description: " _ "\n\n"))

(use-package inf-ruby :defer t)

(use-package robe :defer t
  :init (push 'company-robe company-backends))

(use-package rubocop :defer t :diminish
  :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools :defer t)

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

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

(defun hook-ruby-mode ()
  "Hook for ruby mode."
  (robe-mode)
  (rubocop-mode)
  (ruby-tools-mode))

(add-hook 'ruby-mode-hook #'hook-ruby-mode)

(use-package yard-mode :diminish
  :hook (ruby-mode . yard-mode))

;; -- Python -------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((python-mode . "Python program") nil
               "#!/usr/bin/env python\n\n"))

(use-package elpy :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :commands elpy-enable
  :config  (progn
             (setq python-indent-offset 4)
             (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
             (when (fboundp 'flycheck-mode)
               (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))))

;; -- HTTP ---------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((restclient-mode . "REST client") nil
               "# -*- restclient -*-\n\n"))

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

;; -- PHP ----------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((php-mode . "PHP script") nil
               "<?php\n\n"))

(use-package php-extras :defer t)

(use-package php-mode
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)))

(defun php-auto-lint ()
  "Run PHP autolint."
  (interactive)
  (message (shell-command-to-string (concat "php -l " buffer-file-name))))

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

;; -- JS ------------------------------------------------------------------------

(use-package js2-refactor :defer t)
(use-package xref-js2 :defer t) ;; requires installing `ag'

(when (executable-find "tern") ;; `sudo npm install -g tern'
  (use-package company-tern
    :config (progn
              (add-to-list 'company-backends 'company-tern)
              ;; Disable completion keybindings, as we use xref-js2 instead
              (unbind-key "M-." tern-mode-keymap)
              (unbind-key "M-," tern-mode-keymap))))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))

(defun hook-js2-mode ()
  "Hook for js2 mode."
  (tern-mode)
  (js2-imenu-extras-mode)
  (js2-refactor-mode)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t))

(add-hook 'js2-mode-hook #'hook-js2-mode)

;; -- SQL ----------------------------------------------------------------------

(use-package sqlup-mode :defer t)

(use-package sql-indent :defer t)

(defun hook-sql-mode ()
  "Hook for SQL mode."
  (sqlup-mode t)
  (toggle-truncate-lines t))

(add-hook 'sql-mode-hook #'hook-sql-mode)
(add-hook 'sql-interactive-mode-hook #'hook-sql-mode) ;; When connected to a server within Emacs

;; -- Web ----------------------------------------------------------------------

(use-package htmlize :defer t)

(use-package scss-mode :defer t)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

;; -- Text ---------------------------------------------------------------------

(add-to-list 'auto-insert-alist
             '((org-mode . "Org mode") nil
               "#+TITLE: " _ "\n"
               "#+AUTHOR: " (user-full-name) "\n"
               "#+DATE: " (current-time-string) "\n"
               "#+STARTUP: showall\n\n"))

(use-package dockerfile-mode :defer t)

(use-package terraform-mode :defer t)

(use-package json-mode :defer t)

(use-package markdown-mode :defer t)

(use-package toml-mode :defer t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;;; init.el ends here
