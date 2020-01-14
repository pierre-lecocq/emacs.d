;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Time-stamp: <2020-01-14 08:50:19>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)

(mapc (lambda (mode) (when (fboundp mode) (funcall mode 1)))
      '(column-number-mode
        global-auto-revert-mode
        global-font-lock-mode
        ;; global-hl-line-mode
        global-subword-mode
        line-number-mode
        show-paren-mode))

(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode
        tooltip-mode
        menu-bar-mode))

(setq gc-cons-threshold 50000000 ; 50MB
      large-file-warning-threshold 100000000 ; 100MB
      debug-on-error t
      frame-title-format "%b (%m) - %F"
      auto-revert-verbose nil
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
      scroll-conservatively 100
      ;; ffap-machine-p-known 'reject
      ;; ffap-machine-p-unknown 'reject
      user-full-name "Pierre Lecocq"
      user-mail-address "pierre.lecocq@gmail.com"
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

;; -- Keybindings --------------------------------------------------------------

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
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-o") 'other-window)

(advice-add 'split-window-right :after #'(lambda (&rest _) (other-window 1)))
(advice-add 'split-window-below :after #'(lambda (&rest _) (other-window 1)))

;; -- Package manager ----------------------------------------------------------

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (expand-file-name ".cache/packages" user-emacs-directory)
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

;; -- Theme --------------------------------------------------------------------

(toggle-frame-maximized)

(when (and window-system
           (eq system-type 'darwin)
           (not (version< emacs-version "26.1")))
  (setq frame-title-format nil
        ns-use-proxy-icon nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(defvar themes-candidates '(darkokai modus-operandi))

(defun switch-theme ()
  "Switch theme."
  (interactive)
  (let* ((cur (pop themes-candidates))
         (next (car themes-candidates)))
    (disable-theme cur)
    (load-theme next t)
    (setq themes-candidates (append themes-candidates `(,cur)))))

(global-set-key (kbd "C-c v t") 'switch-theme)

(use-package darkokai-theme :ensure t
  :config (load-theme 'darkokai t))

(use-package modus-operandi-theme :ensure t)

(use-package minions :ensure t
  :init (setq minions-mode-line-lighter "..."
              minions-mode-line-delimiters '("" . ""))
  :config (minions-mode 1))

;; -- Utils --------------------------------------------------------------------

(use-package anzu :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode))

(use-package autopair :ensure t
  :config (autopair-global-mode t))

(use-package dired :ensure nil :demand t
  ;; :hook (dired-mode . (lambda () (text-scale-adjust 1.2)))
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

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq ibuffer-idle-timer (run-with-idle-timer 120 t 'ibuffer))))

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
  :config (which-key-mode 1))

(use-package whitespace :demand t :ensure nil
  :init (setq whitespace-line-column 80
              whitespace-style '(tabs tab-mark face trailing))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

;; -- Persistent notes ---------------------------------------------------------

(defun open-persistent-notes-buffer ()
  "Open persistent notes buffer."
  (interactive)
  (let ((buf (get-buffer-create "*notes*")))
    (switch-to-buffer buf)
    (markdown-mode)
    (ignore-errors
      (persistent-scratch-restore))))

(defun persistent-notes-buffer-p ()
  "Return non-nil if the current buffer's name is *notes*."
  (string= (buffer-name) "*notes*"))

(use-package persistent-scratch :ensure t
  :custom ((persistent-scratch-save-file (expand-file-name ".cache/persistent-notes" user-emacs-directory))
           (persistent-scratch-scratch-buffer-p-function #'persistent-notes-buffer-p))
  :config (add-hook 'kill-emacs-hook #'persistent-scratch-save)
  :bind ("C-c n" . open-persistent-notes-buffer))

;; -- Multicursors -------------------------------------------------------------

(use-package multiple-cursors :ensure t
  :init (setq mc/list-file (expand-file-name ".cache/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;; -- Imenu --------------------------------------------------------------------

(use-package imenu :ensure t
  :bind ("C-c i m" . imenu))

(use-package imenu-list :ensure t
  :config (imenu-list-minor-mode)
  :bind ("C-c i l" . imenu-list))

;; -- Completion ---------------------------------------------------------------

(use-package company :ensure t
  :config (global-company-mode)
  :init (setq company-auto-complete nil
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.25
              company-dabbrev-downcase nil
              ;; company-backends
              ;; '((company-dabbrev-code company-gtags company-etags company-keywords)
              ;;   company-files company-capf company-dabbrev)
              ))

(use-package company-quickhelp :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-shell :ensure t
  :after company)

;; -- Syntax -------------------------------------------------------------------

(use-package flycheck :ensure t
  :bind (("C-c s e" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

;; -- Git ----------------------------------------------------------------------

(use-package git-gutter :ensure t
  :config (global-git-gutter-mode +1)
  :custom ((git-gutter:added-sign " ")
           (git-gutter:modified-sign " ")
           (git-gutter:deleted-sign " ")))

(use-package git-messenger :ensure t
  :init (setq git-messenger:show-detail t)
  :bind (("C-c g m" . git-messenger:popup-message)
         ("C-c g v" . git-messenger:popup-show-verbose)))

(use-package magit :ensure t
  :init (setq transient-history-file (expand-file-name ".cache/transient-history.el" user-emacs-directory)
              transient-levels-file (expand-file-name ".cache/transient-levels.el" user-emacs-directory)
              transient-values-file (expand-file-name ".cache/transient-values.el" user-emacs-directory))
  :bind (("C-c g s" . magit-status)))

;; -- Project ------------------------------------------------------------------

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package ibuffer-projectile :ensure t
  :after projectile
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-projectile-set-filter-groups))))

;; -- Snippets -----------------------------------------------------------------

(use-package yasnippet :ensure t
  :after company
  :config (progn
            (yas-global-mode 1)
            (add-to-list 'company-backends 'company-yasnippet)))

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

;; -- LSP ----------------------------------------------------------------------

(use-package lsp-mode :ensure t
  ;; :hook ((js2-mode . lsp)
  ;;        (php-mode . lsp))
  :commands lsp
  :config (setq lsp-prefer-flymake nil
                lsp-session-file (expand-file-name ".cache/lsp-session.el" user-emacs-directory)))

(use-package lsp-ui :ensure t
  :requires lsp-mode flycheck
  :config (setq lsp-ui-flycheck-enable t
                lsp-ui-flycheck-list-position 'right
                lsp-ui-flycheck-live-reporting t
                lsp-ui-doc-enable t
                lsp-ui-doc-use-childframe t
                lsp-ui-doc-position 'top
                lsp-ui-doc-include-signature t
                lsp-ui-peek-enable t
                lsp-ui-peek-list-width 60
                lsp-ui-peek-peek-height 25
                lsp-ui-sideline-enable nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp :ensure t
  :after (company lsp)
  :config (add-to-list 'company-backends 'company-lsp))

;; -- Lang: Makefile -----------------------------------------------------------

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

;; -- Lang: C ------------------------------------------------------------------

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (progn
          (add-to-list 'company-backends 'company-cmake)
          (add-to-list 'company-backends 'company-c-headers)))

(defun hook-c-mode ()
  "Hook for C mode."
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

;; -- Lang: elisp --------------------------------------------------------------

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; -- Lang: c ------------------------------------------------------------------

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :after company
  :init (progn
          (add-to-list 'company-backends 'company-cmake)
          (add-to-list 'company-backends 'company-c-headers)))

(defun hook-c-mode ()
  "Hook for C mode."
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook #'hook-c-mode)

;; -- Lang: Common Lisp --------------------------------------------------------

(use-package slime-company :ensure t :defer t
  :after company)

(use-package slime :ensure t
  :mode (("\\.lisp'"    . lisp-mode)
         ("\\.lsp'"     . lisp-mode)
         ("\\.cl'"      . lisp-mode)
         ("\\.asd'"     . lisp-mode)
         ("\\.fasl'"    . lisp-mode))
  :config (slime-setup '(slime-company))
  :init (setq slime-contribs '(slime-fancy)))

(defun hook-lisp-mode ()
  "Hook for Lisp mode."
  (slime-mode t)
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))
  (setq inferior-lisp-program (if (eq system-type 'darwin) "/usr/local/bin/sbcl" "sbcl")))

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)
(add-hook 'inferior-lisp-mode-hook #'hook-inferior-lisp-mode)

;; -- Lang: Ruby ---------------------------------------------------------------

(use-package inf-ruby :ensure t :defer t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe :ensure t :defer t
  :after (company inf-ruby)
  :hook (ruby-mode . (lambda ()
                       (robe-mode 1)
                       (add-to-list 'company-backends 'company-robe)
                       ;; (when (executable-find "rvm")
                       ;;   (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
                       ;;     (rvm-activate-corresponding-ruby)))
                       )))

(use-package rubocop :ensure t :defer t
  :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools :ensure t :defer t
  :hook (ruby-mode . ruby-tools-mode))

(use-package ruby-mode :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(use-package yard-mode :ensure t
  :hook (ruby-mode . yard-mode))

;; Lang: Python ----------------------------------------------------------------

(use-package elpy :ensure t :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :commands elpy-enable
  :config (progn
            (setq python-indent-offset 4)
            (when (fboundp 'flycheck-mode)
              (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
              (add-hook 'elpy-mode-hook 'flycheck-mode))))

;; -- Lang: JavaScript ---------------------------------------------------------

(use-package js2-mode :ensure t
  :requires (rjsx-mode company-mode company-tern)
  :mode (("\\.js\\'" . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-default tab-width 2)
                      (setq js-indent-level 2)
                      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
                      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
                      (js2-imenu-extras-mode)
                      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                      (define-key js-mode-map (kbd "M-.") nil)
                      (setq-default js2-show-parse-errors nil)
                      (setq-default js2-strict-missing-semi-warning nil)
                      (setq-default js2-strict-trailing-comma-warning t)
                      (tern-mode t))))

(use-package rjsx-mode :ensure t
  :mode (("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package company-tern :ensure t
  :after company
  :config (progn
            (add-to-list 'company-backends 'company-tern)
            ;; Disable completion keybindings, as we use xref-js2 instead
            (unbind-key "M-." tern-mode-keymap)
            (unbind-key "M-," tern-mode-keymap)))

;; -- Lang: PHP ----------------------------------------------------------------

(use-package php-extras :ensure t)

(use-package php-mode :ensure t
  :init (setq comment-start "// "
              comment-end "")
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode))
  :hook (php-mode . (lambda ()
                      (php-enable-default-coding-style)
                      (set (make-local-variable 'company-backends)
                           '((php-extras-company company-dabbrev-code)
                             company-capf company-files)))))

;; -- Lang: web ----------------------------------------------------------------

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(use-package company-restclient :ensure t
  :after (company restclient)
  :config (add-to-list 'company-backends 'company-restclient))

(use-package scss-mode :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.vue\\'" . web-mode)))

;; -- Lang: Go -----------------------------------------------------------------

(eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package go-eldoc :ensure t :defer t)

(use-package go-mode :ensure t :defer t)

(use-package company-go :ensure t :defer t
  :after company
  :config (add-to-list 'company-backends 'company-go))

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook #'hook-go-mode)

;; -- Lang: text files ---------------------------------------------------------

(use-package dockerfile-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t
  :bind (("C-c C-t d" . markdown-insert-header-date)))

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;; -- Visual help toggles ------------------------------------------------------

(defun toggle-show-paren-mode-style ()
  "Toggle 'show-paren-mode' style."
  (interactive)
  (let* ((min 'parenthesis)
         (max 'expression)
         (style (if (equal show-paren-style min) max min)))
    (setq show-paren-style style)))

(global-set-key (kbd "C-c v p") 'toggle-show-paren-mode-style)

(use-package fill-column-indicator :ensure t)

(defun toggle-fill-column-indicator ()
  "Toggle 'fill-column-indicator'."
  (interactive)
  (fci-mode (if (bound-and-true-p fci-mode) -1 1)))

(global-set-key (kbd "C-c v i") 'toggle-fill-column-indicator)

(use-package highlight-indentation :ensure t)

(defun toggle-highlight-indentation ()
  "Toggle 'highlight-indentation'."
  (interactive)
  (highlight-indentation-mode (if (bound-and-true-p highlight-indentation-mode) -1 1)))

(global-set-key (kbd "C-c v h") 'toggle-highlight-indentation)

(defun toggle-linenum-mode ()
  "Toggle 'linum-mode'."
  (interactive)
  (global-linum-mode (if (bound-and-true-p global-linum-mode) -1 1)))

(global-set-key (kbd "C-c v l") 'toggle-linenum-mode)

(defun toggle-whitespace-mode-style ()
  "Toggle 'whitespace-mode' style."
  (interactive)
  (let* ((min '(tabs tab-mark face trailing))
         (max '(tabs tab-mark spaces space-mark lines lines-tail newline newline-mark empty face trailing))
         (style (if (equal whitespace-active-style min) max min)))
    (setq whitespace-style style))
  (whitespace-turn-off)
  (whitespace-turn-on))

(global-set-key (kbd "C-c v w") 'toggle-whitespace-mode-style)

;;; init.el ends here
