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

(defvar my/package-contents-refreshed nil)

(defun my/install-package(&rest packages)
  "Require PACKAGES and install them if necessary."
  (dolist (package packages)
    (when (not (package-installed-p package))
      (when (not my/package-contents-refreshed)
        (setq my/package-contents-refreshed t)
        (package-refresh-contents))
      (package-install package))))

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

(my/install-package 'simple-modeline)
(simple-modeline-mode)
(customize-set-variable 'simple-modeline-segments
                        '((simple-modeline-segment-modified
                           simple-modeline-segment-buffer-name
                           simple-modeline-segment-position)
                          (simple-modeline-segment-vc
                           simple-modeline-segment-process
                           simple-modeline-segment-misc-info
                           simple-modeline-segment-major-mode)))

;;; Look'n'feel

(my/install-package 'move-text)
(move-text-default-bindings) ;; Set M-<up> and M-<down> to move text up and down

(my/install-package 'hl-todo)
(global-hl-todo-mode)

(my/install-package 'idle-highlight-mode)
(idle-highlight-global-mode)

(require 'dired)
(setq dired-listing-switches "-alFh")

;;; Buffers

(setq split-height-threshold 80
      split-width-threshold 125
      display-buffer-alist '(("\\*compilation\\*"
                              (display-buffer-reuse-mode-window display-buffer-below-selected)
                              (dedicated . t)
                              (window-height . fit-window-to-buffer))))

;;; IDO

(require 'ido)
(my/install-package 'flx-ido 'ido-vertical-mode 'ido-completing-read+)
(setq ido-use-faces t
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t
      ido-save-directory-list-file (expand-file-name ".cache/ido.el" user-emacs-directory)
      ido-max-prospects 100
      ido-case-fold t
      ido-use-filename-at-point nil
      ido-use-url-at-point 'never
      ido-create-new-buffer 'always
      ido-use-faces t)

(setq-default ido-vertical-show-count t
              ido-vertical-define-keys 'C-n-C-p)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)
(set-face-attribute 'ido-subdir nil :foreground "#6395EE")

;;; Search

(require 'isearch)
(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format nil
      lazy-count-suffix-format " (%s/%s)")
(advice-add 'isearch-update :before 'recenter)

;;; Syntax

(my/install-package 'flycheck)
(global-flycheck-mode)

;;; Completion

(my/install-package 'company 'company-box)
(setq-default company-backends '(company-semantic
                                 company-capf
                                 company-files
                                 (company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet)
                                 company-dabbrev)
              company-dabbrev-minimum-length 2
              company-dabbrev-other-buffers t
              completion-styles '(basic flex)
              completions-max-height 40
              completion-ignore-case t
              company-files-exclusions '(".git/" ".DS_Store"))
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook 'company-box-mode)

;;; IDE

(my/install-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(my/install-package 'editorconfig)
(add-hook 'prog-mode-hook 'editorconfig-mode)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq-default whitespace-style '(tabs tab-mark face trailing)
              whitespace-action '(cleanup auto-cleanup))
(add-hook 'prog-mode-hook 'whitespace-mode)

(require 'which-func)
(setq which-func-unknown ""
      which-func-format '(:propertize which-func-current face which-func))
(add-hook 'prog-mode-hook 'which-function-mode)

(require 'time-stamp)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
(add-hook 'before-save 'time-stamp)

(my/install-package 'which-key)
(setq-default which-key-popup-type 'side-window
              which-key-side-window-location 'bottom
              which-key-side-window-max-height 0.5
              which-key-max-description-length 200
              which-key-add-column-padding 2)
(which-key-mode 1)

(my/install-package 'imenu-list)
(setq-default imenu-list-focus-after-activation t
              imenu-list-auto-resize t)

(my/install-package 'string-inflection)
(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)

;; Git

(my/install-package 'magit 'git-gutter)

(global-git-gutter-mode t)

(custom-set-variables
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign " ")
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " "))

(set-face-background 'git-gutter:modified "purple")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")

(add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)

;;; Snippets

(my/install-package 'yasnippet)
(setq-default yas-prompt-functions '(yas-ido-prompt)
              yas-snippet-dir (expand-file-name "snippets" user-emacs-directory))
(yas-global-mode t)

;;; Shell

(my/install-package 'vterm)

(defun spawn-shell(&optional dir)
  "Take an existing shell in DIR or create a new one."
  (interactive)
  (let ((buf (get-buffer "*vterm*"))
        ;; (default-directory (if dir dir (file-name-directory buffer-file-name)))
        (default-directory (if dir dir default-directory)))
    (if buf
        (switch-to-buffer buf)
      (vterm))))

(global-set-key (kbd "C-<return>") 'spawn-shell)
(global-set-key (kbd "C-x p <return>") #'(lambda()
                                           (interactive)
                                           (spawn-shell (project-root (project-current t)))))

;;; LSP

(require 'eglot)
(setq eldoc-documentation-strategy 'eldoc-documentation-compose)
(with-eval-after-load "eglot"
  (my/install-package 'flycheck-eglot)

  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider))

  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))
  (add-to-list 'eglot-server-programs '(js2-mode "typescript-language-server" "--stdio")) ;; npm i -g typescript-language-server typescript
  (add-to-list 'eglot-server-programs '(typescript-mode "typescript-language-server" "--stdio")) ;; npm i -g typescript-language-server typescript
  (add-to-list 'eglot-server-programs '(go-mode "gopls"))

  (add-hook 'php-mode-hook 'eglot-ensure)
  (add-hook 'js2-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)

  (global-set-key (kbd "C-c l ?") 'eldoc)
  (global-set-key (kbd "C-c l a") 'eglot-code-actions)
  (global-set-key (kbd "C-c l r") 'eglot-rename)
  (global-set-key (kbd "C-c l e") 'flycheck-list-errors)
  (global-set-key (kbd "C-c l f d") 'xref-find-definitions)
  (global-set-key (kbd "C-c l f r") 'xref-find-references)
  (global-set-key (kbd "C-c l p") 'xref-go-back)
  (global-set-key (kbd "C-c l n") 'xref-go-forward))

;;; Languages

(my/install-package 'dockerfile-mode
                    'terraform-mode
                    'json-mode
                    'yaml-mode
                    'php-mode)

(add-hook 'makefile-mode-hook #'(lambda()
                                  (whitespace-toggle-options '(tabs tab-mark))
                                  (setq indent-tabs-mode t)))
(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))

(my/install-package 'markdown-mode)
(setq-default markdown-enable-wiki-links t
              markdown-italic-underscore t
              markdown-asymmetric-header t
              markdown-make-gfm-checkboxes-buttons t
              markdown-gfm-uppercase-checkbox t
              markdown-fontify-code-blocks-natively t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(my/install-package 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(my/install-package 'restclient 'restclient-jq)
(with-eval-after-load "restclient"
  (require 'restclient-jq))
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

(my/install-package 'graphql-mode)
(add-hook 'graphql-mode-hook #'(lambda ()
                                 (setq-default tab-width 2)))

(my/install-package 'php-mode)

(my/install-package 'js2-mode 'rjsx-mode 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-hook 'j2-mode-hook #'(lambda ()
                            (make-local-variable 'js-indent-level)
                            (setq-default tab-width 2
                                          js2-basic-offset 2)
                            (setq js-indent-level 2)
                            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
                            (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
                            (js2-imenu-extras-mode)
                            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                            ;; (flycheck-select-checker 'javascript-eslint)
                            (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                            (setq-default flycheck-temp-prefix ".flycheck"
                                          flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                                             '(javascript-jshint json-jsonlist)))))

(my/install-package 'go-mode 'company-go)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook #'(lambda()
                            (whitespace-toggle-options '(tabs tab-mark))
                            (setq indent-tabs-mode t)))

;;; init.el ends here.
