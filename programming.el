;;; programming.el --- Programming config -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-09 10:06:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package aggressive-indent :ensure t
  :config (progn
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'web-mode))
  :hook (prog-mode . global-aggressive-indent-mode))

(use-package editorconfig :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package flycheck :ensure t
  :bind (("<f11>" . flycheck-list-errors))
  :hook (prog-mode . flycheck-mode))

(use-package idle-highlight-mode :ensure t
  :hook (prog-mode . idle-highlight-mode))

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-known-projects-file (concat (file-name-directory load-file-name) ".local/files/projectile-bookmarks.eld"))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (projectile-register-project-type 'npm '("package.json"))))

(use-package rainbow-mode :ensure t
  :hook (prog-mode . rainbow-turn-on))

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-func :ensure t :demand t
  :config (progn
            (setq which-func-unknown "?")
            (set-face-attribute 'which-func nil :foreground "DodgerBlue"))
  :hook (prog-mode . which-function-mode))

(use-package whitespace :demand t :ensure nil
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

(use-package git-gutter :ensure t
  :config (progn
            (set-face-background 'git-gutter:added nil)
            (set-face-foreground 'git-gutter:added "green")
            (set-face-background 'git-gutter:modified nil)
            (set-face-foreground 'git-gutter:modified "yellow")
            (set-face-background 'git-gutter:deleted nil)
            (set-face-foreground 'git-gutter:deleted "red"))
  :hook (prog-mode . git-gutter-mode))

;; -- Emacs Lisp ---------------------------------------------------------------

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode)
  (add-to-list 'auto-insert-alist
               '((emacs-lisp-mode . "Emacs lisp program") nil
                 ";;; " (file-name-nondirectory buffer-file-name) " --- " _ " -*- lexical-binding: t; -*-\n\n"
                 ";; Time-stamp: <>\n"
                 ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
                 ";;; Commentary:\n\n"
                 ";;; Code:\n\n"
                 ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; -- Common Lisp --------------------------------------------------------------

(use-package slime-company :ensure t :defer t)

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
  ;; quicklisp
  (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p helper-file)
        (load helper-file)
      (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))
  ;; interpreter
  (setq inferior-lisp-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/sbcl"
          "sbcl"))
  ;; autoinsert
  (add-to-list 'auto-insert-alist
               '((lisp-mode . "Lisp program") nil
                 ";;;; " (file-name-nondirectory buffer-file-name) "\n\n"
                 ";; Time-stamp: <>\n"
                 ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n")))

(defun hook-inferior-lisp-mode ()
  "Hook for inferior Lisp  mode."
  (inferior-slime-mode t))

(add-hook 'lisp-mode-hook #'hook-lisp-mode)
(add-hook 'inferior-lisp-mode-hook #'hook-inferior-lisp-mode)

;; -- SH -----------------------------------------------------------------------

(defun hook-sh-mode ()
  "Hook for sh mode."
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
                 "BASE_DIR=$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)\n\n")))

(add-hook 'sh-mode-hook #'hook-sh-mode)

;; -- Makefile -----------------------------------------------------------------

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)

;; -- C ------------------------------------------------------------------------

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (progn
          (add-to-list 'company-backends 'company-cmake)
          (add-to-list 'company-backends 'company-c-headers)))

(defun hook-c-mode ()
  "Hook for C mode."
  (c-set-offset 'case-label '+)
  (add-to-list 'auto-insert-alist
               '((c-mode . "C program") nil
                 "/*\n"
                 " * File: " (file-name-nondirectory buffer-file-name) "\n"
                 " * Time-stamp: <>\n"
                 " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
                 " * Description: " _ "\n"
                 " */\n\n")))

(add-hook 'c-mode-common-hook #'hook-c-mode)

;; -- Go -----------------------------------------------------------------------

(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :init (progn (exec-path-from-shell-initialize)
               (exec-path-from-shell-copy-env "GOPATH")))

(use-package go-eldoc :ensure t :defer t)

(use-package go-mode :ensure t :defer t)

(use-package company-go :ensure t :defer t)

(defun hook-go-mode ()
  "Hook for Go mode."
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook #'hook-go-mode)

;; -- Ruby ---------------------------------------------------------------------

(use-package inf-ruby :ensure t :defer t)

(use-package robe :ensure t :defer t
  :init (push 'company-robe company-backends))

(use-package rubocop :ensure t :defer t)

(use-package ruby-tools :ensure t :defer t)

(use-package ruby-mode :ensure t
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(use-package yard-mode :ensure t)

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
  (robe-start)
  (yard-mode)
  (rubocop-mode)
  (ruby-tools-mode)
  (add-to-list 'auto-insert-alist
               '((ruby-mode . "Ruby program") nil
                 "#!/usr/bin/env ruby\n"
                 "# -*- mode: ruby; -*-\n\n"
                 "# File: " (file-name-nondirectory buffer-file-name) "\n"
                 "# Time-stamp: <>\n"
                 "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
                 "# Description: " _ "\n\n")))

(add-hook 'ruby-mode-hook #'hook-ruby-mode)

;; -- Python -------------------------------------------------------------------

(use-package elpy :ensure t :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :commands elpy-enable
  :config  (progn
             (setq python-indent-offset 4)
             (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
             (when (fboundp 'flycheck-mode)
               (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))))

(defun hook-python-mode ()
  "Hook for python mode."
  (add-to-list 'auto-insert-alist
               '((python-mode . "Python program") nil
                 "#!/usr/bin/env python\n\n")))

(add-hook 'python-mode-hook #'hook-python-mode)

;; -- PHP ----------------------------------------------------------------------

(use-package php-extras :ensure t :defer t)

(use-package php-mode :ensure t
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)
         ("\\.php-dev'" . php-mode)))

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end "")
  (add-to-list 'auto-insert-alist
               '((php-mode . "PHP script") nil
                 "<?php\n\n")))

(add-hook 'php-mode-hook #'hook-php-mode)

;; -- JS ------------------------------------------------------------------------

(use-package js2-refactor :ensure t :defer t)

(use-package xref-js2 :ensure t :defer t) ;; requires installing `ag'

(when (executable-find "tern") ;; `sudo npm install -g tern'
  (use-package company-tern :ensure t
    :config (progn
              (add-to-list 'company-backends 'company-tern)
              ;; Disable completion keybindings, as we use xref-js2 instead
              (unbind-key "M-." tern-mode-keymap)
              (unbind-key "M-," tern-mode-keymap))))

(use-package js2-mode :ensure t
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

(use-package sqlup-mode :ensure t :defer t)

(use-package sql-indent :ensure t :defer t)

(defun hook-sql-mode ()
  "Hook for SQL mode."
  (sqlup-mode t)
  (toggle-truncate-lines t))

(add-hook 'sql-mode-hook #'hook-sql-mode)
(add-hook 'sql-interactive-mode-hook #'hook-sql-mode) ;; When connected to a server within Emacs

;; -- Web ----------------------------------------------------------------------

(use-package htmlize :ensure t :defer t)

(use-package scss-mode :ensure t :defer t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

;; -- HTTP ---------------------------------------------------------------------

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(defun hook-restclient-mode ()
  "Hook for restclient mode."
  (add-to-list 'auto-insert-alist
               '((restclient-mode . "REST client") nil
                 "# -*- restclient -*-\n\n")))

(add-hook 'restclient-mode-hook #'hook-restclient-mode)

;; -- Text ---------------------------------------------------------------------

(use-package dockerfile-mode :ensure t :defer t)

(use-package terraform-mode :ensure t :defer t)

(use-package json-mode :ensure t :defer t)

(use-package markdown-mode :ensure t :defer t)

(use-package flymd :ensure t :defer t
  :config (setq flymd-output-directory "/tmp"
                flymd-close-buffer-delete-temp-files t)
  :bind (("C-c m p" . flymd-flyit)))

(use-package toml-mode :ensure t :defer t)

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;;; programming.el ends here
