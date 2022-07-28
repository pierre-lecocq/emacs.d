;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; Time-stamp: <2022-07-28 14:02:05>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(let ((dir (expand-file-name ".cache" user-emacs-directory)))
  (unless (file-exists-p dir)
    (make-directory dir t)))

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

;; -- Packages -----------------------------------------------------------------

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (expand-file-name ".cache/packages" user-emacs-directory)
      package-archives '(("melpa" . "https://melpa.org/packages/")
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
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
(global-set-key (kbd "C-o") 'other-window)

(advice-add 'split-window-right :after #'(lambda (&rest _) (other-window 1)))
(advice-add 'split-window-below :after #'(lambda (&rest _) (other-window 1)))

;; -- Look & feel --------------------------------------------------------------

(toggle-frame-maximized)

(when (and window-system (eq system-type 'darwin))
  (setq frame-title-format nil
        ns-use-proxy-icon nil
        frame-resize-pixelwise t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; (use-package modus-themes :ensure t
;;   :init (modus-themes-load-themes)
;;   :config (modus-themes-load-vivendi)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package nord-theme :ensure t
  :config (load-theme 'nord t))

(use-package rainbow-delimiters :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package simple-modeline :ensure t
  :hook (after-init . simple-modeline-mode)
  :custom (simple-modeline-segments
           '((simple-modeline-segment-modified
              simple-modeline-segment-buffer-name
              simple-modeline-segment-position)
             (simple-modeline-segment-vc
              simple-modeline-segment-process
              simple-modeline-segment-misc-info
              simple-modeline-segment-major-mode))))

;; -- Utils --------------------------------------------------------------------

(use-package ag :ensure t
  :init (setq ag-highlight-search t))

(use-package dired :ensure nil :demand t
  :init (setq dired-recursive-copies 'always
              dired-recursive-deletes 'always
              delete-by-moving-to-trash t
              dired-listing-switches "-aFlv"
              wdired-allow-to-change-permissions t
              wdired-create-parent-directories t
              dired-use-ls-dired (if (eq system-type 'darwin) nil t)))

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

(use-package time-stamp :ensure t :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save . time-stamp))

(use-package uniquify :ensure nil :demand t
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-separator "/"
              uniquify-after-kill-buffer-p t
              uniquify-ignore-buffers-re "^\\*"))

(use-package which-key :demand t :ensure t
  :init (setq which-key-popup-type 'side-window
              which-key-side-window-location 'bottom
              which-key-side-window-max-height 0.5
              which-key-max-description-length 200
              which-key-add-column-padding 2)
  :config (which-key-mode 1))

;; -- IDE ----------------------------------------------------------------------

(use-package editorconfig :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package idle-highlight-mode :ensure t
  :hook (prog-mode . idle-highlight-mode))

(use-package imenu :ensure t
  :bind ("C-c i m" . imenu))

(use-package imenu-list :ensure t
  :config (imenu-list-minor-mode)
  :bind ("C-c i l" . imenu-list))

(use-package string-inflection :ensure t
  :bind (("C-c C-u" . string-inflection-all-cycle)))

(use-package which-func :ensure t :demand t
  :init (setq which-func-unknown "?")
  :hook (prog-mode . which-function-mode))

(use-package whitespace :demand t :ensure nil
  :init (setq whitespace-line-column 80
              whitespace-style '(tabs tab-mark face trailing))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

(use-package isearch :ensure nil :demand t
  :config (setq search-highlight t
                search-whitespace-regexp ".*?"
                isearch-lax-whitespace t
                isearch-regexp-lax-whitespace nil
                isearch-lazy-highlight t
                isearch-lazy-count t
                lazy-count-prefix-format nil
                lazy-count-suffix-format " (%s/%s)")
  (defadvice isearch-update (before my-isearch-reposite activate)
    "Update an isearch session by recentering the buffer to the found location."
    (recenter)))

(use-package vterm :ensure t
  :init (setq confirm-kill-processes nil)
  :bind ("<C-return>" . (lambda ()
                          (interactive)
                          (let ((bname "vterm"))
                            (if (string= (buffer-name) bname)
                                (switch-to-buffer (other-buffer))
                              (if (get-buffer bname)
                                  (switch-to-buffer bname)
                                (vterm)))))))

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
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

(use-package flycheck :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package all-the-icons :ensure t) ;; M-x all-the-icons-install-fonts

(use-package treemacs :ensure t :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (treemacs-git-mode 'deferred)
            (treemacs-resize-icons 16))
  :bind ("C-c f t" . treemacs-display-current-project-exclusively))

(use-package treemacs-projectile :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit :ensure t
  :after (treemacs magit))

(use-package company :ensure t
  :config (global-company-mode)
  :init (setq company-auto-complete nil
              company-minimum-prefix-length 1
              company-tooltip-limit 20
              company-idle-delay 0.25
              company-dabbrev-downcase nil))

(use-package company-quickhelp :ensure t
  :config (company-quickhelp-mode))

(use-package yasnippet :ensure t
  :after company)

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

;; -- LSP ----------------------------------------------------------

(use-package lsp-mode :ensure t :defer t
  :hook (((js2-mode rjsx-mode go-mode-hook) . lsp-deferred))
  :commands lsp
  :config (progn
            (setq lsp-log-io nil
                  lsp-auto-configure t
                  lsp-auto-guess-root t
                  lsp-eldoc-hook nil
                  lsp-modeline-diagnostics-enable t
                  lsp-modeline-diagnostics-scope :file)
            (yas-minor-mode t))
  (lsp-enable-which-key-integration))

(use-package lsp-ui :ensure t :defer t
  :commands lsp-ui-mode
  :config (setq lsp-ui-sideline-enable nil
                ;; lsp-ui-sideline-enable t
                ;; lsp-ui-sideline-show-symbol t
                ;; lsp-ui-sideline-show-hover t
                ;; lsp-ui-sideline-show-code-actions t
                ;; lsp-ui-sideline-delay 0.05
                lsp-ui-peek-enable t
                lsp-ui-imenu-enable t
                lsp-ui-imenu-auto-refresh t
                lsp-ui-doc-enable nil
                lsp-ui-doc-header t
                lsp-ui-doc-include-signature t
                lsp-ui-doc-border (face-foreground 'default)))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package dap-mode :ensure t :defer t
;;   :custom (lsp-enable-dap-auto-configure nil)
;;   :config (progn
;;             (dap-ui-mode 1)
;;             (dap-auto-configure-features '(sessions locals tooltip))))

;; -- LANG: elisp --------------------------------------------------------------

(use-package eros :ensure t)

(defun hook-emacs-lisp-mode ()
  "Hook for emacs-lisp mode."
  (eros-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook #'hook-emacs-lisp-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; -- LANG: Makefile -----------------------------------------------------------

(defun hook-makefile-mode ()
  "Hook for Makefile mode."
  (whitespace-toggle-options '(tabs tab-mark))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'hook-makefile-mode)
(add-to-list 'auto-mode-alist '("Makefile.*\\'" . makefile-mode))

;; -- LANG: Text ---------------------------------------------------------------

(use-package dockerfile-mode :ensure t)

(use-package dotenv-mode :ensure t
  :mode "\\.env\\..*\\'")

(use-package json-mode :ensure t)

(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-enable-wiki-links t
              markdown-italic-underscore t
              markdown-asymmetric-header t
              markdown-make-gfm-checkboxes-buttons t
              markdown-gfm-uppercase-checkbox t
              markdown-fontify-code-blocks-natively t))

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

(defun hook-text-mode ()
  "Hook for Text mode."
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'hook-text-mode)

;; -- LANG: Web ----------------------------------------------------------------

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

;; -- LANG: Go -----------------------------------------------------------------

(use-package go-mode :ensure t
  :config (add-hook 'before-save-hook 'gofmt-before-save)
  :init (setq whitespace-style '(face trailing)))


;; -- LANG: JS -----------------------------------------------------------------

(use-package js2-mode :ensure t
  :mode (("\\.js$" . js2-mode))
  ;; :config (progn
  ;;           (require 'dap-node)
  ;;           (dap-node-setup))
  :hook (js2-mode . (lambda ()
                      (setq-default tab-width 2)
                      (setq js-indent-level 2
                            js2-basic-offset 2)
                      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
                      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
                      (js2-imenu-extras-mode)
                      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                      (define-key js-mode-map (kbd "M-.") nil)
                      (flycheck-select-checker 'javascript-eslint)
                      (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                      (setq-default flycheck-temp-prefix ".flycheck"
                                    flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                                       '(javascript-jshint json-jsonlist))))))

(use-package rjsx-mode :ensure t
  :after js2-mode
  :mode (("components\\/.*\\.js\\'" . rjsx-mode)))

;; -- LANG: PHP ----------------------------------------------------------------

;; (use-package php-extras :ensure t)

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

;; -- Functions ----------------------------------------------------------------

(defun emacs-lisp-mode-header ()
  "Insert header for `emacs-lisp-mode'."
  (insert (file-name-nondirectory buffer-file-name) " --- [insert description] -*- lexical-binding: t; -*-\n\n"))

(defun emacs-lisp-mode-footer ()
  "Insert footer for `emacs-lisp-mode'."
  (insert "\n;;; " (file-name-nondirectory buffer-file-name) " ends here\n"))

(defun sh-mode-header ()
  "Insert header for `sh-mode'."
  (insert "#!/usr/bin/env sh\n\n"))

(defun php-mode-header ()
  "Insert header for `php-mode'."
  (insert "<?php\n\n"))

(defun insert-header ()
  "Insert header in current file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((mode-header-func (intern (concat (symbol-name major-mode) "-header"))))
      (if (fboundp mode-header-func)
          (funcall mode-header-func)))
    (insert "File: " (file-name-nondirectory buffer-file-name) "\n" )
    (insert "Time-stamp: <>\n")
    (insert "Copyright (C): " (substring (current-time-string) -4) " " (user-full-name) "\n\n")
    (comment-region (point-min) (point))
    (let ((mode-footer-func (intern (concat (symbol-name major-mode) "-footer"))))
      (if (fboundp mode-footer-func)
          (progn
            (goto-char (point-max))
            (funcall mode-footer-func))))))

;;; init.el ends here
