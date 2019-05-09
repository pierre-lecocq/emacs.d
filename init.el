;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-09 10:08:26>
;; Copyright (C) 2019 Pierre Lecocq
;; Version: <insert your big int here>
;; Code name: Yet another rewrite

;;; Commentary:

;;; Code:

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

(setq auto-save-default nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      case-fold-search t
      debug-on-error t
      frame-title-format "%b (%m) - %F"
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message (format ";; Scratch - Started on %s\n\n" (current-time-string))
      load-prefer-newer t
      make-backup-files nil
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1)
      next-line-add-newlines nil
      require-final-newline t
      scroll-conservatively 101
      select-enable-clipboard t
      sentence-end-double-space nil
      show-trailing-whitespace t
      uniquify-buffer-name-style 'forward uniquify-separator "/"
      use-dialog-box nil
      user-full-name "Pierre Lecocq")

(setq custom-file (concat (file-name-directory load-file-name) ".local/files/my-custom.el")
      nsm-settings-file (concat (file-name-directory load-file-name) ".local/files/network-security.data"))

(setq-default fill-column 80)

;; -- Indent -------------------------------------------------------------------

(setq-default backward-delete-char-untabify-method 'hungry
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              electric-indent-inhibit t
              indent-tabs-mode nil
              tab-width 4)

;; -- Charset ------------------------------------------------------------------

(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)

;; -- Package manager ----------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil
      package-user-dir (concat (file-name-directory load-file-name) ".local/packages")
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

(use-package bind-key :demand t :ensure t)
(use-package diminish :demand t :ensure t)

;; -- Theme --------------------------------------------------------------------

(when (display-graphic-p)
  (toggle-frame-maximized))

(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(defun set-font-size (wanted-size)
  "Change font size to WANTED-SIZE."
  (interactive "nFont size: ")
  (let ((wanted-font "Source Code Pro"))
    (if (member wanted-font (font-family-list))
        (set-frame-font (format "%s %d" wanted-font wanted-size) nil t)
      (warn "Font %s not found" wanted-font))))

(set-font-size 12)

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package all-the-icons-dired :ensure t
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package darkokai-theme :ensure t
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

(use-package doom-modeline :ensure t
  :config (progn
            (doom-modeline-def-modeline 'my-modeline
                                        '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                                        '(misc-info major-mode vcs checker))
            (defun setup-custom-doom-modeline ()
              (doom-modeline-set-modeline 'my-modeline 'default))
            (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))
  :hook (after-init . doom-modeline-mode))

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

(use-package which-key :demand t :ensure t
  :config (which-key-mode 1))

;; -- Utilities ----------------------------------------------------------------

(use-package anzu :ensure t
  :config (global-anzu-mode +1)
  :custom-face (anzu-mode-line ((t (:foreground "yellow")))))

(use-package autoinsert :demand t
  :init (progn
          (auto-insert-mode 1)
          (auto-insert)))

(use-package autopair :ensure t
  :config (autopair-global-mode t))

(use-package epa-file :ensure nil :demand t
  :init (setq epa-gpg-program "gpg2")
  :config (epa-file-enable))

(use-package time-stamp :demand t
  :init (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")
  :hook (before-save . time-stamp))

;; -- Navigation ---------------------------------------------------------------

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
  :init (setq ido-save-directory-list-file (concat (file-name-directory load-file-name) ".local/files/my-ido.el")
              ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ido-create-new-buffer 'always
              ido-vertical-show-count t))

;; -- Completion ---------------------------------------------------------------

(use-package company :ensure t
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
                                  company-dabbrev-code)))
  :hook (after-init-hook . company-mode))

;; -- Jump ---------------------------------------------------------------------

;; (use-package dumb-jump :ensure t
;;   :config (dumb-jump-mode)
;;   :bind (("C-c q l" . dumb-jump-quick-look)
;;          ("C-c q g" . dumb-jump-go)
;;          ("C-c q b" . dumb-jump-back)
;;          ("C-c q o" . dumb-jump-other-window)
;;          ("C-c q p" . dumb-jump-go-prompt)))

;; -- File tree ----------------------------------------------------------------

(defun neotree-project-dir ()
  "Open NeoTree using the git root of the current project."
  (interactive)
  ;; (let ((project-dir (ffip-project-root)))
  (let ((project-dir (projectile-project-root)))
    (neotree-dir project-dir)
    (neotree-find (buffer-file-name))))

(use-package neotree :ensure t
  :after (:all all-the-icons)
  :bind (("C-c f t" . neotree-toggle)
         ("C-c f p" . neotree-project-dir)
         ("C-c f h" . neotree-hidden-file-toggle))
  :init (setq neo-smart-open t
              neo-window-fixed-size nil
              neo-theme (if (display-graphic-p) 'icons 'nerd)))

;; -- Externals ----------------------------------------------------------------

(load-file "programming.el")

;; (when (file-exists-p "host.el")
;;   (load-file "host.el"))

;;; init.el ends here
