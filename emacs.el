;;; emacs.el --- Emacs config

;; Time-stamp:  <2015-06-10 23:13:11>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;; This is yet another step to the biggest lie in the world:
;; a perfect Emacs configuration file.

;;; Code:

(defvar internal-libs
  '(autoinsert
    bookmark
    linum
    org
    paren
    recentf
    time-stamp
    whitespace
    yak))

(defvar internal-modes-off
  '(menu-bar-mode
    tool-bar-mode
    scroll-bar-mode))

(defvar internal-modes-on
  '(auto-compression-mode
    column-number-mode
    global-auto-revert-mode
    global-font-lock-mode
    global-hl-line-mode
    line-number-mode
    recentf-mode
    show-paren-mode
    transient-mark-mode
    which-function-mode))

(defvar init-funcs
  '(pl--init-behaviour
    pl--init-display
    pl--init-files
    pl--init-auto-insert
    pl--init-org-mode
    pl--init-keybindings))

(add-to-list 'load-path "~/work/src/yak")
(setq yak-dir-base (file-name-directory (or load-file-name (buffer-file-name))))

(mapc #'require internal-libs)
(mapc (lambda (mode) (when (fboundp mode) (funcall mode 1))) internal-modes-on)
(mapc (lambda (mode) (when (fboundp mode) (funcall mode -1))) internal-modes-off)

;;;;;;;;;;;;;;;;;
;; + functions ;;
;;;;;;;;;;;;;;;;;

(defun pl--mkpath (&rest args)
  "Build a path and eventually create it on file system according to ARGS."
  (unless (boundp 'yak-dir-base)
    (setq yak-dir-base user-emacs-directory))
  (let* ((name (plist-get args :name))
         (base (or (plist-get args :base) yak-dir-base))
         (directory (plist-get args :directory))
         (create (plist-get args :create))
         (path (expand-file-name (concat (file-name-as-directory base) name))))
    (when create
      (if directory
          (unless (file-accessible-directory-p path)
            (make-directory path t))
        (unless (file-exists-p path)
          (write-region "" nil path))))
    path))

(defun pl--set-indentation ()
  "Set indentation."
  (setq-default tab-width 4
                c-basic-offset 4
                c-hanging-comment-ender-p nil
                indent-tabs-mode nil))

(defun pl-set-locale (locale)
  "Set the LOCALE locale."
  (interactive "zLocale: ")
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (setq locale-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*shell*") (buffer-list))
          (switch-to-buffer "*shell*")
        (shell)))))

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when (display-graphic-p)
    (set-frame-parameter (selected-frame) 'alpha value)))

(defun pl-rb-require ()
  "Insert required rubygems."
  (interactive "*")
  (let ((gems (read-from-minibuffer "Rubygems to require: ")))
    (when gems
      (mapcar (lambda (gem)
                (insert (format "require \"%s\"\n" gem)))
              (split-string gems nil t)))))

(defun pl-google-at-point ()
  "Search on the internetz of Google."
  (interactive)
  (let* ((q (read-from-minibuffer "Google: " (thing-at-point 'symbol))))
    (browse-url (format "http://www.google.com/search?q=%s" q))))

(defun pl-kill-buffers-by-mode (&optional mode-name)
  "Kill buffers by mode.  Ask which mode if MODE-NAME is not provided."
  (interactive)
  (unless mode-name
    (setq mode-name (read-from-minibuffer "Mode to kill: ")))
  (let ((killed-buffers 0)
        (mode-to-kill (intern mode-name)))
    (dolist (buffer (buffer-list))
      (when (eq mode-to-kill (buffer-local-value 'major-mode buffer))
        (setq killed-buffers (1+ killed-buffers))
        (kill-buffer buffer)))
    (message "%d buffer(s) killed" killed-buffers)))

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;;;;;;;;;;;;;;;;
;; + packages ;;
;;;;;;;;;;;;;;;;

(yak-pkg 'anzu
         (global-anzu-mode +1)
         (set-face-attribute 'anzu-mode-line nil :foreground "yellow"))

(yak-pkg 'autopair
         (autopair-global-mode t))

(yak-pkg 'bonjourmadame)

(yak-pkg 'browse-kill-ring)

(yak-pkg 'company
         (setq company-auto-complete nil)
         (global-company-mode 1))

(yak-pkg 'cycle-resize)

(yak-pkg 'darkmine-theme
         (load-theme 'darkmine t))

(yak-pkg 'flx-ido)
(yak-pkg 'ido-hacks)
(yak-pkg 'ido-vertical-mode)
(yak-pkg 'ido
         (require 'ido)
         (require 'ido-hacks)
         (setq ido-case-fold t
               ido-enable-flex-matching t
               ido-use-filename-at-point 'guess
               ido-create-new-buffer 'always
               ido-use-virtual-buffers t)
         (ido-everywhere 1)
         (flx-ido-mode 1)
         (ido-mode t)
         (ido-hacks-mode)
         (ido-vertical-mode))

(yak-pkg 'htmlize)
(yak-pkg 'idle-highlight-mode)
(yak-pkg 'js2-mode)
(yak-pkg 'markdown-mode)
(yak-pkg 'php-extras)
(yak-pkg 'php-mode)
(yak-pkg 'rainbow-delimiters)
(yak-pkg 'rainbow-mode)
(yak-pkg 'ruby-mode)
(yak-pkg 'symon
         (setq symon-delay 5)
         (symon-mode t))

(yak-pkg 'web-mode)

(yak-pkg 'whitespace
         (setq whitespace-line-column 80
               whitespace-style '(tabs tab-mark face)
               whitespace-global-modes '(not org-mode web-mode))
         (global-whitespace-mode))

(yak-pkg 'yaml-mode)

;;;;;;;;;;;;;
;; + hooks ;;
;;;;;;;;;;;;;

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(add-hook 'dired-mode-hook
          (lambda ()
            (put 'dired-find-alternate-file 'disabled nil)))

(add-hook 'text-mode-hook
          (lambda ()
            (linum-mode 1)
            (make-local-variable 'linum-format)
            (setq linum-format " %d ")))

(add-hook 'prog-mode-hook
          (lambda ()
            (idle-highlight-mode t)
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)
            (rainbow-delimiters-mode)))

(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-extras)
            (setq comment-start "// "
                  comment-end "")
            (set (make-local-variable 'indent-tabs-mode) nil)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)))

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)))

(add-hook 'makefile-mode-hook
          (lambda ()
            (whitespace-toggle-options '(tabs))
            (setq indent-tabs-mode t)))

(add-hook 'before-save-hook
          (lambda ()
            (time-stamp)
            (delete-trailing-whitespace)
            (whitespace-cleanup)))

;;;;;;;;;;;;;;;;;;;;
;; + initializers ;;
;;;;;;;;;;;;;;;;;;;;

(defun pl--init-behaviour ()
  "Initialize behaviour."
  (setq user-full-name "Pierre Lecocq"
        user-mail-address "pierre.lecocq@gmail.com"
        frame-title-format "Emacs %f"
        time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
        initial-scratch-message ";; Scratch buffer\n\n"
        inhibit-startup-message t
        inhibit-splash-screen t
        backup-inhibited t
        make-backup-files nil
        auto-save-default nil
        vc-follow-symlinks t
        kill-whole-line t
        require-final-newline t
        next-line-add-newlines nil
        recentf-max-menu-items 50
        password-cache-expiry nil
        uniquify-buffer-name-style 'forward uniquify-separator "/"
        bookmark-default-file (pl--mkpath :name "bookmarks")
        org-directory (pl--mkpath :name "org-files" :directory t :create t :base "~/")
        custom-file (pl--mkpath :name "custom.el")
        host-file (pl--mkpath :name (format "host-%s.el" (downcase (car (split-string (system-name) "\\."))))))
  (auto-insert)
  (pl--set-indentation)
  (pl-set-locale 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p))

(defun pl--init-display ()
  "Initialize display."
  (setq-default show-trailing-whitespace t
                highlight-tabs t
                mode-line-format
                (list
                 '(:eval (if (buffer-modified-p)
                             (propertize "  %b" 'face 'bold-italic)
                           (propertize "  %b" 'face 'bold)))
                 " (%l:%c) %p/%I - %m";; (format " %s" minor-mode-alist)
                 '(which-function-mode (" " which-func-format))))
  (when (member "Inconsolata" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Inconsolata" ;; "DejaVu Sans Mono"
                        :height 120
                        :weight 'normal
                        :width 'normal))
  (when (display-graphic-p)
    (setq show-paren-style 'expression
	  select-enable-clipboard t)
    (set-fringe-mode 10)))

(defun pl--init-files ()
  "Initialize files."
  (add-to-list 'auto-mode-alist '("\\.log\\'"         . auto-revert-mode))
  (add-to-list 'auto-mode-alist '("\\.js[on]\\'"      . js2-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile"        . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile"       . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile"           . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile"          . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'"        . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'"          . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'"     . ruby-mode))
  (add-to-list 'auto-mode-alist '(".bashrc"           . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".zshrc"            . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".gnus"             . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"         . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erubis\\'"      . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'"       . yaml-mode)))

(defun pl--init-auto-insert ()
  "Initialize auto-insert."
  (setq auto-insert-alist
        '(((ruby-mode . "Ruby program") nil
           "#!/usr/bin/env ruby\n\n"
           "# File: " (file-name-nondirectory buffer-file-name) "\n"
           "# Time-stamp: <>\n"
           "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
           "# Description: " _ "\n\n")
          ((emacs-lisp-mode . "Emacs lisp mode") nil
           ";;; " (file-name-nondirectory buffer-file-name) " --- " _ "\n\n"
           ";; Time-stamp: <>\n"
           ";; Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n\n"
           ";;; Commentary:\n\n"
           ";;; Code:\n\n"
           ";;; " (file-name-nondirectory buffer-file-name) " ends here\n")
          ((c-mode . "C program") nil
           "/*\n"
           " * File: " (file-name-nondirectory buffer-file-name) "\n"
           " * Time-stamp: <>\n"
           " * Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
           " * Description: " _ "\n"
           " */\n\n")
          ((shell-mode . "Shell script") nil
           "#!/bin/bash\n\n"
           " # File: " (file-name-nondirectory buffer-file-name) "\n"
           " # Time-stamp: <>\n"
           " # Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
           " # Description: " _ "\n\n"))))

(defun pl--init-org-mode ()
  "Initialize Org."
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-src-fontify-natively t
        org-default-notes-file (pl--mkpath :name "notes.org" :create t :base org-directory)
        org-agenda-files (list (pl--mkpath :name "agenda.org" :create t :base org-directory))))

(defun org-font-lock-ensure (beg end)
  "Org font lock ensure from BEG to END."
  (font-lock-ensure))

(defun pl--init-keybindings ()
  "Initialize keybindings."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier nil
          mac-command-modifier 'meta
          select-enable-clipboard t))
  (global-set-key [delete] 'delete-char)
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "C-;") 'other-window)
  (global-set-key (kbd "M-y") 'browse-kill-ring)
  (global-set-key (kbd "M-o") 'occur)
  (global-set-key (kbd "C-c C-c") 'comment-region)
  (global-set-key (kbd "C-c C-u") 'uncomment-region)
  (global-set-key (kbd "C-S-s") 'find-grep)
  (global-set-key (kbd "C-S-f") 'imenu)
  (global-set-key (kbd "C-S-x k") 'pl-kill-buffers-by-mode)
  (global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
  (global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)
  (global-set-key [f5] 'bookmark-bmenu-list)
  (global-set-key [f6] 'recentf-open-files)
  (global-set-key [f12] 'pl-get-shell)
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z")))
  ;; Credits to emacsfodder
  (define-key occur-mode-map (kbd "<down>")
    (lambda ()
      (interactive)
      (occur-next)
      (occur-mode-goto-occurrence-other-window)
      (recenter)
      (other-window 1)))
  (define-key occur-mode-map (kbd "<up>")
    (lambda ()
      (interactive)
      (occur-prev)
      (occur-mode-goto-occurrence-other-window)
      (recenter)
      (other-window 1))))

;;;;;;;;;;;;;;;;;
;; + bootstrap ;;
;;;;;;;;;;;;;;;;;

(mapc #'funcall init-funcs)

(dolist (f (list host-file custom-file))
  (when (file-exists-p f)
    (load f 'noerror)))

(message "Config has been successfully loaded from %s" yak-dir-base)

;;; emacs.el ends here
