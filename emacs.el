;;; emacs.el --- Emacs config

;; Time-stamp:  <2015-05-26 22:44:55>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;; This is yet another step to the biggest lie in the world:
;;  a perfect Emacs configuration file.

;;; Code:

(defvar yak/base-dir (file-name-directory (or load-file-name (buffer-file-name))))
(defvar yak/pkg-initialized nil)
(defvar yak/pkg-refreshed nil)

;;;; core - Yet Another Konfig-helper

(defun yak/initialize (name)
  "Initialize and refresh the package manager if needed (to install NAME)."
  (unless yak/pkg-initialized
    (setq package-archives
          '(("melpa"        . "http://melpa.org/packages/")
            ("gnu"          . "http://elpa.gnu.org/packages/")
            ("marmalade"    . "http://marmalade-repo.org/packages/")))
    (package-initialize)
    (setq yak/pkg-initialized t))
  (unless (or yak/pkg-refreshed
              (package-built-in-p name)
              (package-installed-p name))
    (package-refresh-contents)
    (setq yak/pkg-refreshed t)))

(defun yak/pkg-install (name)
  "Install the NAME package."
  (yak/initialize name)
  (unless (or (package-built-in-p name)
              (package-installed-p name))
    (package-install name)))

(defmacro yak/pkg (name &rest body)
  "Install the NAME package and configure with BODY."
  `(progn
     (yak/pkg-install ,name)
     (eval-after-load ,name
       (progn ,@body))))

(defmacro yak/repo (name &rest body)
  "Install the NAME package from its repository and configure with BODY."
  `(progn
     (let ((bin "git"))
       (unless (executable-find bin)
         (error "The executable git is not found in your PATH"))
       (let* ((repo-name (car (last (split-string (symbol-name ,name) "/"))))
              (path (concat
                     (file-name-as-directory lisp-user-dir)
                     repo-name))
              (cmd (format "%s clone https://github.com/%s %s"
                           bin (symbol-name ,name) path)))
         (unless (file-accessible-directory-p path)
           (shell-command cmd))
         (add-to-list 'load-path path)
         (require (intern repo-name))
         (progn ,@body)))))

;;;; functions

(defun pl/mkpath (&rest args)
  "Build a path and eventually create it on file system according to ARGS."
  (unless (boundp 'yak/base-dir)
    (setq yak/base-dir user-emacs-directory))
  (let* ((name (plist-get args :name))
         (base (or (plist-get args :base) yak/base-dir))
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

(defun pl/set-locale (locale)
  "Set the LOCALE locale."
  (interactive "zLocale: ")
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (setq locale-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(defun pl/set-indentation ()
  "Set indentation."
  (setq-default
   tab-width 4
   c-basic-offset 4
   c-hanging-comment-ender-p nil
   indent-tabs-mode nil))

(pl/set-indentation)

(defun pl/get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*shell*") (buffer-list))
          (switch-to-buffer "*shell*")
        (shell)))))

(defun pl/transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when window-system
    (set-frame-parameter (selected-frame) 'alpha value)))

(defun pl/rb-require ()
  "Insert required rubygems."
  (interactive "*")
  (let ((gems (read-from-minibuffer "Rubygems to require: ")))
    (when gems
      (mapcar (lambda (gem)
                (insert (format "require \"%s\"\n" gem)))
              (split-string gems nil t)))))

(defun pl/google-at-point ()
  "Search on the internetz of Google."
  (interactive)
  (let* ((q (read-from-minibuffer "Google: " (thing-at-point 'symbol))))
    (browse-url (format "http://www.google.com/search?q=%s" q))))

(defun pl/kill-buffers-by-mode (&optional mode-name)
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

(defun pl/cycle-dictionaries()
  "Cycle through my dictionaries."
  (interactive)
  (let* ((prev-dict ispell-dictionary)
         (next-dict (if (string= prev-dict "francais") "english" "francais")))
    (setq ispell-dictionary next-dict)
    (message "Dictionary switched from %s to %s" prev-dict next-dict)))

(defun pl/force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;;;; display

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when window-system (set-fringe-mode 10))

;; Set Inconsolata font but falls back to DejaVu when unicode chars fail
(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 120
                      :weight 'normal
                      :width 'normal))
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12
                               :weight 'normal)))

;;;; internals

(require 'autoinsert)
(require 'bookmark)
(require 'linum)
(require 'org)
(require 'package)
(require 'paren)
(require 'recentf)
(require 'time-stamp)
(require 'whitespace)

(defvar host-file nil)
(defvar lisp-user-dir nil)

(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(global-font-lock-mode t)
(line-number-mode t)
(recentf-mode 1)
(show-paren-mode t)
(transient-mark-mode t)
(which-function-mode)

(setq
 user-full-name "Pierre Lecocq"
 user-mail-address "pierre.lecocq@gmail.com"
 frame-title-format "Emacs %f"
 time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
 initial-scratch-message ";; Scratch buffer\n\n"
 inhibit-startup-message t
 inhibit-splash-screen t
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 kill-whole-line t
 require-final-newline t
 next-line-add-newlines nil
 show-paren-style 'expression
 recentf-max-menu-items 50
 password-cache-expiry nil
 uniquify-buffer-name-style 'forward uniquify-separator "/"
 bookmark-default-file (pl/mkpath :name "bookmarks")
 package-user-dir (pl/mkpath :name "vendor/packages" :directory t :create t)
 lisp-user-dir (pl/mkpath :name "vendor/lisp" :directory t :create t)
 org-directory (pl/mkpath :name "org-files" :directory t :create t :base "~/")
 custom-file (pl/mkpath :name "custom.el")
 host-file (pl/mkpath :name (format "host-%s.el" (downcase (car (split-string (system-name) "\\."))))))

(setq-default
 show-trailing-whitespace t
 highlight-tabs t
 mode-line-format
 (list
  '(:eval (if (buffer-modified-p)
              (propertize "  %b" 'face 'bold-italic)
            (propertize "  %b" 'face 'bold)))
  " (%l:%c) %p/%I - %m";; (format " %s" minor-mode-alist)
  '(which-function-mode (" " which-func-format))))

;;;; org-mode

(setq
 org-hide-leading-stars t
 org-hide-emphasis-markers t
 org-fontify-done-headline t
 org-src-fontify-natively t
 org-default-notes-file (pl/mkpath :name "notes.org" :create t :base org-directory)
 org-agenda-files (list
                   ;; Add other files here byt duplicating the below line.
                   (pl/mkpath :name "agenda.org" :create t :base org-directory)))

(defun org-font-lock-ensure (beg end)
  "Org font lock ensure from BEG to END."
  (font-lock-ensure))

;;;; autoinsert

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
         " # Description: " _ "\n\n")))

;;;; packages

(yak/pkg 'anzu
         (global-anzu-mode +1)
         (set-face-attribute 'anzu-mode-line nil :foreground "yellow"))

(yak/pkg 'autopair
         (autopair-global-mode t))

(yak/pkg 'browse-kill-ring)

(yak/pkg 'company
         (setq company-auto-complete nil)
         (global-company-mode 1))

(yak/pkg 'cycle-resize)

(yak/pkg 'darkmine-theme
         (load-theme 'darkmine t))

(yak/pkg 'find-file-in-project)
(yak/pkg 'flycheck)

(yak/pkg 'flyspell
         (setq ispell-program-name "aspell")
         (setq ispell-dictionary "english"))

(yak/pkg 'flx-ido)
(yak/pkg 'ido-hacks)
(yak/pkg 'ido-vertical-mode)
(yak/pkg 'ido
         (require 'ido)
         (require 'ido-hacks)
         (setq
          ido-case-fold t
          ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-create-new-buffer 'always
          ido-use-virtual-buffers t)
         (ido-everywhere 1)
         (flx-ido-mode 1)
         (ido-mode t)
         (ido-hacks-mode)
         (ido-vertical-mode))

(yak/pkg 'htmlize)
(yak/pkg 'idle-highlight-mode)
(yak/pkg 'js2-mode)
(yak/pkg 'markdown-mode)
(yak/pkg 'php-extras)
(yak/pkg 'php-mode)
(yak/pkg 'rainbow-delimiters)
(yak/pkg 'rainbow-mode)
(yak/pkg 'ruby-mode)

(yak/pkg 'symon
         (setq symon-delay 5)
         (symon-mode t))

(yak/pkg 'web-mode)

(yak/pkg 'whitespace
         (require 'whitespace)
         (setq whitespace-line-column 80)
         ;; (setq whitespace-style '(tabs tab-mark face lines-tail))
         (setq whitespace-style '(tabs tab-mark face))
         (setq whitespace-global-modes '(not org-mode web-mode))
         (global-whitespace-mode))

(yak/pkg 'yaml-mode)

(yak/repo 'pierre-lecocq/bonjourmadame)

;;;; hooks

(add-hook 'after-init-hook 'global-company-mode)

(defun hook-minibuffer-setup ()
  "Hook for minibuffer."
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook 'hook-minibuffer-setup)

(defun hook-dired-mode ()
  "Hook for dired mode."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "xpf")
          ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice"))))

(add-hook 'dired-mode-hook 'hook-dired-mode)

(defun hook-text-mode ()
  "Hook for text modes."
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format " %d ")
  (flyspell-mode 1))

(add-hook 'text-mode-hook 'hook-text-mode)

(defun hook-prog-mode ()
  "Hook for prog modes."
  (idle-highlight-mode t)
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>")  'hs-hide-block)
  (local-set-key (kbd "C-c <up>")    'hs-hide-all)
  (local-set-key (kbd "C-c <down>")  'hs-show-all)
  (hs-minor-mode t)
  (global-flycheck-mode)
  (rainbow-delimiters-mode))

(add-hook 'prog-mode-hook 'hook-prog-mode)

(defun hook-c-mode-common ()
  "Hook for C modes."
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'hook-c-mode-common)

(defun hook-ruby-mode ()
  "Hook for Ruby mode."
  (global-set-key (kbd "C-c C-r") 'pl/rb-require))

(add-hook 'ruby-mode-hook 'hook-ruby-mode)

(defun hook-php-mode ()
  "Hook for PHP mode."
  ;; (pl/set-locale 'latin-1) ;; Fuck you, PHP. Just Fuck you.
  (require 'php-extras)
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'indent-tabs-mode) nil))

(add-hook 'php-mode-hook 'hook-php-mode)

(defun hook-emacs-lisp-mode ()
  "Hook for Emacs LISP mode."
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'hook-emacs-lisp-mode)

(defun hook-css-mode ()
  "Hook for CSS mode."
  (rainbow-mode))

(add-hook 'css-mode-hook 'hook-css-mode)

(defun hook-makefile-mode ()
  "Hook for Makefiles."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'hook-makefile-mode)

(defun hook-compilation-filter ()
  "Hook for compilation buffers."
  (require 'ansi-color)
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))

(add-hook 'compilation-filter-hook 'hook-compilation-filter)

(defun hook-before-save ()
  "Hook before saving."
  (time-stamp)
  (delete-trailing-whitespace)
  (whitespace-cleanup))

(add-hook 'before-save-hook 'hook-before-save)

(defun hook-find-file ()
  "Hook when finding a file."
  (pl/set-locale 'utf-8)
  (auto-insert))

(add-hook 'find-file-hook 'hook-find-file)

;;;; files

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
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'"       . yaml-mode))

;;;; keybindings

(when (eq system-type 'darwin)
  (setq
   mac-option-modifier nil
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
(global-set-key (kbd "C-S-x C-S-f") 'find-file-in-project)
(global-set-key (kbd "C-S-x k") 'pl/kill-buffers-by-mode)
(global-set-key (kbd "C-M-v") 'cycle-resize-window-vertically)
(global-set-key (kbd "C-M-h") 'cycle-resize-window-horizontally)
(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f12] 'pl/get-shell)
(when window-system
  (global-unset-key (kbd "C-z"))) ;; Fuck you, `suspend-frame'

;;;; extrafiles

(load custom-file 'noerror)
(if (file-exists-p host-file)
    (load host-file)
  (message "No host specific file loaded (%s)" host-file))

;;; emacs.el ends here
