;;; 02-functions.el --- Emacs Config - Functions

;;; Commentary:
;; Time-stamp: <2015-02-26 13:49:09 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

(defun pl--init-indentation ()
  "Initialize indentation rules."
  (unless (string= major-mode "GNUmakefile")
    (setq-default
     tab-width 4
     c-basic-offset 4
     c-hanging-comment-ender-p nil
     indent-tabs-mode nil))
  (c-add-style
   "custom-four-indent"
   '((c-offsets-alist
      (arglist-close . 0)
      (arglist-intro . 4)
      (case-label . 4)))))

(defun pl--init-hooks ()
  "Initialize hooks."
  ;; Text modes
  (add-hook 'org-mode-hook 'pl--linum-mode)
  (add-hook 'markdown-mode-hook 'pl--linum-mode)
  (add-hook 'text-mode-hook 'pl--linum-mode)
  ;; Find file hook
  (add-hook 'find-file-hook
            '(lambda ()
               ;; Set locale (a.k.a FUCK you, PHP)
               (if (string= major-mode "php-mode")
                   (pl--set-locale 'latin-1)
                 (pl--set-locale 'utf-8))
               ;; Special indentation
               (if (and buffer-file-name
                        (string-match "/gnulib\\>" (buffer-file-name))
                        (not (string-equal mode-name "Change Log"))
                        (not (string-equal mode-name "Makefile")))
                   (setq indent-tabs-mode nil)))))

(defun pl--init-look-and-feel ()
  "Initialize look and feel."
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Bars
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  ;; Internal modes
  (show-paren-mode t)
  (global-font-lock-mode t)
  (transient-mark-mode t)
  (line-number-mode t)
  (column-number-mode t)
  (global-auto-revert-mode 1)

  ;; Theme
  (load-theme 'darkmine t)

  (when window-system
    (set-fringe-mode '(1 . 1)))

  ;; Mode line
  (which-function-mode)
  (setq-default
   mode-line-format
   (list
    '(:eval (if (buffer-modified-p)
                (propertize "  %b" 'face 'bold-italic)
              (propertize "  %b" 'face 'bold)))
    " (%l:%c)"
    " %p/%I -"
    " %m";; (format " %s" minor-mode-alist)
    '(which-function-mode (" " which-func-format))))

  ;; Font
  (when (member "Inconsolata-g" (font-family-list))
    (set-face-attribute 'default nil :font "Inconsolata-g-10"))

  ;; Spaces, tabs and new lines
  (setq-default show-trailing-whitespace t)
  (setq-default highlight-tabs t)
  (setq require-final-newline t)
  (setq next-line-add-newlines nil)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook (lambda() (delete-trailing-whitespace))))

(defun pl--init-files-modes ()
  "Initialize files modes."
  (add-to-list 'auto-mode-alist '(".bashrc" . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".zshrc" . shell-script-mode))
  (add-to-list 'auto-mode-alist '(".gnus" . emacs-lisp-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".rake" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".ru" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".gemspec" . ruby-mode))
  (add-to-list 'auto-mode-alist '(".erb" . rhtml-mode))
  (add-to-list 'auto-mode-alist '(".erubis" . rhtml-mode))
  (add-to-list 'auto-mode-alist '(".ya?ml" . yaml-mode))
  (add-to-list 'auto-mode-alist '(".js" . js2-mode))
  (add-to-list 'auto-mode-alist '(".json" . js2-mode))
  (add-to-list 'auto-mode-alist '(".html?" . web-mode)))

(defun pl--set-locale (locale)
  "Initialize locale.
Argument LOCALE the locale to set."
  (set-language-environment locale)
  (set-terminal-coding-system locale)
  (setq locale-coding-system locale)
  (set-default-coding-systems locale)
  (set-selection-coding-system locale)
  (prefer-coding-system locale))

(defun pl--transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun pl--linum-mode()
  (interactive)
  (linum-mode 1)
  (make-local-variable 'linum-format)
  (setq linum-format "  %d "))

(defun pl--get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq nil (get-buffer "*shell*"))
      (shell)
    (switch-to-buffer "*shell*")))

(defun pl--rb-require ()
  "Insert required rubygems."
  (interactive "*")
  (let ((gems (read-from-minibuffer "Rubygems to require: ")))
    (when gems
      (mapcar (lambda (gem)
                (insert (format "require \"%s\"\n" gem)))
              (split-string gems nil t)))))

;;; 02-functions.el ends here
