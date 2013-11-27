(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Archives

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; El get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq
 el-get-packages
 '(
   auto-complete
   autopair
   buffer-move
   color-theme
   column-marker
   flycheck
   highlight-symbol
   ido-vertical-mode
   js2-mode
   magit
   move-text
   multiple-cursors
   php-mode
   php-mode-improved
   rainbow-mode
   rhtml-mode
   ruby-mode
   shell-pop
   yaml-mode))

(el-get 'sync el-get-packages)

(defun el-get-update-once-a-week()
  (if (= 0 (% 7 (string-to-number (format-time-string "%d"))))
      (el-get-update-all)))

(el-get-update-once-a-week)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init

(setq user-full-name "Pierre Lecocq")
(setq user-mail-address "pierre.lecocq@mymail.com")
(setq change-log-default-name "CHANGELOG")

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace t)
(setq-default highlight-tabs t)
(show-paren-mode t)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Backups

(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Locale

(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Indentation

(setq-default tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              indent-tabs-mode t)
(setq-default indent-tabs-mode nil)

(c-add-style
 "custom-four-indent"
 '((c-offsets-alist
    (arglist-close . 0)
    (arglist-intro . 4)
    (case-label . 4))))

(add-hook 'php-mode-hook
          '(lambda ()
             (setq comment-start "// ")
             (setq comment-end "")
             (set (make-local-variable 'indent-tabs-mode) nil)
             (c-set-style "custom-four-indent")))

(defun pl/indent-all ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; File modes

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup

(setq inhibit-startup-message t)
(setq initial-scratch-message
      (format "* Perkeleen Vittupää (%s)\n" (substring (emacs-version) 10 16)))
(setq frame-title-format "%b - emacs")

;; Modes

(global-font-lock-mode t)
(transient-mark-mode t)
(line-number-mode t)
(column-number-mode t)
(display-time)

;; Decorations

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Transparency

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Themes

(setq pl/available-themes '(
                color-theme-dark-laptop
                color-theme-deep-blue
                color-theme-standard))

(defun pl/theme-switch()
  "Switch between themes"
  (interactive)
  (setq pl/next-theme (pop pl/available-themes))
  (setq pl/available-themes (append pl/available-themes (list pl/next-theme)))
  (message "Switch to theme %s" pl/next-theme)
  (funcall pl/next-theme)
)

;; X mode

(defun x-mode()
  (add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))
  (global-linum-mode t)
  (global-hl-line-mode t)
  (pl/theme-switch)
  ;;(color-theme-dark-laptop)
  (transparency 85))

(if window-system (x-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org mode

(setq org-agenda-files (list
                        "~/.emacs.d/org/agenda.org"
                        ;; Add other files here ...
                        ))

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "Grey55" :strike-through t)))))

;; Ido mode

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))

;; Ido vertical mode

(require 'ido-vertical-mode)
(ido-vertical-mode)

;; AutoPair mode

(autopair-global-mode t)

;; AutoComplete mode

(require 'auto-complete)
(global-auto-complete-mode t)

;; Recentf mode

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Rainbow mode

(add-hook 'css-mode-hook
          (lambda () (rainbow-mode 1)))

;; Shell pop mode

(require 'shell-pop)
(shell-pop-set-internal-mode "shell")
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 50)
(shell-pop-set-window-position "bottom")

;; Move text mode

(require 'move-text)
(move-text-default-bindings)

;; Flycheck mode

(require 'flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-S-s") 'find-grep)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Join lines below

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Steroide moves (from http://whattheemacsd.com/)

(global-set-key (kbd "C-S-n") (lambda() (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda() (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda() (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda() (interactive) (ignore-errors (backward-char 5))))

;; Ido specific TAB behaviour

(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))
(global-set-key (kbd "C-S-x C-S-f") 'ido-find-file-in-tag-files)

;; Tags

(global-set-key (kbd "C-S-t g") 'find-tag) ;; Goto tag under point
(global-set-key (kbd "C-S-t f") 'tags-apropos) ;; Search tags
(global-set-key (kbd "C-S-t l") 'list-tags) ;; List tags

;; Buffer move

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Highlight

(global-set-key [f2] 'highlight-symbol-at-point)
(global-set-key [(control f2)] 'highlight-symbol-next)
(global-set-key [(shift f2)] 'highlight-symbol-prev)
(global-set-key [(meta f2)] 'highlight-symbol-query-replace)

;; F keys

(global-set-key [f4] 'gnus) ;; Gnus
(global-set-key [f5] 'bookmark-bmenu-list) ;; Bookmarks list
(global-set-key [f6] 'recentf-open-files) ;; Recent files history
(global-set-key [f7] 'add-change-log-entry-other-window) ;; Open changelog
(global-set-key [f11] 'pl/theme-switch) ;; Swith theme
(global-set-key [f12] 'shell-pop) ;; Pop shell buffer
