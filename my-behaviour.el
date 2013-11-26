;;
;; Development behaviour
;;


;; User
(setq user-full-name "Pierre Lecocq")
(setq user-mail-address "pierre.lecocq@mymail.com")

;; Changelog
(setq change-log-default-name "CHANGELOG")

;; Locale
(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Global
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace t)
(setq-default highlight-tabs t)
(show-paren-mode t)
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Associate files to modes
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

;; Functions
(defun indent-all ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Mode ORG
(setq org-agenda-files (list
                        "~/.emacs.d/org/agenda.org"
                        ;; Add other files here ...
                        ))

;; Mode Ido
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

;; Mode AutoPair
(autopair-global-mode t)

;; Mode AutoComplete
(require 'auto-complete)
(global-auto-complete-mode t)

;; Mode Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Rainbow mode
(add-hook 'css-mode-hook
          (lambda () (rainbow-mode 1)))
