;;
;; Development behaviour
;;

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

;; Indentation
(setq-default tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil
              indent-tabs-mode t)
(setq-default indent-tabs-mode nil)

(c-add-style
 "custom-php-indent"
 '((c-offsets-alist
    (arglist-close . 0)
    (arglist-intro . 4)
    (case-label . 4))))

(add-hook 'php-mode-hook
      '(lambda ()
         (setq comment-start "// ")
         (setq comment-end "")
         (set (make-local-variable 'indent-tabs-mode) nil)
         (c-set-style "custom-php-indent")))

;; Functions
(defun indent-all ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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

;; Mode AUtoComplete
(require 'auto-complete)
(global-auto-complete-mode t)

;; Mode Recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Mode YAS
(require 'yasnippet)
(yas-global-mode 1)
