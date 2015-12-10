;;; init-keybindings.el --- Emacs config - keybindings

;; Time-stamp: <2015-12-10 19:48:05>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

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
    (other-window 1)))

(provide 'init-keybindings)

;;; init-keybindings.el ends here
