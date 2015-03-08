;;; 09-keybindings --- Emacs Config - KeyBindings

;;; Commentary:
;; Time-stamp: <2015-02-25 23:39:43 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

(when (eq system-type 'darwin)
  (setq
   mac-option-modifier nil
   mac-command-modifier 'meta
   x-select-enable-clipboard t))

(global-set-key [delete] 'delete-char)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

(global-set-key (kbd "C-S-s") 'find-grep)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-S-n") (lambda() (interactive) (ignore-errors (forward-line 5))))
(global-set-key (kbd "C-S-p") (lambda() (interactive) (ignore-errors (forward-line -5))))
(global-set-key (kbd "C-S-f") (lambda() (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda() (interactive) (ignore-errors (backward-char 5))))

(global-set-key (kbd "C-S-t g") 'find-tag) ;; Goto tag under point
(global-set-key (kbd "C-S-t f") 'tags-apropos) ;; Search tags
(global-set-key (kbd "C-S-t l") 'list-tags) ;; List tags

(global-set-key (kbd "C-S-f") 'imenu) ;; imenu mode

(global-set-key (kbd "C-x o") 'switch-window) ;; switch-window mode

(global-set-key [f4] 'gnus)
(global-set-key [f5] 'bookmark-bmenu-list)
(global-set-key [f6] 'recentf-open-files)
(global-set-key [f7] 'add-change-log-entry-other-window)
(global-set-key [f12] 'pl-get-shell)

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-hook 'ruby-mode-hook
          (lambda()
            (global-set-key  (kbd "C-c C-r") 'pl--rb-require)))

;;; 09-keybindings ends here
