;;
;; Keyboard
;;

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-S-s") 'find-grep)

;; Join lines below
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Steroide moves (from http://whattheemacsd.com/)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))

;; Ido specific TAB behaviour
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))
(global-set-key (kbd "C-S-x C-S-f") 'ido-find-file-in-tag-files)

;; Git
(global-set-key (kbd "C-S-g s") 'magit-status)

;; Buffer move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; F keys
(global-set-key [f5] 'bookmark-bmenu-list) ;; Bookmarks list
(global-set-key [f6] 'recentf-open-files) ;; Recent files history
(global-set-key [f7] 'add-change-log-entry-other-window) ;; Open changelog
(global-set-key [f12] 'shell-pop) ;; Pop shell buffer

;; Shell specific keys
(defun my-shell-keys ()
  (local-set-key (quote [(return)]) (quote newline))
  (local-set-key (quote [(tab)])  'indent-or-complete))
(add-hook 'term-exec-hook 'my-shell-keys)
