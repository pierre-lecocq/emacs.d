;;
;; Keyboard
;;

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-S-s") 'find-grep)

;; Ido specific TAB behaviour
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [tab] 'ido-complete)))
(global-set-key (kbd "C-x C-M-f") 'ido-find-file-in-tag-files)

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
