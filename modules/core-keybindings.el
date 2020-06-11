;;; core-keybindings.el --- Keybindings -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:15:10>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t)
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)))

(global-set-key [delete] 'delete-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c r") 'comment-dwim)
;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-o") 'other-window)

(advice-add 'split-window-right :after #'(lambda (&rest _) (other-window 1)))
(advice-add 'split-window-below :after #'(lambda (&rest _) (other-window 1)))

(provide 'core-keybindings)

;;; core-keybindings.el ends here
