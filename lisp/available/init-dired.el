;;; init-dired.el --- Dired

;; Time-stamp: <2017-04-24 14:19:08>
;; Copyright (C) 2017 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar dired-dotfiles-show-p t)

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if dired-dotfiles-show-p
        (progn
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines)
          (setq dired-dotfiles-show-p nil))
      (progn
        (revert-buffer)
        (setq dired-dotfiles-show-p t)))))

(defun hook-dired-mode ()
  "Hook for Dired mode."
  (put 'dired-find-alternate-file 'disabled nil))

(add-hook 'dired-mode-hook #'hook-dired-mode)

(provide 'init-dired)

;;; init-dired.el ends here
