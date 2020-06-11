;;; core-scratch.el --- Scratch -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:15:55>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun open-persistent-notes-buffer ()
  "Open persistent notes buffer."
  (interactive)
  (let ((buf (get-buffer-create "*notes*")))
    (switch-to-buffer buf)
    (markdown-mode)
    (ignore-errors
      (persistent-scratch-restore))))

(defun persistent-notes-buffer-p ()
  "Return non-nil if the current buffer's name is *notes*."
  (string= (buffer-name) "*notes*"))

(use-package persistent-scratch :ensure t
  :custom ((persistent-scratch-save-file (expand-file-name ".cache/persistent-notes" user-emacs-directory))
           (persistent-scratch-scratch-buffer-p-function #'persistent-notes-buffer-p))
  :config (add-hook 'kill-emacs-hook #'persistent-scratch-save)
  :bind ("C-c n" . open-persistent-notes-buffer))

(provide 'core-scratch)

;;; core-scratch.el ends here
