;;; 50-shell.el --- Shell

;; Time-stamp: <2016-09-01 11:16:22>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*shell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (if (member (get-buffer "*shell*") (buffer-list))
        (switch-to-buffer "*shell*")
      (shell))))

(defun hook-shell-mode ()
  "Hook for Shell mode."
  (setq show-trailing-whitespace nil))

(add-hook 'shell-mode-hook #'hook-shell-mode)

(use-package better-shell :ensure t
  :bind (("<f12>" . better-shell-shell)
         ("C-<f12>" . better-shell-remote-open)))

(provide '50-shell)

;;; 50-shell.el ends here
