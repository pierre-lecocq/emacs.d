;;; init-shell.el --- Emacs configuration - shell

;; Time-stamp: <2015-12-08 23:52:36>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'eshell)
(require 'em-alias)

(defun pl-get-shell ()
  "Get a shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer "*eshell*"))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (progn
      (if (member (get-buffer "*eshell*") (buffer-list))
          (switch-to-buffer "*eshell*")
        (eshell)))))

(defun hook-shell-mode ()
  "Hook for Shell mode."
  (setq show-trailing-whitespace nil)
  (eshell/alias "l" "ls -l")
  (eshell/alias "la" "ls -la"))

(add-hook 'shell-mode-hook #'hook-shell-mode)
(add-hook 'eshell-mode-hook #'hook-shell-mode)

(provide 'init-shell)

;;; init-shell.el ends here
