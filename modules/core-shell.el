;;; core-shell.el --- Shell -*- lexical-binding: t; -*-

;; Time-stamp: <2020-06-18 18:37:44>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package vterm :ensure t
  :bind ("M-RET" . (lambda ()
                     (interactive)
                     (let ((bname "vterm"))
                       (if (string= (buffer-name) bname)
                           (switch-to-buffer (other-buffer))
                         (if (get-buffer bname)
                             (switch-to-buffer bname)
                           (vterm)))))))

(provide 'core-shell)

;;; core-shell.el ends here
