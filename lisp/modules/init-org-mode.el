;;; init-org-mode.el --- Emacs configuration - org-mode

;; Time-stamp: <2015-12-07 23:23:34>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package org :ensure t
  :init (progn
          ;; (setq org-directory (expand-file-name "~/org-files/")
          ;;       org-default-notes-file (expand-file-name (concat org-directory "notes.org"))
          ;;       org-agenda-files (expand-file-name (concat org-directory "agenda.org")))
          (setq org-hide-leading-stars t
                org-hide-emphasis-markers t
                org-fontify-done-headline t
                org-src-fontify-natively t)))

(provide 'init-org-mode)

;;; init-org-mode.el ends here
