;;; feat-project.el --- Project feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-01 14:01:26>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-known-projects-file (expand-file-name ".local/files/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(provide 'feat-project)

;;; feat-project.el ends here
