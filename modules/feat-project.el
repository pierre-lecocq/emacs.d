;;; feat-project.el --- Project feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 14:55:44>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/" "~/src/mass")
              projectile-known-projects-file (expand-file-name ".local/files/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(provide 'feat-project)

;;; feat-project.el ends here
