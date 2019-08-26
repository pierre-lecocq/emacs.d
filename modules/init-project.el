;;; init-project.el --- Project init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-26 15:57:16>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-known-projects-file (expand-file-name ".local/files/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  :bind (("C-c p C-r" . projectile-replace-regexp)
         ("C-c p C-f" . projectile-find-regexp)))

(provide 'init-project)

;;; init-project.el ends here
