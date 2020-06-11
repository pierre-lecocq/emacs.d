;;; dev-projects.el --- Projects -*- lexical-binding: t; -*-

;; Time-stamp: <2020-06-11 15:27:06>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory)
              projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package ibuffer-projectile :ensure t
  :after projectile
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-projectile-set-filter-groups))))

(provide 'dev-projects)

;;; dev-projects.el ends here
