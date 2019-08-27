;;; init-project.el --- Project init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-27 09:32:55>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package projectile :ensure t
  :init (setq projectile-project-search-path '("~/src/")
              projectile-known-projects-file (expand-file-name ".local/files/projectile-bookmarks.eld" user-emacs-directory))
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package ibuffer-projectile :ensure t
  :after (projectile)
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-projectile-set-filter-groups)
                          (unless (eq ibuffer-sorting-mode 'alphabetic)
                            (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-project)

;;; init-project.el ends here
