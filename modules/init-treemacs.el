;;; init-treemacs.el --- Treemacs init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-12-03 10:08:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package treemacs :ensure t :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (treemacs-git-mode 'deferred)
            (treemacs-resize-icons 16))
  :bind (("C-c f t" . treemacs)))

(use-package treemacs-projectile :ensure t
  :after treemacs projectile)

(use-package treemacs-magit :ensure t
  :after treemacs magit)

(provide 'init-treemacs)

;;; init-treemacs.el ends here
