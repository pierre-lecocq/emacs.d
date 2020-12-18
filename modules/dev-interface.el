;;; dev-interface.el --- Interface -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-18 11:01:52>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package imenu :ensure t
  :bind ("C-c i m" . imenu))

(use-package imenu-list :ensure t
  :config (imenu-list-minor-mode)
  :bind ("C-c i l" . imenu-list))

(use-package all-the-icons :ensure t) ;; M-x all-the-icons-install-fonts

(use-package treemacs :ensure t :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (treemacs-git-mode 'deferred)
            (treemacs-resize-icons 16))
  :bind ("C-c f t" . treemacs))

(use-package treemacs-projectile :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit :ensure t
  :after (treemacs magit))

(provide 'dev-interface)

;;; dev-interface.el ends here
