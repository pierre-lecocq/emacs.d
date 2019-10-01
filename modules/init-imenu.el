;;; init-imenu.el --- Imenu init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-10-01 15:33:39>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package imenu :ensure t
  :bind ("C-c i m" . imenu))

(use-package imenu-list :ensure t
  :init (setq imenu-list-auto-resize t)
  :config (imenu-list-minor-mode)
  :bind ("C-c i l" . imenu-list))

(provide 'init-imenu)

;;; init-imenu.el ends here
