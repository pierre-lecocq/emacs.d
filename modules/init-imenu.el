;;; init-imenu.el --- Imenu init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-10-01 14:48:48>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package imenu :ensure t
  :bind ("C-c i m" . imenu))

(use-package imenu-list :ensure t
  :init (setq imenu-list-auto-resize t)
  :bind ("C-c i l" . imenu-list))

(provide 'init-imenu)

;;; init-imenu.el ends here
