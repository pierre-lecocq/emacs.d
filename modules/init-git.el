;;; init-git.el --- Git init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-26 16:02:24>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t
  :config (global-git-gutter-mode +1)
  :custom-face (git-gutter:modified ((t (:foreground "yellow")))))

(use-package git-messenger :ensure t
  :init (setq git-messenger:show-detail t)
  :bind (("C-c g m" . git-messenger:popup-message)
         ("C-c g v" . git-messenger:popup-show-verbose)))

(provide 'init-git)

;;; init-git.el ends here
