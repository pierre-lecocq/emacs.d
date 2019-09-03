;;; init-git.el --- Git init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-09-02 16:53:30>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t
  :init (setq git-gutter:modified-sign "~")
  :config (global-git-gutter-mode +1))

(use-package git-messenger :ensure t
  :init (setq git-messenger:show-detail t)
  :bind (("C-c g m" . git-messenger:popup-message)
         ("C-c g v" . git-messenger:popup-show-verbose)))

(provide 'init-git)

;;; init-git.el ends here
