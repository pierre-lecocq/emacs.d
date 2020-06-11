;;; dev-git.el --- Git -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:14:39>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t
  :config (global-git-gutter-mode +1)
  :custom ((git-gutter:added-sign " ")
           (git-gutter:modified-sign " ")
           (git-gutter:deleted-sign " ")))

(use-package git-messenger :ensure t
  :init (setq git-messenger:show-detail t)
  :bind (("C-c g m" . git-messenger:popup-message)
         ("C-c g v" . git-messenger:popup-show-verbose)))

(use-package magit :ensure t
  :init (setq transient-history-file (expand-file-name ".cache/transient-history.el" user-emacs-directory)
              transient-levels-file (expand-file-name ".cache/transient-levels.el" user-emacs-directory)
              transient-values-file (expand-file-name ".cache/transient-values.el" user-emacs-directory))
  :bind (("C-c g s" . magit-status)))

(provide 'dev-git)

;;; dev-git.el ends here
