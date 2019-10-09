;;; init-git.el --- Git init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-10-09 08:28:22>
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

(use-package magit :ensure t
  :init (setq transient-history-file (expand-file-name ".local/files/transient-history.el" user-emacs-directory)
              transient-levels-file (expand-file-name ".local/files/transient-levels.el" user-emacs-directory)
              transient-values-file (expand-file-name ".local/files/transient-values.el" user-emacs-directory))
  :bind (("C-c g s" . magit-status)))

(use-package dired-git-info :ensure t
  :config (progn
            (setq dgi-commit-message-format "%h %s\t%cr\t%an")
            (with-eval-after-load 'dired
              (define-key dired-mode-map "?" 'dired-git-info-mode))))

(provide 'init-git)

;;; init-git.el ends here
