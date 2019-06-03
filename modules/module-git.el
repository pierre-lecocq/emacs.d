;;; module-git.el --- Git module -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:40:48>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t :diminish
  :config (global-git-gutter-mode +1)
  :custom-face (git-gutter:modified ((t (:foreground "yellow")))))

(use-package git-messenger :ensure t :diminish
  :init (setq git-messenger:show-detail t)
  :bind (("C-c v m" . git-messenger:popup-message)))

(provide 'module-git)

;;; module-git.el ends here
