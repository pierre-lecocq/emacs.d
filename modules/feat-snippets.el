;;; feat-snippets.el --- Snippets feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-04 23:18:43>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(provide 'feat-snippets)

;;; feat-snippets.el ends here
