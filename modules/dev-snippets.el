;;; dev-snippets.el --- Snippets -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:17:14>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package yasnippet :ensure t
  :after company
  :config (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t
  :after yasnippet)

(provide 'dev-snippets)

;;; dev-snippets.el ends here
