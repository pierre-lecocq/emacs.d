;;; init-snippets.el --- Snippets init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-12-02 13:33:06>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package yasnippet :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets :ensure t)

(provide 'init-snippets)

;;; init-snippets.el ends here
