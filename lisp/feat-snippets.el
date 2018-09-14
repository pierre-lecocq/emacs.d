;;; feat-snippets.el --- Snippets support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:39:07>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(let ((snippets-dir (concat (file-name-directory load-file-name) "../snippets")))
  (use-package yasnippet :ensure t
    :if (file-accessible-directory-p snippets-dir)
    :diminish yas-minor-mode
    :config (yas-global-mode 1)
    :init (setq yas-snippet-dirs '(snippets-dir))))

;;; feat-snippets.el ends here
