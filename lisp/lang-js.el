;;; lang-js.el --- Javascript language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:19:55>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package js2-refactor :ensure t :defer t)

(use-package js2-mode :ensure t
  :after (:all js2-refactor)
  :mode "\\.js\\'")

(defun hook-js2-mode ()
  "Hook for js2 mode."
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t))

(add-hook 'js2-mode-hook #'hook-js2-mode)

;;; lang-js.el ends here
