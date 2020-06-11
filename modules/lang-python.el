;;; lang-python.el --- Lang-Python -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:46:37>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

;; See https://realpython.com/emacs-the-best-python-editor/

(use-package elpy :ensure t :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :commands elpy-enable
  :config (progn
            (setq python-indent-offset 4)
            (when (fboundp 'flycheck-mode)
              (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
              (add-hook 'elpy-mode-hook 'flycheck-mode))))

(provide 'lang-python)

;;; lang-python.el ends here
