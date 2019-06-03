;;; lang-python.el --- Pyton language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:47:09>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - pip install flake8

;;; Code:

(use-package elpy :ensure t :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :commands elpy-enable
  :config (progn
            (setq python-indent-offset 4)
            (when (not (require 'yasnippet nil 'noerror))
              (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules)))
            (when (fboundp 'flycheck-mode)
              (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))))

(provide 'lang-python)

;;; lang-python.el ends here
