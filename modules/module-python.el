;;; module-python.el --- Pyton feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:01:15>
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

(provide 'module-python)

;;; module-python.el ends here
