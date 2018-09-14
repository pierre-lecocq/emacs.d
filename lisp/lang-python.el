;;; lang-python.el --- Python language support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:42:21>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((python-mode . "Python program") nil
               "#!/usr/bin/env python\n\n"))

(use-package elpy :ensure t :defer t
  :init (with-eval-after-load 'python (elpy-enable))
  :commands elpy-enable
  :config  (progn (setq python-indent-offset 4)
                  (when (fboundp 'flycheck-mode)
                    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))))

;;; lang-python.el ends here
