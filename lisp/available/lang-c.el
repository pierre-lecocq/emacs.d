;;; lang-c.el --- C

;; Time-stamp: <2016-12-11 18:25:45>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package cc-mode :ensure t
  :config (setq gdb-many-windows t
                gdb-show-main t))

(use-package company-c-headers :ensure t
  :init (add-to-list 'company-backends 'company-c-headers))

(provide 'lang-c)

;;; lang-c.el ends here
