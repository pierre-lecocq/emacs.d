;;; init-lisp.el --- Emacs configuration - lisp

;; Time-stamp: <2015-12-08 00:00:38>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package slime-company :ensure t)

(use-package slime :ensure t
  :init (progn
          (if (eq system-type 'darwin)
              (setq inferior-lisp-program "/usr/local/bin/sbcl")
            (setq inferior-lisp-program "sbcl"))
          (slime-setup '(slime-company))))

(provide 'init-lisp)

;;; init-lisp.el ends here
