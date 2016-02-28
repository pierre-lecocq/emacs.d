;;; pl-lisp.el --- Emacs configuration - lisp

;; Time-stamp: <2016-02-29 00:02:01>
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

(provide 'pl-lisp)

;;; pl-lisp.el ends here
