;;; pl-lisp.el --- Emacs configuration - lisp

;; Time-stamp: <2016-04-10 17:33:04>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package slime-company :ensure t)

(use-package slime :ensure t
  :init (progn
          ;; Assuming (ql:quickload "quicklisp-slime-helper") has be ran in quicklisp before
          (let ((helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
            (if (file-exists-p helper-file)
                (load helper-file)
              (warn "(ql:quickload \"quicklisp-slime-helper\") must be run in quicklisp before")))

          (if (eq system-type 'darwin)
              (setq inferior-lisp-program "/usr/local/bin/sbcl")
            (setq inferior-lisp-program "sbcl"))

          (slime-setup '(slime-company))))

(provide 'pl-lisp)

;;; pl-lisp.el ends here
