;;; my-recentf --- Emacs config - recentf

;; Time-stamp: <2015-12-06 22:19:07>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq recentf-save-file (concat config-dir "lisp/files/recentf"))

(add-to-list 'recentf-exclude "lisp/packages")
(add-to-list 'recentf-exclude "lisp/files/ido\\.last")

(provide 'my-recentf)

;;; my-recentf.el ends here
