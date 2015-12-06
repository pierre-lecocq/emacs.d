;;; my-recentf --- Emacs config - recentf

;; Time-stamp: <2015-12-07 00:02:23>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      recentf-keep '(file-remote-p file-readable-p)
      recentf-save-file (concat config-dir "lisp/files/recentf"))

(add-to-list 'recentf-exclude "lisp/packages")
(add-to-list 'recentf-exclude "lisp/files/ido\\.last")

(provide 'my-recentf)

;;; my-recentf.el ends here
