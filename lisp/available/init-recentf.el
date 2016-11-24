;;; init-recentf.el --- Recentf

;; Time-stamp: <2016-11-24 10:04:49>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'recentf)

(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      ;; recentf-keep '(file-remote-p file-readable-p)
      )

(add-to-list 'recentf-exclude package-user-dir)
(add-to-list 'recentf-exclude files-dir)

(provide 'init-recentf)

;;; init-recentf.el ends here
