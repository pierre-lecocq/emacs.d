;;; 50-recentf.el --- Recentf

;; Time-stamp: <2016-06-29 10:32:13>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'recentf)

(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      recentf-keep '(file-remote-p file-readable-p))

(add-to-list 'recentf-exclude package-user-dir)
(add-to-list 'recentf-exclude files-dir)

(provide '50-recentf)

;;; 50-recentf.el ends here
