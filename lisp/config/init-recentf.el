;;; init-recentf --- Emacs config - recentf

;; Time-stamp: <2015-12-08 23:01:55>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'recentf)

(recentf-mode 1)

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      recentf-keep '(file-remote-p file-readable-p)
      recentf-save-file (concat files-dir "recentf"))

(add-to-list 'recentf-exclude packages-dir)
(add-to-list 'recentf-exclude files-dir)

(provide 'init-recentf)

;;; init-recentf.el ends here
