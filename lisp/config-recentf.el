;;; config-recentf --- Emacs config - recentf

;; Time-stamp: <2016-02-24 10:40:09>
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

(provide 'config-recentf)

;;; config-recentf.el ends here
