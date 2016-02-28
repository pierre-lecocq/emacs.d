;;; pl-recentf --- Emacs config - recentf

;; Time-stamp: <2016-02-29 00:02:51>
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

(provide 'pl-recentf)

;;; pl-recentf.el ends here
