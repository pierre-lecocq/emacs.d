;;; my-recentf --- Emacs config - recentf

;; Time-stamp: <2015-12-07 10:38:04>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq recentf-auto-cleanup 'never
      recentf-max-menu-items 50
      recentf-keep '(file-remote-p file-readable-p)
      recentf-save-file (concat config-dir-files "recentf"))

(add-to-list 'recentf-exclude config-dir-packages)
(add-to-list 'recentf-exclude config-dir-files)

(provide 'my-recentf)

;;; my-recentf.el ends here
