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
      recentf-save-file (concat config-dir-files "recentf"))

(add-to-list 'recentf-exclude config-dir-packages)
(add-to-list 'recentf-exclude config-dir-files)

(provide 'init-recentf)

;;; init-recentf.el ends here
