;;; init-cursors.el --- Multi cursors init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-12-03 10:18:51>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package multiple-cursors :ensure t
  :init (setq mc/list-file (expand-file-name ".cache/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'init-cursors)

;;; init-cursors.el ends here
