;;; init-multicursors.el --- Multi cursors init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-22 15:19:02>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package multiple-cursors :ensure t
  :init (setq mc/list-file (expand-file-name ".local/files/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'init-multicursors)

;;; init-multicursors.el ends here
