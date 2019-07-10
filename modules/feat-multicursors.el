;;; feat-multicursors.el --- Multi cursors feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-03 23:19:52>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package multiple-cursors :ensure t :diminish
  :init (setq mc/list-file (expand-file-name ".local/files/mc-lists.el" user-emacs-directory))
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

(provide 'feat-multicursors)

;;; feat-multicursors.el ends here
