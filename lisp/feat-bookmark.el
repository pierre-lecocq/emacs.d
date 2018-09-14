;;; feat-bookmark.el --- Bookmark support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:26:26>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package bookmark :demand t :ensure nil
  :bind (("<f10>" . bookmark-bmenu-list))
  :init (setq bookmark-default-file (concat (file-name-directory load-file-name) "../local/my-bookmarks.el")
              bookmark-sort-flag nil
              bookmark-alist '(("home"     (filename . "~/"))
                               ("emacs.d"  (filename . "~/src/emacs.d/"))
                               ("sources"  (filename . "~/src/")))))

;;; feat-bookmark.el ends here
