;;; init-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-16 10:39:04>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs"         (filename . "~/src/emacs.d"))
                       ("Fotolia"       (filename . ,(ftl-secret-path "/www/fotolia")))))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
