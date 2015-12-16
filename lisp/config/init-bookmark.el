;;; init-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-16 13:17:56>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs"         (filename . "~/src/emacs.d"))
                       ("Fotolia"       (filename . ,(-secret-path-ftl "/www/fotolia")))))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
