;;; config-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2016-01-20 09:03:33>
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

(provide 'config-bookmark)

;;; config-bookmark.el ends here
