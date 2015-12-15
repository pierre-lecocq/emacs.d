;;; init-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-15 16:43:02>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist '(("Home"          (filename . "~/"))
                       ("Emacs folder"  (filename . "~/src/emacs.d"))
                       ("Fotolia dev"   (filename . (concat ftl-path-prefix "/www/fotolia")))))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
