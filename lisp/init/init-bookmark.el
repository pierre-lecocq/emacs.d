;;; init-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-08 23:00:54>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist '(("Home"          (filename . "~/"))
                       ("Emacs folder"  (filename . "~/src/emacs.d"))
                       ("Fotolia dev"   (filename . "/scp:eqx-dev1:/home/plecocq/www/fotolia"))))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
