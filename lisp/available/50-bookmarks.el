;;; 50-bookmark.el --- Bookmark

;; Time-stamp: <2016-07-21 08:37:46>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs.d"         (filename . "~/src/emacs.d"))))

(provide '50-bookmark)

;;; 50-bookmark.el ends here
