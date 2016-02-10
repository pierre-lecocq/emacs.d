;;; config-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2016-02-10 09:53:51>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs"         (filename . "~/src/emacs.d"))
                       ("Fotolia"       (filename . ,(-secret-path-ftl "/www/fotolia")))
                       ("AdobeStock"    (filename . ,(-secret-path-adb "/www/adobestock")))))

(provide 'config-bookmark)

;;; config-bookmark.el ends here
