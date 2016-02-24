;;; config-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2016-02-24 10:39:34>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-default-file (concat files-dir "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs"         (filename . "~/src/emacs.d"))
                       ("Kenny"         (filename . "/scp:kenny:/home/pi"))
                       ("Qsdfgh"        (filename . ,(-secret-path-qsd "/home/www")))
                       ("Fotolia"       (filename . ,(-secret-path-ftl "/www/fotolia")))
                       ("AdobeStock"    (filename . ,(-secret-path-adb "/www/adobestock")))))

(provide 'config-bookmark)

;;; config-bookmark.el ends here
