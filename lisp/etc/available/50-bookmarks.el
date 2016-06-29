;;; 50-bookmark.el --- Bookmark

;; Time-stamp: <2016-06-13 09:49:22>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'bookmark)

(setq bookmark-sort-flag nil
      bookmark-alist `(("Home"          (filename . "~/"))
                       ("Sources"       (filename . "~/src"))
                       ("Emacs"         (filename . "~/src/emacs.d"))
                       ("Raspberry"     (filename . "/scp:kenny:~/"))
                       ("Qsdfgh"        (filename . "/scp:qsdfgh:~/"))
                       ("Fotolia"       (filename . "/scp:eqx-dev1:~/www/fotolia"))
                       ("AdobeStock"    (filename . "/scp:eqx-dev2:~/www/adobestock"))))

;;; 50-bookmark.el ends here
