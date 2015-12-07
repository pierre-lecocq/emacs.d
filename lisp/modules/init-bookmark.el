;;; init-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-07 22:53:16>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq bookmark-default-file (concat config-dir-files "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist '(("Home"          (filename . "~/"))
                       ("Emacs folder"  (filename . "~/src/emacs.d"))
                       ("Fotolia dev"   (filename . "/scp:eqx-dev1:/home/plecocq/www/fotolia"))))

(provide 'init-bookmark)

;;; init-bookmark.el ends here
