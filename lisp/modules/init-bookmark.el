;;; my-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-07 10:36:39>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq bookmark-default-file (concat config-dir-files "bookmarks")
      bookmark-sort-flag nil
      bookmark-alist '(("Home" (filename . "~/"))
                       ("Emacs folder" (filename . "~/src/emacs.d"))
                       ;; ("Qsdfgh home" (filename . "/scp:pierre@qsdfgh.com#38170:~/"))
                       ("Fotolia dev" (filename . "/scp:eqx-dev1:/home/plecocq/www/fotolia"))))

(provide 'my-bookmark)

;;; my-bookmark.el ends here
