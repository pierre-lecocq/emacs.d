;;; my-bookmark.el --- Emacs config - bookmark

;; Time-stamp: <2015-12-06 22:17:21>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq bookmark-default-file (expand-file-name (concat config-dir "bookmarks")))
(setq bookmark-sort-flag nil)
(setq bookmark-alist '(("Home" (filename . "~/"))
                       ("Emacs folder" (filename . "~/src/emacs.d"))
                       ;; ("Qsdfgh home" (filename . "/scp:pierre@qsdfgh.com#38170:~/"))
                       ("Fotolia dev" (filename . "/scp:eqx-dev1:/home/plecocq/www/fotolia"))))

(provide 'my-bookmark)

;;; my-bookmark.el ends here
