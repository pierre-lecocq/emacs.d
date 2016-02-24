;;; config-newsticker.el --- Emacs configuration - newsticker

;; Time-stamp: <2016-02-24 10:40:02>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-launch-newsticker ()
  "Launch newsticker."
  (interactive)
  (unless (newsticker-running-p)
    (newsticker-start))
  (newsticker-treeview))

(use-package w3m :ensure t
  :commands w3m-region)

(use-package newsticker :ensure t
  :init (progn
          (setq newsticker-retrieval-interval 600 ;; 10mins
                newsticker-html-renderer 'w3m-region
                newsticker-dir (concat files-dir "newsticker")
                newsticker-cache-filename (concat files-dir "newsticker" "/.cache")
                newsticker-url-list-defaults nil
                newsticker-url-list '(("p-emacsen" "http://planet.emacsen.org/atom.xml")
                                      ("r-lisp" "http://www.reddit.com/r/lisp.rss")
                                      ("r-debian" "http://www.reddit.com/r/debian.rss")
                                      ("r-emacs" "http://www.reddit.com/r/emacs.rss")
                                      ("r-netsec" "http://www.reddit.com/r/netsec.rss")
                                      ("r-linux" "http://www.reddit.com/r/linux.rss")
                                      ("r-ruby" "http://www.reddit.com/r/ruby.rss")
                                      ("xkcd" "http://xkcd.com/rss.xml")
                                      ("commit-strip" "http://www.commitstrip.com/fr/feed/")))
          (setq newsticker-groups-filename (concat files-dir "newsticker" "/.groups")
                newsticker-groups '("Feeds"
                                    ("System" "r-linux" "r-debian" "r-netsec")
                                    ("Emacs" "p-emacsen" "r-emacs")
                                    ("Dev" "r-lisp" "r-ruby")
                                    ("Other" "xkcd" "commit-strip"))))
  :bind (("C-c r" . pl-launch-newsticker)))

(provide 'config-newsticker)

;;; config-newsticker.el ends here
