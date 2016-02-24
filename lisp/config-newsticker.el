;;; config-newsticker.el --- Emacs configuration - newsticker

;; Time-stamp: <2016-02-24 09:33:39>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package w3m :ensure t
  :commands w3m-region)

(use-package newsticker :ensure t
  :init (progn
          (setq newsticker-retrieval-interval 600 ;; 10mins
                newsticker-html-renderer 'w3m-region
                newsticker-dir (concat files-dir "newsticker")
                newsticker-cache-filename (concat files-dir "newsticker" "/.cache")
                newsticker-url-list-defaults nil
                newsticker-url-list '(("Planet Emacsen" "http://planet.emacsen.org/atom.xml")
                                      ("/r/lisp ""http://www.reddit.com/r/lisp.rss")
                                      ("/r/debian ""http://www.reddit.com/r/debian.rss")
                                      ("/r/emacs" "http://www.reddit.com/r/emacs.rss")
                                      ("/r/netsec" "http://www.reddit.com/r/netsec.rss")
                                      ("/r/linux" "http://www.reddit.com/r/linux.rss")
                                      ("/r/ruby" "http://www.reddit.com/r/ruby.rss")
                                      ("XKCD" "http://xkcd.com/rss.xml")))
          (newsticker-start))
  :bind (("C-c r" . newsticker-treeview)))

(provide 'config-newsticker)

;;; config-newsticker.el ends here
