;;; config-newsticker.el --- Emacs configuration - newsticker

;; Time-stamp: <2016-02-24 08:39:18>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package w3m :ensure t
  :commands w3m-region)

(use-package newsticker :ensure t
  :init (progn
          (setq newsticker-retrieval-interval 600 ;; 10mins
                newsticker-html-renderer 'w3m-region
                newsticker-url-list-defaults nil
                newsticker-url-list '(("Planet Emacsen" "http://planet.emacsen.org/atom.xml" nil nil nil)
                                      ("XKCD" "http://xkcd.com/rss.xml" nil nil nil)))
          (newsticker-start))
  :bind (("C-c r" . newsticker-treeview)))

(provide 'config-newsticker)

;;; config-newsticker.el ends here
