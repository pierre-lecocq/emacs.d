;;; feat-news.el --- News feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-06 14:12:29>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package elfeed :ensure t :diminish
  :config (progn
            ;; date format
            (defun elfeed-search-format-date (date)
              (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
            ;; custom titles
            (defadvice elfeed-search-update (before nullprogram activate)
              (mapc (lambda (info)
                      (let ((feed (elfeed-db-get-feed (concat "https://reddit.com/r/" (car info) ".rss"))))
                        (setf (elfeed-feed-title feed) (cadr info))))
                    '(("common_lisp" "/r/CommonLisp")
                      ("emacs" "/r/Emacs")
                      ("linux" "/r/Linux")
                      ("lisp" "/r/Lisp")
                      ("netsec" "/r/Netsec")
                      ("postgresql" "/r/PostgreSQL")
                      ("ruby" "/r/Ruby")))))
  :init (progn
          (setq-default elfeed-search-filter "@1-week-ago +unread ")
          (setq elfeed-feeds
                '(("https://reddit.com/r/common_lisp.rss" dev lisp)
                  ("https://reddit.com/r/emacs.rss" dev emacs)
                  ("https://reddit.com/r/linux.rss" system linux)
                  ("https://reddit.com/r/lisp.rss" dev lisp)
                  ("https://reddit.com/r/netsec.rss" security)
                  ("https://reddit.com/r/postgresql.rss" database)
                  ("https://reddit.com/r/ruby.rss" dev ruby)
                  ("https://www.commitstrip.com/fr/feed/" comic)
                  ("https://xkcd.com/rss.xml" comic)))
          )
  :bind ("C-c n r" . elfeed))

(provide 'feat-news)

;;; feat-news.el ends here
