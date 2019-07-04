;;; feat-news.el --- News feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-04 22:40:56>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package elfeed :ensure t :diminish
  :config (defun elfeed-search-format-date (date)
            (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
  :init (progn
          (setq-default elfeed-search-filter "@1-week-ago +unread ")
          (setq elfeed-feeds
                '(("https://reddit.com/r/common_lisp.rss" lisp)
                  ("https://reddit.com/r/emacs.rss" emacs)
                  ("https://reddit.com/r/linux.rss" linux)
                  ("https://reddit.com/r/lisp.rss" lisp)
                  ("https://reddit.com/r/netsec.rss" security)
                  ("https://reddit.com/r/postgresql.rss" database)
                  ("https://reddit.com/r/ruby.rss" ruby))))
  :bind ("C-c n r" . elfeed))

(provide 'feat-news)

;;; feat-news.el ends here
