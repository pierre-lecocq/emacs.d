;;; init-elfeed.el --- Emacs configuration - elfeed

;; Time-stamp: <2015-12-07 15:43:10>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package elfeed
  :ensure t
  :init (progn
          (setq elfeed-feeds
                '(("http://planet.emacsen.org/atom.xml" emacs)
                  ("http://planet.debian.org/rss20.xml" debian)
                  ("http://www.securityfocus.com/rss/vulnerabilities.xml" security)
                  ("http://www.reddit.com/r/debian.rss" debian)
                  ("http://www.reddit.com/r/emacs.rss" emacs)
                  ("http://www.reddit.com/r/netsec.rss" security)
                  ("http://www.reddit.com/r/linux.rss" linux)
                  ("http://www.reddit.com/r/ruby.rss" ruby)
                  ("https://www.ruby-lang.org/en/feeds/news.rss" ruby)
                  ("http://xkcd.com/rss.xml" humour)))))

(provide 'init-elfeed)

;;; init-elfeed.el ends here
