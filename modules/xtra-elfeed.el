;;; xtra-elfeed.el --- ElFeed -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-21 10:59:13>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package elfeed :ensure t
  :init (setq elfeed-feeds
              '(("https://www.reddit.com/r/emacs/.rss" emacs reddit)
                ("https://sachachua.com/blog/feed/" emacs blog)
                ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs youtube)
                ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs youtube)
                ("https://www.youtube.com/feeds/videos.xml?channel_id=UCs_tLP3AiwYKwdUHpltJPuA" talks programming youtube)
                ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8butISFwT-Wl7EV0hUK0BQ" programming youtube)))
  :bind ("C-c e" . elfeed))

(provide 'xtra-elfeed)

;;; xtra-elfeed.el ends here
