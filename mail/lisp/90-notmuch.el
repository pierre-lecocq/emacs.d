;;; 90-notmuch.el --- Notmuch config

;; Time-stamp: <2016-08-23 17:06:08>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package notmuch :ensure t
  :init (progn

          (when (eq system-type 'darwin) ;; installed via brew
            (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
            (setq exec-path (append exec-path '("/usr/local/bin"))))

          (setq notmuch-search-line-faces '(("unread" :weight bold :background "grey27")
                                            ("flagged" :foreground "yellow")))

          (setq notmuch-show-logo nil
                notmuch-search-oldest-first nil
                notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                         (:name "unread" :query "tag:unread" :key "u")
                                         (:name "flagged" :query "tag:flagged" :key "f")
                                         (:name "sent" :query "tag:sent" :key "t")
                                         (:name "drafts" :query "tag:draft" :key "d")
                                         (:name "all mail" :query "*" :key "a")))))

;;; 90-notmuch.el ends here
