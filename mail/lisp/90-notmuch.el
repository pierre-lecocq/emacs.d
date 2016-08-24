;;; 90-notmuch.el --- Notmuch config

;; Time-stamp: <2016-08-24 14:15:28>
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
                                         (:name "all mail" :query "*" :key "a")))

          ;; (define-key notmuch-show-mode-map "F" (lambda () (notmuch-show-tag (list "+flagged"))))
          ;; (define-key notmuch-search-mode-map "F" (lambda () (notmuch-show-tag (list "+flagged"))))

          ;; (define-key notmuch-show-mode-map "S" (lambda () (notmuch-show-tag (list "+spam" "-inbox"))))
          ;; (define-key notmuch-search-mode-map "S" (lambda () (notmuch-show-tag (list "+spam" "-inbox"))))
          )
  :bind (("C-c m" . notmuch)))

;;; 90-notmuch.el ends here
