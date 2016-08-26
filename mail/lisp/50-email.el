;;; 50-email.el --- Email configuraton

;; Time-stamp: <2016-08-26 16:22:28>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package notmuch :ensure t
  :init (progn
          (when (eq system-type 'darwin) ;; installed via brew
            (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
            (setq exec-path (append exec-path '("/usr/local/bin"))))
          (setq notmuch-search-line-faces '(("unread" :weight bold :background "grey27")
                                            ("inbox" :weight bold :background "grey27")
                                            ("flagged" :foreground "yellow")))
          (setq notmuch-show-logo nil
                notmuch-search-oldest-first nil
                notmuch-message-headers '("Subject" "To" "Cc" "Bcc" "Date")
                notmuch-tag-formats '(("unread" (propertize tag 'face '(:foreground "red")))
                                      ("flagged" (notmuch-tag-format-image-data tag (notmuch-tag-star-icon))))
                notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                         (:name "all mail" :query "*" :key "a")
                                         (:name "unread" :query "tag:unread" :key "u")
                                         (:name "flagged" :query "tag:flagged" :key "f")
                                         (:name "sent" :query "tag:sent" :key "t")
                                         (:name "drafts" :query "tag:draft" :key "d")
                                         (:name "gmail" :query "tag:gmail" :key "g")
                                         (:name "qsdfgh" :query "tag:qsdfgh" :key "q"))))
  :config (progn
            (defmacro notmuch-toggle-tag (key tag-to-toggle &optional other-tags)
              `(define-key notmuch-show-mode-map ,key
                 (lambda ()
                   (interactive)
                   (notmuch-show-tag (if (member ,tag-to-toggle (notmuch-show-get-tags))
                                         (concat "-" ,tag-to-toggle)
                                       (concat "+" ,tag-to-toggle)))
                   (when ,other-tags
                     (notmuch-show-tag ,other-tags))
                   (notmuch-bury-or-kill-this-buffer)
                   (notmuch-refresh-this-buffer)))
              `(define-key notmuch-search-mode-map ,key
                 (lambda ()
                   (interactive)
                   (notmuch-show-tag (if (member ,tag-to-toggle (notmuch-show-get-tags))
                                         (concat "-" ,tag-to-toggle)
                                       (concat "+" ,tag-to-toggle)))
                   (when ,other-tags
                     (notmuch-show-tag ,other-tags))
                   (next-line))))
            (notmuch-toggle-tag "F" "flagged" (list "-inbox"))
            (notmuch-toggle-tag "D" "deleted" (list "-inbox" "-unread")))
  :bind (("C-c m" . notmuch)))

(defun setSMTPServer (name addr)
  "Set a SMTP server accorging to its NAME and ADDR."
  (interactive)
  (message "Select %s SMTP server" name)
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-sendmail-extra-arguments (list "-a" name)
        user-full-name "Pierre Lecocq"
        user-mail-address addr
        mail-host-address (replace-regexp-in-string ".*@" "" addr)))

(defun hook-message-mode ()
  "Hook for message mode."
  (setq show-trailing-whitespace nil
        mail-user-agent 'message-user-agent
        message-citation-line-format "On %a, %d %b %Y, %f wrote:"
        message-citation-line-function 'message-insert-formatted-citation-line)
  (let ((smtp (ido-completing-read "Select a SMTP server: " '("gmail" "qsdfgh"))))
    (cond
     ((string= smtp "qsdfgh") (setSMTPServer "qsdfgh" "pemacsmail1@gmail.com"))
     ((string= smtp "gmail") (setSMTPServer "gmail" "pemacsmail2@gmail.com"))
     (t (error "Unknown SMTP server")))))

(add-hook 'message-mode-hook #'hook-message-mode)

;;; 50-email.el ends here
