;;; gnus.el --- Gnus config file

;; Time-stamp: <2016-07-29 09:03:04>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'gnus-art)
(require 'message)
(require 'starttls)

;; Variables
(setq-default user-full-name "Pierre Lecocq"
              user-mail-address "pierre@qsdfgh.com"
              gnus-posting-styles '((".*" (signature "\n\P.\n")))
              read-mail-command 'gnus
              gnus-select-method '(nnml "")
              gnus-large-newsgroup 'nil ;; No expiration
              gnus-fetch-old-headers 'some ;; Load read messages
              mm-text-html-renderer 'w3m
              gnus-inhibit-images nil
              gnus-ignored-newsgroups ""
              gnus-topic-display-empty-topics nil
              gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
              gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
              gnus-parameters '(("nnimap.*"
                                 (display . 300) ;; (display . all)
                                 (gnus-use-scoring nil)
                                 (expiry-wait . 5))))

(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "\nOn %a, %b %d %Y, %f wrote:\n"
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-ignored-mime-types '("text/x-vcard")
      starttls-use-gnutls t
      ;; starttls-extra-arguments '("--insecure")
      starttls-gnutls-program "gnutls-cli")

;; 72 cols
(unless (boundp 'message-fill-column)
  (add-hook 'message-mode-hook (lambda () (setq fill-column 72) (turn-on-auto-fill))))

;; Windows
(gnus-add-configuration
 '(article (horizontal 1.0
                       (vertical 55 (group 1.0))
                       (vertical 1.0 (summary 0.16 point) (article 1.0)))))

(gnus-add-configuration
 '(summary (horizontal 1.0
                       (vertical 55 (group 1.0))
                       (vertical 1.0 (summary 1.0 point)))))

;; Autorefresh 1 mn
(gnus-demon-add-handler 'gnus-demon-scan-news 1 nil)

;; Split
(setq nnimap-split-inbox '("INBOX")
      nnimap-split-predicate "UNDELETED"
      nnimap-split-crosspost nil
      nnmail-split-methods
      '(("Emacs" "^.*emacs-devel@gnu.org")
        ("Github" "^From:.*@github.com")
        ("Security" "^.*debian-security@lists.debian.org")
        ("Misc" "")))

;; IMAP
(setq gnus-secondary-select-methods
      '((nnimap "Qsdfgh"
                (nnimap-stream ssl)
                (nnimap-address "mail.qsdfgh.com")
                (nnimap-inbox "INBOX")
                (nnimap-split-methods default)
                (nnir-search-engine imap))))

;; SMTP
(defun setQsdfghSMTP ()
  "Set Qsdfgh SMTP server."
  (interactive)
  (message "Select Qsdfgh SMTP server")
  (setq user-mail-address "pierre@qsdfgh.com")
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("mail.qsdfgh.com" 25 nil nil))
        smtpmail-auth-credentials '(("mail.qsdfgh.com" 25 "pierre@qsdfgh.com" nil))
        smtpmail-default-smtp-server "mail.qsdfgh.com"
        smtpmail-smtp-server "mail.qsdfgh.com"
        smtpmail-smtp-service 25
        smtpmail-local-domain "qsdfgh.com"))

(add-hook 'message-mode-hook
          '(lambda ()
             (cond
              ((string-match "qsdfgh" gnus-newsgroup-name) (setQsdfghSMTP))
              (t (error "No SMTP server selected")))))

;;; gnus.el ends here
