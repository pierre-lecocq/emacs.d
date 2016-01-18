;;; init-erc.el --- Emacs configuration - ERC

;; Time-stamp: <2016-01-18 20:51:31>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package erc :ensure t
  :init (progn
          (defvar erc-insert-post-hook)
          (setq erc-nick "pierre404"
                erc-fill-column (- (window-width) 2)
                erc-input-line-position -2
                erc-log-insert-log-on-open nil
                erc-log-channels t
                erc-log-channels-directory (concat files-dir "erc")
                erc-save-buffer-on-part t
                erc-hide-timestamps nil
                erc-hide-list '("JOIN" "PART" "QUIT")
                erc-max-buffer-size 20000
                erc-truncate-buffer-on-save t
                erc-keywords '("pierre404")
                erc-timestamp-only-if-changed-flag nil
                erc-timestamp-format "[%R] "
                erc-insert-timestamp-function 'erc-insert-timestamp-left
                erc-server-coding-system '(utf-8 . utf-8)
                erc-interpret-mirc-color t
                erc-kill-buffer-on-part t
                erc-kill-queries-on-quit t
                erc-kill-server-buffer-on-quit t
                erc-autojoin-channels-alist '(("freenode.net" "#debian" "#emacs")))
          (erc-netsplit-mode 1)
          (erc-match-mode 1)
          (add-hook 'erc-insert-post-hook
                    #'erc-truncate-buffer)
          (add-hook 'erc-mode-hook
                    #'(lambda ()
                        (setq show-trailing-whitespace nil)
                        (auto-fill-mode 0)))
          (add-hook 'erc-after-connect
                    #'(lambda (SERVER NICK)
                        (erc-message "PRIVMSG" (format "NickServ identify %s" (read-passwd "IRC NickServ Password: ")))))))

(defun pl-erc-connect ()
  "Connect to ERC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6666 :nick "pierre404" :full-name "Pierre Lecocq"))

(provide 'init-erc)

;;; init-erc.el ends here
