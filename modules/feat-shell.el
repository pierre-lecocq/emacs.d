;;; feat-shell.el --- Shell feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-04 09:51:21>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq eshell-prompt-function
      (lambda ()
        (concat
         ;; time
         (propertize (format-time-string "[%H:%M] " (current-time))
                     'face '(:foreground "DeepSkyBlue"))
         ;; pwd
         (propertize (replace-regexp-in-string (getenv "HOME") "~" (eshell/pwd))
                     'face '(:foreground "white"))
         ;; git branch
         (let ((branch (replace-regexp-in-string
                        "\r?\n\\'" ""
                        (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2> /dev/null"))))
           (when (> (length branch) 0)
             (propertize (format ":%s " branch)
                         'face '(:foreground "yellow"))))
         ;; separator
         (if (= (user-uid) 0)
             (propertize "# " 'face '(:foreground "red"))
           (propertize "$ " 'face '(:foreground "white"))))))

(provide 'feat-shell)

;;; feat-shell.el ends here
