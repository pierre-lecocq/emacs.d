;;; feat-recentf.el --- Recentf support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:26:58>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun deferred-recentf ()
  "Defer recentd installation and loading to shorten the Emacs loading time."
  (interactive)
  (unless (boundp 'recentf-save-file)
    (setq recentf-save-file (concat (file-name-directory load-file-name) "../local/my-recentf.el"))
    (use-package recentf :demand t :ensure nil
      :init (setq recentf-auto-cleanup 'never
                  recentf-max-menu-items 20)
      :config (progn (add-to-list 'recentf-exclude package-user-dir)
                     (recentf-mode 1))))
  ;; Launch
  (recentf-open-files))

(global-set-key (kbd "<f11>") 'deferred-recentf)

;;; feat-recentf.el ends here
