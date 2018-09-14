;;; feat-search.el --- Search support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 22:57:40>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package anzu :ensure t :diminish anzu-mode
  :config (global-anzu-mode +1)
  :custom-face (anzu-mode-line ((t (:foreground "yellow")))))

;;; feat-search.el ends here
