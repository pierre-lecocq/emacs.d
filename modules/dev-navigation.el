;;; dev-navigation.el --- Navigation -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-20 15:28:18>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package ag :ensure t
  :init (setq ag-highlight-search t))

(use-package dumb-jump :ensure t
  :init (setq dumb-jump-prefer-searcher 'ag)
  :bind (("C-c j o" . dumb-jump-go-other-window)
         ("C-c j g" . dumb-jump-go)
         ("C-c j b" . dumb-jump-back)
         ("C-c j p" . dumb-jump-go-prompt)
         ("C-c j e" . dumb-jump-go-prefer-external)
         ("C-c j w" . dumb-jump-go-prefer-external-other-window)))

(provide 'dev-navigation)

;;; dev-navigation.el ends here
