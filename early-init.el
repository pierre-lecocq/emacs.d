;;; early-init.el --- Early Emacs configuration -*- lexical-binding: t; -*-

;; File: init.el
;; Creation: Thu Oct 19 12:19:54 2023
;; Time-stamp: <2025-08-26 08:24:20>
;; Copyright (C): 2023 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'default-frame-alist '(reverse . t))
(setq initial-frame-alist default-frame-alist)

(set-face-attribute 'vertical-border nil :foreground "#444444" :background "#444444")
(set-face-attribute 'fringe nil :foreground "#111111" :background "#111111")

(set-face-attribute 'font-lock-string-face nil :foreground "#ED556A")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#6395EE")
(set-face-attribute 'font-lock-constant-face nil :foreground "#63dbee")
(set-face-attribute 'font-lock-type-face nil :foreground "#63dbee")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#ffffff")

(set-face-attribute 'font-lock-comment-face nil :foreground "#848484")
(set-face-attribute 'font-lock-doc-face nil :foreground "#848484")
(set-face-attribute 'font-lock-doc-markup-face nil :foreground "#848484")

;;; early-init.el ends here.
