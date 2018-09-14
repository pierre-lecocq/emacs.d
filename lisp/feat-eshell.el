;;; feat-eshell.el --- Eshell support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:37:03>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun eshell-other-frame ()
  "Open eshell in another maximized frame."
  (interactive)
  (with-selected-frame (make-frame)
    (eshell)
    (when (display-graphic-p)
      (toggle-frame-maximized))))

(defun eshell-window-vertical ()
  "Open eshell in another vertically splitted window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (eshell (string-to-number (format-time-string "%s"))))

(defun eshell-window-horizontal ()
  "Open eshell in another horizontally splitted window."
  (interactive)
  (split-window-bottom)
  (other-window 1)
  (eshell (string-to-number (format-time-string "%s"))))

(use-package eshell :demand t :ensure nil
  :bind (("<f9>" . eshell-other-frame))
  :init (setq eshell-directory-name (concat (file-name-directory load-file-name) "../local/my-eshell")))

;;; feat-eshell.el ends here
