;;; feat-shell.el --- Shell support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-20 00:07:52>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar shell-buffer-name "*ansi-term*")
(defvar shell-func 'ansi-term)
(defvar shell-program "/bin/bash")

(defun toggle-shell ()
  "Toggle shell buffer."
  (interactive)
  (if (eq (current-buffer) (get-buffer shell-buffer-name))
      (switch-to-buffer (other-buffer (current-buffer) t))
    (if (member (get-buffer shell-buffer-name) (buffer-list))
        (switch-to-buffer shell-buffer-name)
      (funcall shell-func shell-program))))

(defun shell-other-frame ()
  "Open shell in another maximized frame."
  (interactive)
  (with-selected-frame (make-frame)
    (funcall shell-func shell-program)
    (when (display-graphic-p)
      (toggle-frame-maximized))))

(use-package ansi-term
  :bind (("<f9>" . toggle-shell)
         ("S-<f9>" . shell-other-frame)))

;;; feat-shell.el ends here
