;;; feat-shell.el --- Shell support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-10-01 14:53:03>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar shell-func 'ansi-term)
(defvar shell-buffer-name "*ansi-term*")
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

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Close buffer when term exits. Stolen from http://echosa.github.io/blog/2012/06/06/improving-ansi-term/ ."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(use-package ansi-term
  :bind (("<f9>" . toggle-shell)
         ("S-<f9>" . shell-other-frame)))

;;; feat-shell.el ends here
