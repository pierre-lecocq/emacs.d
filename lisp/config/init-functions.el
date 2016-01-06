;;; init-functions.el --- Emacs config - functions

;; Time-stamp: <2016-01-06 08:45:12>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-kill-buffers-by-mode (&optional mode-name)
  "Kill buffers by mode.  Ask which mode if MODE-NAME is not provided."
  (interactive)
  (unless mode-name
    (setq mode-name (read-from-minibuffer "Mode to kill: ")))
  (let ((killed-buffers 0)
        (mode-to-kill (intern mode-name)))
    (dolist (buffer (buffer-list))
      (when (eq mode-to-kill (buffer-local-value 'major-mode buffer))
        (setq killed-buffers (1+ killed-buffers))
        (kill-buffer buffer)))
    (message "%d buffer(s) killed" killed-buffers)))

(defun pl-force-eval ()
  "Forced Emacs Lisp buffer evaluation - stolen from SO."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun pl-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 = transparent, 100 = opaque."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when (display-graphic-p)
    (set-frame-parameter (selected-frame) 'alpha value)))

(defun pl-join-lines ()
  "Join lines."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (kill-line)
    (just-one-space)))

(defun pl-join-lines-on-region (start end)
  "Join lines on region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (funcall #'pl-join-lines)
      (forward-line 1))))

(defun pl-clean-system-name ()
  "Get a clean system name."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" (downcase (car (split-string (system-name) "\\.")))))

(defun org-font-lock-ensure (beg end)
  "Org font lock ensure from BEG to END."
  (font-lock-ensure))

(provide 'init-functions)

;;; init-functions.el ends here
