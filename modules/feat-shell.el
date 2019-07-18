;;; feat-shell.el --- Shell feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-07-17 11:30:27>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

;; (defadvice ansi-term (before force-bash)
;;   "Advice BEFORE ansi term with FORCE-BASH."
;;   (interactive (list "/bin/bash")))
;; (ad-activate 'ansi-term)

(defun toggle-shell ()
  "Toggle shell."
  (interactive)
  (let ((shell-buf (get-buffer "*ansi-term*")))
    (if shell-buf
        (if (equal (current-buffer) shell-buf)
            (switch-to-buffer (other-buffer))
          (switch-to-buffer shell-buf))
      (ansi-term "/bin/bash" "ansi-term"))))

(if (eq system-type 'darwin)
    (global-set-key (kbd "<M-return>") 'toggle-shell)
  (global-set-key (kbd "<S-return>") 'toggle-shell))

(defun open-terminal-here (dir)
  "Open terminal application in DIR."
  (interactive "DDirectory: ")
  (let ((cmd (if (eq system-type 'darwin) "open -a iTerm" "rxvt-unicode")))
    (call-process-shell-command (concat cmd dir) nil 0)))

(provide 'feat-shell)

;;; feat-shell.el ends here
