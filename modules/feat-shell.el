;;; feat-shell.el --- Shell feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-05 09:01:34>
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
      (ansi-term "/bin/bash"))))

(global-set-key (kbd "<M-return>") 'toggle-shell)

(provide 'feat-shell)

;;; feat-shell.el ends here
