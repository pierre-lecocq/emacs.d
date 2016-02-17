;;; config-mode-line.el --- Emacs configuration - modeline

;; Time-stamp: <2016-02-17 19:24:23>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Add modes that require the default mode-line
(defvar default-mode-line-modes '(erc-mode))

;; Do not edit this
(defvar original-mode-line-format mode-line-format)
(defvar custom-mode-line-p nil)
(defvar useless-modes '(fundamental-mode minibuffer-inactive-mode))

(defun pl-set-custom-mode-line ()
  "Set custom mode-line format."
  (interactive)
  (setq custom-mode-line-p t)
  (setq-default mode-line-format
                (list (quote ((:eval (let ((foreground-value (if (and (buffer-file-name) (file-remote-p (buffer-file-name))) "yellow" "white"))
                                           (slant-value (if (buffer-modified-p) 'italic 'normal)))
                                       (propertize " %b" 'font-lock-face `(:weight bold :slant ,slant-value :foreground ,foreground-value))))
                              " | %l:%c %p:%I | %m" ;; (format " %s" minor-mode-alist)
                              (which-function-mode (" " which-func-format))
                              (vc-mode vc-mode))))))

(defun pl-set-default-mode-line ()
  "Set default mode-line format."
  (interactive)
  (setq custom-mode-line-p nil)
  (setq-default mode-line-format original-mode-line-format))

(defun hook-after-change-major-mode ()
  "Hook for after-change-major."
  (unless (member major-mode useless-modes)
    (if (member major-mode default-mode-line-modes)
        (when custom-mode-line-p
          (pl-set-default-mode-line))
      (unless custom-mode-line-p
        (pl-set-custom-mode-line)))))

;; BUG: not called when switching to existing buffers, even if in the required modes
(add-hook 'after-change-major-mode-hook #'hook-after-change-major-mode)

(provide 'config-modeline)

;;; config-modeline.el ends here
