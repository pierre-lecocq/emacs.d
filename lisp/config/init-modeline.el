;;; init-mode-line.el --- Emacs configuration - modeline

;; Time-stamp: <2016-01-18 15:38:55>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar original-mode-line-format   mode-line-format)
(defvar is-custom-mode-line-loaded  t)
(defvar default-mode-line-modes     '(erc-mode))

(defun pl-set-custom-mode-line ()
  "Set custom mode-line format."
  (interactive)
  (setq is-custom-mode-line-loaded t)
  (setq-default mode-line-format
                (list (quote ((:eval (if (buffer-modified-p) (propertize " %b" 'face 'bold-italic) (propertize " %b" 'face 'bold)))
                              " | %l:%c %p:%I | %m" ;; (format " %s" minor-mode-alist)
                              (which-function-mode (" " which-func-format))
                              (vc-mode vc-mode))))))

(defun pl-set-default-mode-line ()
  "Set default mode-line format."
  (interactive)
  (setq is-custom-mode-line-loaded nil)
  (setq-default mode-line-format original-mode-line-format))

(defun hook-after-change-major-mode ()
  "Hook for after-change-major."
  (if (member major-mode default-mode-line-modes)
      (when is-custom-mode-line-loaded
        (pl-set-default-mode-line))
    (unless is-custom-mode-line-loaded
      (pl-set-custom-mode-line))))

(add-hook 'after-change-major-mode-hook #'hook-after-change-major-mode)

(pl-set-custom-mode-line)

(provide 'init-modeline)

;;; init-modeline.el ends here
