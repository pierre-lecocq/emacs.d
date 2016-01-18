;;; init-mode-line.el --- Emacs configuration - modeline

;; Time-stamp: <2016-01-18 14:23:07>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar original-mode-line-format mode-line-format)
(defvar is-custom-mode-line-loaded t)
(defvar default-mode-line-modes '(erc-mode))

(defun pl-set-custom-mode-line ()
  "Set custom mode-line format."
  (interactive)
  (setq-default mode-line-format
                (list (quote ((:eval (if (buffer-modified-p)
                                         (propertize " %b" 'face 'bold-italic)
                                       (propertize " %b" 'face 'bold)))
                              ;; Position and major mode
                              " | %l:%c %p:%I | %m" ;; (format " %s" minor-mode-alist)
                              ;; Which function
                              (which-function-mode (" " which-func-format))
                              ;; VC
                              (vc-mode vc-mode))))))

(defun pl-set-default-mode-line ()
  "Set default mode-line format."
  (interactive)
  (setq-default mode-line-format original-mode-line-format))

(defun hook-after-change-major-mode ()
  "Hook for after-change-major."
  (if (member major-mode default-mode-line-modes)
      (when is-custom-mode-line-loaded
        (setq is-custom-mode-line-loaded nil)
        (pl-set-default-mode-line))
    (unless is-custom-mode-line-loaded
      (setq is-custom-mode-line-loaded t)
      (pl-set-custom-mode-line))))

(add-hook 'after-change-major-mode-hook #'hook-after-change-major-mode)

(pl-set-custom-mode-line)

(provide 'init-modeline)

;;; init-modeline.el ends here
