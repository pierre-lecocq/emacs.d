;;; init-mode-line.el --- Emacs configuration - modeline

;; Time-stamp: <2016-01-18 21:39:31>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Add modes that require the default mode-line
(defvar default-mode-line-modes     '(erc-mode))

;; Do not edit this
(defvar original-mode-line-format   mode-line-format)
(defvar is-custom-mode-line-loaded  nil)
(defvar useless-modes               '(fundamental-mode
                                      minibuffer-inactive-mode))

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
  (unless (member major-mode useless-modes)
    (if (member major-mode default-mode-line-modes)
        (when is-custom-mode-line-loaded
          (pl-set-default-mode-line))
      (unless is-custom-mode-line-loaded
        (pl-set-custom-mode-line)))))

;; BUG: not called when switching to existing buffers, even if in the required modes
(add-hook 'after-change-major-mode-hook #'hook-after-change-major-mode)

(provide 'init-modeline)

;;; init-modeline.el ends here
