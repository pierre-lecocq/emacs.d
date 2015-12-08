;;; init-mode-line.el --- Emacs configuration - mode-line

;; Time-stamp: <2015-12-08 23:04:42>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq-default mode-line-format
              (list
               '(:eval (if (buffer-modified-p)
                           (propertize " %b" 'face 'bold-italic)
                         (propertize " %b" 'face 'bold)))
               " | %l:%c %p:%I | %m";; (format " %s" minor-mode-alist)
               '(which-function-mode (" " which-func-format))
               '(vc-mode vc-mode)))

(provide 'init-mode-line)

;;; init-mode-line.el ends here
