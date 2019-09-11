;;; init-neotree.el --- Neotree init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-22 15:19:20>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun neotree-project-dir ()
  "Open NeoTree using the git root of the current project."
  (interactive)
  ;; (let ((project-dir (ffip-project-root)))
  (let ((project-dir (projectile-project-root)))
    (neotree-dir project-dir)
    (neotree-find (buffer-file-name))))

(use-package neotree :ensure t
  :after (:all all-the-icons)
  :bind (("C-c f t" . neotree-toggle)
         ("C-c f p" . neotree-project-dir)
         ("C-c f h" . neotree-hidden-file-toggle))
  :init (setq neo-smart-open t
              neo-window-fixed-size nil
              neo-theme (if (display-graphic-p) 'icons 'nerd)))

(provide 'init-neotree)

;;; init-neotree.el ends here