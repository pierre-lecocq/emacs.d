;;; feat-neotree.el --- Neotree support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:17:41>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

(use-package all-the-icons :ensure t) ;; Run `M-x all-the-icons-install-fonts'

(use-package neotree :ensure t
  :after (:all neotree)
  :bind (("C-c t" . neotree-toggle)
         ("C-c p" . neotree-project-dir)
         ("C-c h" . neotree-hidden-file-toggle))
  :init (setq neo-smart-open t
              neo-window-fixed-size nil
              neo-theme (if (display-graphic-p) 'icons 'nerd)))

;;; feat-neotree.el ends here
