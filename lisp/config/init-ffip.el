;;; init-ffip.el --- Emacs configuration - ffip

;; Time-stamp: <2016-01-12 15:19:12>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package find-file-in-project :ensure t
  :bind (("C-S-x C-S-f" . find-file-in-project)))

(defun ffip-environment ()
  "Setup FFIP environment."
  (interactive)
  (when (ffip-current-full-filename-match-pattern-p "\\(/www/fotolia\\)")
    (setq-local ffip-find-options "-not -size +64k")
    (setq-local ffip-patterns '("*.php" "*.js" "*.css"))
    (setq-local ffip-prune-patterns '("*/.git/*"
                                      "*/vendor/*"
                                      "*/external/*"
                                      "*/html_hashes/*"
                                      "*/po/*"
                                      "*/po_current/*"
                                      "*/include/Zend/*"
                                      "*/sprites/*"))))

(add-hook 'prog-mode-hook 'ffip-environment)

(provide 'init-ffip)

;;; init-ffip.el ends here
