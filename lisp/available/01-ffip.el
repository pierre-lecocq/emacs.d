;;; 01-ffip.el --- Find file in project

;; Time-stamp: <2016-06-29 10:56:30>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package find-file-in-project :ensure t
  :bind (("C-S-x C-S-f" . find-file-in-project)))

(defun ffip-environment ()
  "Setup FFIP environment."
  (interactive)
  (setq ffip-prefer-ido-mode t)
  (when (or (ffip-current-full-filename-match-pattern-p "\\(/www/fotolia\\)")
            (ffip-current-full-filename-match-pattern-p "\\(/www/adobestock\\)"))
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

;;; 01-ffip.el ends here
