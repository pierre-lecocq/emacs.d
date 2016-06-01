;;; 01-completion.el --- Completion

;; Time-stamp: <2016-02-28 23:58:20>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t
  :init (progn
          (setq company-auto-complete nil
                company-tooltip-flip-when-above t
                company-minimum-prefix-length 2
                company-tooltip-limit 10
                company-idle-delay 0.5)
          (global-company-mode 1)))

;;; 01-completion.el ends here
