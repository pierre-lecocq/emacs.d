;;; 01-completion.el --- Completion

;; Time-stamp: <2016-09-13 23:12:22>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t
  :init (setq company-auto-complete nil
              company-tooltip-flip-when-above t
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.5)
  :config (global-company-mode 1)
  :diminish company-mode)

(provide '01-completion)

;;; 01-completion.el ends here
