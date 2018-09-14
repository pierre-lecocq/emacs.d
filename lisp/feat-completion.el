;;; feat-completion.el --- Completion support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:26:30>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t :diminish company-mode
  :config (global-company-mode 1)
  :init (setq company-auto-complete nil
              company-tooltip-flip-when-above t
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.5))

;;; feat-completion.el ends here
