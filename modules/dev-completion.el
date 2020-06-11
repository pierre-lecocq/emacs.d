;;; dev-completion.el --- Completion -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:13:35>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t
  :config (global-company-mode)
  :init (setq company-auto-complete nil
              company-minimum-prefix-length 1
              company-tooltip-limit 20
              company-idle-delay 0.25
              company-dabbrev-downcase nil
              ;; company-backends '((company-dabbrev-code company-gtags company-etags company-keywords)
              ;;                    company-files company-capf company-dabbrev)
              ))

(use-package company-quickhelp :ensure t
  :config (company-quickhelp-mode))

(provide 'dev-completion)

;;; dev-completion.el ends here
