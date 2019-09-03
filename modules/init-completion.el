;;; init-completion.el --- Completion init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-09-03 15:06:15>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t
  :config (global-company-mode)
  :init (setq company-auto-complete nil
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.25
              company-dabbrev-downcase nil
              company-backends '(company-capf
                                 company-dabbrev
                                 company-files
                                 (company-dabbrev-code
                                  company-etags
                                  company-gtags
                                  company-keywords))))

(use-package company-quickhelp :ensure t
  :after (company)
  :config (company-quickhelp-mode))

(provide 'init-completion)

;;; init-completion.el ends here
