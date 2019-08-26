;;; init-completion.el --- Completion init -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-22 15:18:37>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t
  :init (progn
          (global-company-mode)
          (setq company-auto-complete nil
                company-minimum-prefix-length 2
                company-tooltip-limit 20
                company-idle-delay 0.5
                company-dabbrev-downcase nil
                company-backends '((company-files
                                    company-keywords
                                    company-capf
                                    company-etags
                                    company-gtags)
                                   (company-abbrev
                                    company-dabbrev
                                    company-dabbrev-code)))))

(provide 'init-completion)

;;; init-completion.el ends here
