;;; module-completion.el --- Completion module -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:39:27>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package company :ensure t :diminish
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

(provide 'module-completion)

;;; module-completion.el ends here
