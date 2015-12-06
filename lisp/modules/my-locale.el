;;; my-locale --- Emacs config - locale

;; Time-stamp: <2015-12-06 22:16:51>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(provide 'my-locale)

;;; my-locale.el ends here
