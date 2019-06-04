;;; feat-shell.el --- Shell feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-04 22:48:54>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(defadvice ansi-term (before force-bash)
  "Advice BEFORE ansi term with FORCE-BASH."
  (interactive (list "/bin/bash")))

(ad-activate 'ansi-term)

(global-set-key (kbd "<M-return>") 'ansi-term)

(provide 'feat-shell)

;;; feat-shell.el ends here
