;;; config-encrypt.el --- Emacs configuration - encrypt

;; Time-stamp: <2016-01-20 09:04:40>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Locate gpg tools on mac
(when (file-exists-p "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/bin"))

(require 'epg-config)
(require 'epa-file)

(epa-file-enable)

(provide 'config-encrypt)

;;; config-encrypt.el ends here
