;;; init-package.el --- Emacs configuration - package

;; Time-stamp: <2015-12-09 00:08:56>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Package manager settings
(require 'package)
(require 'nsm)

(setq nsm-settings-file (concat config-dir-files "network-security.data")
      package-user-dir config-dir-packages
      package-enable-at-startup nil
      package-archives '(("melpa"        . "http://melpa.org/packages/")
                         ("gnu"          . "http://elpa.gnu.org/packages/")
                         ("marmalade"    . "http://marmalade-repo.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(provide 'init-package)

;;; init-package.el ends here
