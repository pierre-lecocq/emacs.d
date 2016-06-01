;;; packages.el --- Packages

;; Time-stamp: <2016-03-18 08:18:33>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'package)

(setq package-user-dir packages-dir
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

(eval-when-compile
  (require 'use-package))

;;; packages.el ends here
