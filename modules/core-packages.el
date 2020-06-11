;;; core-packages.el --- Packages -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-15 15:16:37>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(require 'package)

(setq package-enable-at-startup nil
      package-user-dir (expand-file-name ".cache/packages" user-emacs-directory)
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package bind-key :ensure t :demand t)
(use-package diminish :ensure t :demand t)

(provide 'core-packages)

;;; core-packages.el ends here
