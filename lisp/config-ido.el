;;; config-ido.el --- Emacs configuration - ido

;; Time-stamp: <2016-01-20 09:06:57>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flx-ido :ensure t)

(use-package ido-hacks :ensure t)

(use-package ido-vertical-mode :ensure t)

(use-package ido :ensure t
  :init (progn
          (require 'ido)
          (require 'ido-hacks)
          (setq ido-save-directory-list-file (concat files-dir "ido.last")
                ido-case-fold t
                ido-enable-flex-matching t
                ido-use-filename-at-point 'guess
                ido-create-new-buffer 'always
                ido-use-virtual-buffers t)
          (ido-everywhere 1)
          (flx-ido-mode 1)
          (ido-mode t)
          (ido-hacks-mode)
          (ido-vertical-mode)))

(provide 'config-ido)

;;; config-ido.el ends here
