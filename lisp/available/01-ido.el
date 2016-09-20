;;; 01-ido.el --- Ido

;; Time-stamp: <2016-06-29 10:58:13>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package flx-ido :ensure t)
(use-package ido-hacks :ensure t)
(use-package ido-vertical-mode :ensure t)

(use-package ido :ensure t
  :init (setq ido-case-fold t
              ido-enable-flex-matching t
              ido-use-filename-at-point 'guess
              ;; ido-use-virtual-buffers t ;; Fucks TRAMP buffers up when switching or killing
              ido-create-new-buffer 'always)
  :config (progn
            (ido-everywhere 1)
            (flx-ido-mode 1)
            (ido-mode t)
            (ido-hacks-mode)
            (ido-vertical-mode)))

(provide '01-ido)

;;; 01-ido.el ends here
