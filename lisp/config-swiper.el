;;; config-swiper.el --- Emacs configuration - swiper

;; Time-stamp: <2016-01-20 09:08:37>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package swiper :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq ivy-height 30))
  :bind (("C-s" . swiper)))

(provide 'config-swiper)

;;; config-swiper.el ends here
