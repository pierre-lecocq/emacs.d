;;; pl-swiper.el --- Emacs configuration - swiper

;; Time-stamp: <2016-02-29 00:03:20>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package swiper :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t)
          (setq ivy-height 30))
  :bind (("C-s" . swiper)))

(provide 'pl-swiper)

;;; pl-swiper.el ends here
