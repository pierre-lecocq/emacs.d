;;; init-swiper.el --- Emacs configuration - swiper

;; Time-stamp: <2016-01-12 10:38:45>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package swiper :ensure t
  :init (progn
          (ivy-mode 1)
          (setq ivy-use-virtual-buffers t))
  :bind (("C-s" . swiper)))

(provide 'init-swiper)

;;; init-swiper.el ends here
