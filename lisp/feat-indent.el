;;; feat-indent.el --- Indentation support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:26:44>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package aggressive-indent :ensure t :diminish aggressive-indent-mode
  :config (progn
            (global-aggressive-indent-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'web-mode)))

;;; feat-indent.el ends here
