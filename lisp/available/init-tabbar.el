;;; init-tabbar.el --- Tabbar

;; Time-stamp: <2016-12-01 14:17:24>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package tabbar-mode
  :init (progn
          (tabbar-mode 1)
          (setq tabbar-use-images nil)
          (set-face-attribute 'tabbar-default nil
                              :background "grey20"
                              :foreground "grey40"
                              :box '(:line-width 6 :color "grey20"))
          (set-face-attribute 'tabbar-selected nil
                              :background "grey13"
                              :foreground "grey70"
                              :box '(:line-width 6 :color "grey13"))
          (set-face-attribute 'tabbar-unselected nil
                              :background "grey20"
                              :foreground "grey40"
                              :box '(:line-width 6 :color "grey20"))
          (set-face-attribute 'tabbar-button nil
                              :box nil)
          (set-face-attribute 'tabbar-separator nil
                              :box nil)))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
