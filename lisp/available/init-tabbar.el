;;; init-tabbar.el --- Tabbar

;; Time-stamp: <2016-12-02 11:37:30>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package tabbar-mode
  :init (progn
          (tabbar-mode 1)
          (setq tabbar-scroll-left-help-function nil
                tabbar-scroll-right-help-function nil
                tabbar-scroll-left-button (quote (("") ""))
                tabbar-scroll-right-button (quote (("") "")))
          (set-face-attribute 'tabbar-default nil
                              :background "grey22"
                              :foreground "grey50"
                              :box '(:line-width 6 :color "grey22"))
          (set-face-attribute 'tabbar-unselected nil
                              :background "grey22"
                              :foreground "grey50"
                              :box '(:line-width 6 :color "grey22"))
          (set-face-attribute 'tabbar-selected nil
                              :background "grey13"
                              :foreground "grey80"
                              :box '(:line-width 6 :color "grey13"))
          (set-face-attribute 'tabbar-modified nil
                              :background "grey22"
                              :foreground "red"
                              :box '(:line-width 6 :color "grey22"))
          (set-face-attribute 'tabbar-selected-modified nil
                              :background "grey13"
                              :foreground "red"
                              :box '(:line-width 6 :color "grey13"))
          (set-face-attribute 'tabbar-button nil
                              :box nil)
          (set-face-attribute 'tabbar-separator nil
                              :box nil)))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
