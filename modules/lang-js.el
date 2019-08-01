;;; lang-js.el --- Javascript language support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-08-01 14:04:17>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; - brew install the_silver_searcher
;; - sudo npm install -g tern

;;; Code:

(use-package js2-refactor :ensure t :diminish)

(use-package xref-js2 :ensure t :diminish)

(when (executable-find "tern")
  (use-package company-tern :ensure t :diminish
    :config (progn
              (eval-after-load 'company
                '(push 'company-tern company-backends))
              ;; Disable completion keybindings, as we use xref-js2 instead
              (unbind-key "M-." tern-mode-keymap)
              (unbind-key "M-," tern-mode-keymap))))

(use-package js2-mode :ensure t
  :mode (("\\.js\\'" . js2-mode)))

(use-package rjsx-mode :ensure t
  :mode (("components\\/.*\\.js\\'" . rjsx-mode)))

(defun hook-js2-mode ()
  "Hook for js2 mode."
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
  (tern-mode t)
  (js2-imenu-extras-mode)
  (js2-refactor-mode)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (define-key js-mode-map (kbd "M-.") nil)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t))

(add-hook 'js2-mode-hook #'hook-js2-mode)

(provide 'lang-js)

;;; lang-js.el ends here
