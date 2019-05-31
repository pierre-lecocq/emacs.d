;;; feat-js.el --- Javascript feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-30 15:16:44>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package js2-refactor :ensure t)

(use-package xref-js2 :ensure t) ;; requires installing `ag'

(when (executable-find "tern") ;; `sudo npm install -g tern'
  (use-package company-tern :ensure t
    :config (progn
              (add-to-list 'company-backends 'company-tern)
              ;; Disable completion keybindings, as we use xref-js2 instead
              (unbind-key "M-." tern-mode-keymap)
              (unbind-key "M-," tern-mode-keymap))))

(use-package js2-mode :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode)))

(defun hook-js2-mode ()
  "Hook for js2 mode."
  (tern-mode)
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

(provide 'feat-js)

;;; feat-js.el ends here
