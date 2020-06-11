;;; lang-javascript.el --- Lang-Javascript -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-20 15:12:55>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package js2-mode :ensure t
  :mode (("\\.js$" . js2-mode))
  :hook (js2-mode . (lambda ()
                      (setq-default tab-width 2)
                      (setq js-indent-level 2
                            js2-basic-offset 2)
                      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
                      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))
                      (js2-imenu-extras-mode)
                      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
                      (define-key js-mode-map (kbd "M-.") nil)
                      ;; (setq-default js2-show-parse-errors nil
                      ;;               js2-strict-missing-semi-warning nil
                      ;;               js2-strict-trailing-comma-warning t)
                      (flycheck-select-checker 'javascript-eslint)
                      (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                      (setq-default flycheck-temp-prefix ".flycheck"
                                    flycheck-disabled-checkers (append flycheck-disabled-checkers
                                                                       '(javascript-jshint json-jsonlist)))
                      )))

(use-package js2-refactor :ensure t
  :config (js2r-add-keybindings-with-prefix "C-c C-f")
  :hook (js2-mode . js2-refactor-mode))

(use-package rjsx-mode :ensure t
  :after js2-mode
  :mode (("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package tern :ensure t
  :hook (js2-mode . tern-mode))

;; (use-package company-tern :ensure t
;;   :after (js2-mode company tern)
;;   :config (progn
;;             (add-to-list 'company-backends 'company-tern)
;;             ;; Disable completion keybindings, as we use xref-js2 instead
;;             (unbind-key "M-." tern-mode-keymap)
;;             (unbind-key "M-," tern-mode-keymap)))

(provide 'lang-javascript)

;;; lang-javascript.el ends here
