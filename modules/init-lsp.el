;;; init-lsp.el --- LSP support -*- lexical-binding: t; -*-

;; Time-stamp: <2019-12-02 14:30:07>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; Ruby:   gem install solargraph
;; JS:     npm i -g javascript-typescript-langserver

;;; Code:

(use-package lsp-mode :ensure t
  :hook (js2-mode . lsp)
  :commands lsp
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode
  :config (setq lsp-ui-flycheck-enable t
                lsp-ui-flycheck-list-position 'right
                lsp-ui-flycheck-live-reporting t
                lsp-ui-doc-enable t
                lsp-ui-doc-use-childframe t
                lsp-ui-doc-position 'top
                lsp-ui-doc-include-signature t
                lsp-ui-peek-enable t
                lsp-ui-peek-list-width 60
                lsp-ui-peek-peek-height 25
                lsp-ui-sideline-enable nil))

(use-package company-lsp :ensure t
  :commands company-lsp
  :config (push 'company-lsp company-backends))

;; (defun lsp-session ()
;;   "Launch a LSP session."
;;   (interactive)
;;   (lsp)
;;   (lsp-ui-mode 1)
;;   (lsp-ui-imenu)
;;   (lsp-ui-sideline-mode 1)
;;   (lsp-ui-peek-mode 1)
;;   (lsp-ui-doc-mode 1)
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions) ;; M-.
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references) ;; M-?
;;   )

(provide 'init-lsp)

;;; init-lsp.el ends here
