;;; dev-lsp.el --- Lsp -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-20 14:55:29>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;; npm install -g typescript-language-server

;;; Code:

(use-package treemacs :ensure t :defer t
  :config (progn
            (treemacs-follow-mode t)
            (treemacs-filewatch-mode t)
            (treemacs-fringe-indicator-mode t)
            (treemacs-git-mode 'deferred)
            (treemacs-resize-icons 16))
  :bind (("C-c f t" . treemacs)))

(use-package treemacs-projectile :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit :ensure t
  :after (treemacs magit))

(use-package lsp-mode :ensure t
  :commands (lsp lsp-deferred)
  :hook ((js2-mode . lsp)
         (php-mode . lsp))
  :config (setq lsp-prefer-flymake nil
                lsp-keep-workspace-alive nil
                lsp-session-file (expand-file-name ".cache/lsp-session.el" user-emacs-directory))
  :bind (:map lsp-mode-map
              ("C-c t" . lsp-describe-thing-at-point)))

(use-package lsp-ui :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :init (setq lsp-ui-imenu-enable t
              lsp-ui-flycheck-enable t
              lsp-ui-flycheck-list-position 'right
              lsp-ui-flycheck-live-reporting t
              ;; -- lsp-ui-doc
              ;; lsp-ui-doc-use-webkit t
              ;; lsp-ui-doc-enable t
              ;; lsp-ui-doc-header t
              ;; lsp-ui-doc-use-childframe t
              ;; lsp-ui-doc-position 'top
              ;; lsp-ui-doc-include-signature t
              ;; -- ls-ui-peek
              lsp-ui-peek-enable t
              ;; -- lsp-ui-sideline
              lsp-ui-sideline-enable t
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-show-symbol t
              lsp-ui-sideline-show-hover t
              ;; lsp-ui-sideline-show-diagnostics t
              ;; lsp-ui-sideline-show-code-actions t
              )
  :config (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
            (setq mode-line-format nil))
  :custom (lsp-intelephense-storage-path (expand-file-name ".cache/lsp-cache" user-emacs-directory))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ;; M-.
              ([remap xref-find-references] . lsp-ui-peek-find-references) ;; M-?
              ("C-c l i" . lsp-ui-imenu)))

(use-package company-lsp :ensure t
  :config (progn
            (setq company-lsp-enable-snippet t)
            (push 'company-lsp company-backends)))

(use-package lsp-treemacs :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

;; (defun lsp-toggle-visual-info ()
;;   "Activate LSP visual info."
;;   (interactive)
;;   (lsp-ui-sideline-toggle-symbols-info)
;;   (lsp-treemacs-sync-mode)
;;   (lsp-treemacs-symbols)
;;   (lsp-treemacs-errors-list))

;; (use-package dap-mode  :ensure t
;;   :config (progn
;;             (dap-mode 1)
;;             (dap-ui-mode 1)
;;             (dap-tooltip-mode 1)
;;             (tooltip-mode 1))
;;   :hook ((js2-mode . (lambda ()
;;                        (require 'dap-node)
;;                        ;; (dap-register-debug-template "Node Docker - AssetManager::52899"
;;                        ;;                              (list :type "node"
;;                        ;;                                    :request "attach"
;;                        ;;                                    :name "Node Docker - AssetManager::52899"
;;                        ;;                                    :protocol "auto"
;;                        ;;                                    :port 52899))

;;                        (dap-register-debug-template "Node::Run"
;;                                                     (list :type "node"
;;                                                           :cwd "/Users/lecocq/src/test"
;;                                                           :request "launch"
;;                                                           :program nil
;;                                                           :name "Node::Run"))
;;                        )
;;                    )
;;          (php-mode . (lambda ()
;;                        (require 'dap-php)))))

(provide 'dev-lsp)

;;; dev-lsp.el ends here
