;;; web.el --- Web feature -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(use-package htmlize :ensure t)

(use-package scss-mode :ensure t)

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)))

;; -- PHP ----------------------------------------------------------------------

(use-package php-extras :ensure t)

(use-package php-mode :ensure t
  :mode (("\\.php-dev'" . php-mode)
         ("\\.php-dist'" . php-mode)
         ("\\.php-dev'" . php-mode)))

(defun hook-php-mode ()
  "Hook for PHP mode."
  (php-enable-default-coding-style)
  (set (make-local-variable 'company-backends)
       '((php-extras-company company-dabbrev-code) company-capf company-files))
  (setq comment-start "// "
        comment-end ""))

(add-hook 'php-mode-hook #'hook-php-mode)

;; -- JS ------------------------------------------------------------------------

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

;; -- SQL ----------------------------------------------------------------------

(use-package sqlup-mode :ensure t)

(use-package sql-indent :ensure t)

(defun hook-sql-mode ()
  "Hook for SQL mode."
  (sqlup-mode t)
  (toggle-truncate-lines t))

(add-hook 'sql-mode-hook #'hook-sql-mode)
(add-hook 'sql-interactive-mode-hook #'hook-sql-mode) ;; When connected to a server within Emacs

(provide 'web)

;;; web.el ends here
