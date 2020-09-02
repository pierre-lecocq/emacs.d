;;; lang-web.el --- Lang-Web -*- lexical-binding: t; -*-

;; Time-stamp: <2020-09-02 14:11:32>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(defun restclient-scratch ()
  "Open a restclient scratch buffer."
  (interactive)
  (let ((buf (get-buffer-create "*restclient-scratch*")))
    (with-current-buffer buf
      (restclient-mode)
      (switch-to-buffer buf)
      (yas-expand-snippet (yas-lookup-snippet "base")))))

(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.erubis\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config (flycheck-add-mode 'javascript-eslint 'web-mode)
  :init (setq web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2))

(provide 'lang-web)

;;; lang-web.el ends here
