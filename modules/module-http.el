;;; module-http.el --- HTTP feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-06-03 11:01:24>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(provide 'module-http)

;;; module-http.el ends here
