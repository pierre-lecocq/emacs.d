;;; feat-http.el --- HTTP feature -*- lexical-binding: t; -*-

;; Time-stamp: <2019-05-30 15:16:13>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package restclient :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode)))

(provide 'feat-http)

;;; feat-http.el ends here
