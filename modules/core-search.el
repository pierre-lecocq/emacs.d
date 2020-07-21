;;; core-search.el --- Search -*- lexical-binding: t; -*-

;; Time-stamp: <2020-07-21 15:28:56>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package isearch :ensure nil :demand t
  :config (setq search-highlight t
                search-whitespace-regexp ".*?"
                isearch-lax-whitespace t
                isearch-regexp-lax-whitespace nil
                isearch-lazy-highlight t
                isearch-lazy-count t
                lazy-count-prefix-format nil
                lazy-count-suffix-format " (%s/%s)")
  (defadvice isearch-update (before my-isearch-reposite activate)
    "Update an isearch session by recentering the buffer to the found location."
    (recenter)))

(provide 'core-search)

;;; core-search.el ends here
