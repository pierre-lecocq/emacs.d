;;; lang-ruby.el --- Ruby laguage support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 23:21:13>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(add-to-list 'auto-insert-alist
             '((ruby-mode . "Ruby program") nil
               "#!/usr/bin/env ruby\n"
               "# -*- mode: ruby; -*-\n\n"
               "# File: " (file-name-nondirectory buffer-file-name) "\n"
               "# Time-stamp: <>\n"
               "# Copyright (C) " (substring (current-time-string) -4) " " (user-full-name) "\n"
               "# Description: " _ "\n\n"))

(use-package inf-ruby :ensure t :defer t)

(use-package robe :ensure t :defer t
  :init (push 'company-robe company-backends))

(use-package rubocop :ensure t :defer t)

(use-package ruby-tools :ensure t :defer t)

(use-package ruby-mode :ensure t
  :after (:all inf-ruby robe rubocop ruby-tools)
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Puppetfile" . ruby-mode)
         ("Rakefile" . ruby-mode)))

(defun ruby-transform-hash-keys (regexp-string match-string)
  "Transform hash keys from with REGEXP-STRING and MATCH-STRING."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (while (re-search-forward regexp-string nil t)
          (replace-match match-string)))))

(defun ruby-hash-symbols-to-strings ()
  "Transform hash keys from symbols to strings in a given region."
  (interactive)
  (ruby-transform-hash-keys ":\\([a-zA-Z0-9_-]+\\)" "'\\1'"))

(defun ruby-hash-strings-to-symbols ()
  "Transform hash keys from strings to symbols in a given region."
  (interactive)
  (ruby-transform-hash-keys "'\\([a-zA-Z0-9_-]+\\)'" ":\\1"))

(defun hook-ruby-mode ()
  "Hook for ruby mode."
  (robe-mode)
  (rubocop-mode)
  (ruby-tools-mode))

(add-hook 'ruby-mode-hook #'hook-ruby-mode)

;;; lang-ruby.el ends here
