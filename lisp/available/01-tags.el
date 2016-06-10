;;; 01-tags.el --- Tags

;; Time-stamp: <2016-06-10 17:41:33>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar tags-bin-path "/usr/local/bin/ctags")

(defun pl-files-regexp-by-mode ()
  "Get files regexp by mode."
  (cond
   ((string= major-mode "common-lisp-mode") ".cl")
   ((string= major-mode "emacs-lisp-mode") ".el")
   ((string= major-mode "c-mode") ".c.h")
   ((string= major-mode "ruby-mode") ".rb")
   ((string= major-mode "pythome-mode") ".py")
   ((string= major-mode "php-mode") ".php.css.js")))

(defun pl-compile-tags (directory)
  "Compile etags for a given DIRECTORY."
  (interactive "DRoot directory: ")
  (let* ((dir (expand-file-name (file-name-as-directory directory)))
         (dir-local (replace-regexp-in-string "/[^/]+:[^/]+:/" "/" dir))
         (file (concat dir "/TAGS")))
    (cd dir)
    (compile (format "%s -e -h \"%s\" -R ." tags-bin-path (pl-files-regexp-by-mode)))
    (setq tags-file-name file)
    (visit-tags-table file)))

;;; 01-tags.el ends here
