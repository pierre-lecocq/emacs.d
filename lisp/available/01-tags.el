;;; 01-tags.el --- Tags

;; Time-stamp: <2016-06-10 15:55:38>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-files-regexp-by-mode ()
  "Get files regexp by mode."
  (cond
   ((string= major-mode "common-lisp-mode") "cl")
   ((string= major-mode "emacs-lisp-mode") "el")
   ((string= major-mode "c-mode") "[ch]")
   ((string= major-mode "ruby-mode") "rb")
   ((string= major-mode "pythome-mode") "py")
   ((string= major-mode "php-mode") "php")))

(defun pl-compile-tags (directory)
  "Compile etags for a given DIRECTORY."
  (interactive "DRoot directory: ")
  (let* ((dir (expand-file-name (file-name-as-directory directory)))
         (dir-local (replace-regexp-in-string "/[^/]+:[^/]+:/" "/" dir)))
    (cd dir)
    (compile (format "find . -name \"*.%s\" -type f -print | etags -" (pl-files-regexp-by-mode)))
    (bury-buffer "*compilation*")
    (let ((file (concat dir "/TAGS")))
      (setq tags-file-name file)
      (visit-tags-table file))))

;;; 01-tags.el ends here
