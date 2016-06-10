;;; 01-tags.el --- Tags

;; Time-stamp: <2016-06-10 11:57:51>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-files-regexp-by-mode ()
  "Get files regexp by mode."
  (cond
   ((string= major-mode "ruby-mode") "rb")
   ((string= major-mode "php-mode") "php")
   ((string= major-mode "emacs-lisp-mode") "el")))

(defun pl-compile-tags (directory)
  "Compile etags for a given DIRECTORY."
  (interactive "DRoot directory: ")
  (let* (
         (dir (expand-file-name (file-name-as-directory directory)))
         (dir-local (replace-regexp-in-string "/[^/]+:[^/]+:/" "/" dir))
         )
    (message " ** %s" dir)
    (message " -- %s" dir-local)
    (cd dir)
    (compile (format "find . -name \"*.%s\" -type f | xargs ctags -a -f TAGS" (pl-files-regexp-by-mode)))
    (unless (member dir-local 'tags-file-name)
      (add-to-list 'tags-file-name dir-local))
    (visit-tags-table (concat dir "/TAGS"))))

;;; 01-tags.el ends here
