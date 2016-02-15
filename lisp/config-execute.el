;;; config-execute.el --- Emacs configuration - execute

;; Time-stamp: <2016-02-15 11:42:01>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar pl-execute-info '(("rb"  . ("ruby -c" "ruby"))
                          ("py"  . ("pylint" "python"))
                          ("php" . ("php -l" "php"))))

(defun pl-lint-or-execute (cmd)
  "Lint or execute the current file."
  (when (or (null (buffer-file-name))
            (buffer-modified-p))
    (save-buffer))
  (let* ((ext (file-name-extension (buffer-file-name)))
         (fileinfo (cdr (assoc ext pl-execute-info)))
         (cmd-index (if (string-equal "lint" cmd) 0 1)))
    (unless fileinfo
      (error "Unknown file type"))
    (compile (concat (nth cmd-index fileinfo) " " (buffer-file-name)))))

(defun pl-lint ()
  "Lint the current file."
  (interactive)
  (pl-lint-or-execute "lint"))

(defun pl-execute ()
  "Execute the current file."
  (interactive)
  (pl-lint-or-execute "execute"))

(provide 'config-execute)

;;; lisp-execute.el ends here
