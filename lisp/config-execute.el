;;; config-execute.el --- Emacs configuration - execute

;; Time-stamp: <2016-02-15 13:40:22>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; ("extention" . ("lint command" "execute command"))
(defvar pl-execute-info '(("lisp"   . (nil "sbcl --noinform --load"))
                          ("sh"     . (nil "bash"))
                          ("rb"     . ("ruby -c" "ruby"))
                          ("php"    . ("php -l" "php"))
                          ("py"     . ("pylint" "python"))))

(defun pl-lint-or-execute (action)
  "Lint or execute the current file."
  (when (or (null (buffer-file-name))
            (buffer-modified-p))
    (save-buffer))
  (let* ((ext (file-name-extension (buffer-file-name)))
         (fileinfo (cdr (assoc ext pl-execute-info)))
         (cmd-index (if (string-equal "lint" action) 0 1)))
    (unless fileinfo
      (error "Unsupported file type"))
    (let ((cmd (nth cmd-index fileinfo)))
      (unless cmd
        (error "Unsupported action on this file type"))
      (compile (concat cmd " " (buffer-file-name))))))

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
