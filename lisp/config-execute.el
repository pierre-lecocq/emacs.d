;;; config-execute.el --- Emacs configuration - execute

;; Time-stamp: <2016-02-15 22:00:34>
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
  "Lint or execute the current file according to ACTION."
  (when (or (null (buffer-file-name))
            (buffer-modified-p))
    (save-buffer))
  (let ((file-name (if (file-remote-p (buffer-file-name))
                       (aref (tramp-dissect-file-name (buffer-file-name)) 3)
                     (buffer-file-name))))
    (let* ((ext (file-name-extension file-name))
           (fileinfo (cdr (assoc ext pl-execute-info)))
           (cmd-index (if (string-equal "lint" action) 0 1)))
      (unless fileinfo
        (error "Unsupported file type \"%s\"" ext))
      (let ((cmd (nth cmd-index fileinfo)))
        (unless cmd
          (error "Unsupported action \"%s\" on this file type" action))
        (compile (concat cmd " " file-name))))))

(defun pl-lint ()
  "Lint the current file."
  (interactive)
  (pl-lint-or-execute "lint"))

(defun pl-execute ()
  "Execute the current file."
  (interactive)
  (pl-lint-or-execute "execute"))

(provide 'config-execute)

;;; config-execute.el ends here
