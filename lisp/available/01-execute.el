;;; 01-execute.el --- Execute command on a buffer

;; Time-stamp: <2016-06-28 10:59:30>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

;; ("extention" . ("execute command" "lint command"))
(defvar pl-execute-info '(("lisp"   . ("sbcl --noinform --load" nil))
                          ("sh"     . ("bash"                   nil))
                          ("rb"     . ("ruby"                   "ruby -c"))
                          ("php"    . ("php"                    "php -l"))
                          ("py"     . ("python"                 "pylint"))))

(defun pl-lint-or-execute (action)
  "Lint or execute the current file according to ACTION."
  (when (or (null (buffer-file-name))
            (buffer-modified-p))
    (save-buffer))
  (let* ((file-name (if (file-remote-p (buffer-file-name))
                        (aref (tramp-dissect-file-name (buffer-file-name)) 3)
                      (buffer-file-name)))
         (ext (file-name-extension file-name))
         (fileinfo (cdr (assoc ext pl-execute-info)))
         (cmd-index (if (string-equal "lint" action) 1 0)))
    (unless fileinfo
      (error "Unsupported file type \"%s\"" ext))
    (let ((cmd (nth cmd-index fileinfo)))
      (unless cmd
        (error "Unsupported action \"%s\" on this file type" action))
      (compile (concat cmd " " file-name))
      (select-window (get-buffer-window "*compilation*")))))

(defun pl-lint ()
  "Lint the current file."
  (interactive)
  (pl-lint-or-execute "lint"))

(defun pl-execute ()
  "Execute the current file."
  (interactive)
  (pl-lint-or-execute "execute"))

;;; 01-execute.el ends here
