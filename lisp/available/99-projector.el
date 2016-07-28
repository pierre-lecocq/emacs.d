;;; 99-projector.el --- Project manager

;; Time-stamp: <2016-07-28 22:42:33>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar projector-file ".projector.el")
(defvar projector--dir nil)

(defun projector-hook ()
  "Projector hook."
  (let ((dname (file-name-directory buffer-file-name))
        (fname (file-name-nondirectory buffer-file-name)))

    ;; if file is in scope


    ;; activate
    (when (string= fname projector-file)
      (when (not (string= dname projector--dir))
        (yes-or-no-p "Do you want to activate projector? ")
        (message "Yes")
        ))


      ))

(add-hook 'find-file-hook 'projector-hook)

;;; 99-projector.el ends here
