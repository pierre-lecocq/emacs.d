;;; 99-projector.el --- Project manager

;; Time-stamp: <2016-07-29 11:15:00>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar projector-file ".projector.el")

(defvar projector--dir nil)

(defvar projector--missed-count 0)
(defvar projector--missed-max 5)

(defun projector--activated-p ()
  "Is projector activated."
  projector--dir)

(defun projector--in-scope-p (dname)
  "Is file located in DNAME is in scope."
  (and (projector--activated-p)
       (string-match-p (regexp-quote projector--dir) dname)))

(defun projector--check-and-activate (fname dname)
  "Activate projector if FNAME is a configuration file from DNAME."
  (when (and (string= fname projector-file)
             (not (string= dname projector--dir))
             (yes-or-no-p "Do you want to activate projector? "))
    (setq projector--dir dname)
    (message " - Activated")))

(defun projector--hook ()
  "Projector hook."
  (let ((dname (file-name-directory buffer-file-name))
        (fname (file-name-nondirectory buffer-file-name)))

    (message " - Dir = %s" projector--dir)
    (message " - Fname = %s" fname)
    (message " - Dname = %s" dname)

    (projector--check-and-activate fname dname)

    (if (projector--activated-p)
        (if (projector--in-scope-p dname)
            (message "I am in the project scope")
          (if (< projector--missed-count projector--missed-max)
              (progn
                (incf projector--missed-count)
                (message "I am NOT in the project scope (%d)" projector--missed-count))
            (when (yes-or-no-p "Do you want to deactivate projector? ")
              (setq projector--dir nil)
              (setq projector--missed-count 0)
              (message " - Deactivated")
              )
            ))
      )

    ))

(add-hook 'find-file-hook 'projector--hook)

;;; 99-projector.el ends here
