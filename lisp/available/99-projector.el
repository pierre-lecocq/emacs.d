;;; 99-projector.el --- Project manager

;; Time-stamp: <2016-07-29 11:26:17>
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
    (message "Projector has been activated")))

(defun projector--check-and-deactivate ()
  "Check if the user wants to deactiavte projector after missing hits."
  (if (< projector--missed-count projector--missed-max)
      (incf projector--missed-count)
    (when (yes-or-no-p "Do you want to deactivate projector? ")
      (setq projector--dir nil)
      (setq projector--missed-count 0)
      (message "Projector has been deactivated"))))

(defun projector--hook ()
  "Projector hook."
  (let ((dname (file-name-directory buffer-file-name))
        (fname (file-name-nondirectory buffer-file-name)))
    (projector--check-and-activate fname dname)
    (if (and (projector--activated-p)
             (not (projector--in-scope-p dname)))
        (projector--check-and-deactivate))))

(add-hook 'find-file-hook 'projector--hook)

;;; 99-projector.el ends here
