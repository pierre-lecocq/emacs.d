;;; 90-host.el --- Host specific file

;; Time-stamp: <2016-06-28 11:09:24>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun pl-clean-system-name ()
  "Get a clean system name."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "-" (downcase (car (split-string (system-name) "\\.")))))

;; Hosts are:
;;     - beastro (home)
;;     - laptaupe (home)
;;     - fritebsd (home)
;;     - lecocq-s (work)

(let ((host (pl-clean-system-name)))
  (cond
   ((string= host "beastro" ) (progn (when (display-graphic-p) (pl-transparency 90))))
   (t (message "No host specific configuration loaded"))))

;;; 90-host.el ends here
