;;; 90-host.el --- Host specific file

;; Time-stamp: <2016-09-15 13:32:32>
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

   ((or (string= host "beastro" )
        (string= host "laptaupe" ))
    (progn
      (when (display-graphic-p)
        (pl-transparency 90))
      (add-to-list 'bookmark-alist '("Raspberry"     (filename . "/scp:kenny:~/")))
      (add-to-list 'bookmark-alist '("Qsdfgh"     (filename . "/scp:qsdfgh:~/")))))

   ((string= host "lecocq-s")
    (progn
      (add-to-list 'bookmark-alist '("AdobeStock" (filename . "/scp:eqx-dev2:~/www/adobestock")))
      (add-to-list 'bookmark-alist '("Fotolia" (filename . "/scp:eqx-dev1:~/www/fotolia")))))

   (t (message "No host specific configuration loaded"))))

(provide '90-host)

;;; 90-host.el ends here
