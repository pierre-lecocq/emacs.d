;;; override-host.el --- Host specific file

;; Time-stamp: <2017-03-02 22:16:17>
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

   ((string= host "lecocq-s")
    (progn
      (add-to-list 'bookmark-alist '("AdobeStock" (filename . "/scp:eqx-dev2:~/www/adobestock")))
      (add-to-list 'bookmark-alist '("Fotolia" (filename . "/scp:eqx-dev1:~/www/fotolia")))))

   (t (message "No host specific configuration loaded"))))

(provide 'override-host)

;;; override-host.el ends here
