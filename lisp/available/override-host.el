;;; override-host.el --- Host specific file

;; Time-stamp: <2017-03-15 11:45:43>
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
      (add-to-list 'bookmark-alist '("Remote AdobeStock" (filename . "/scp:eqx-dev2:~/www/adobestock")))
      (add-to-list 'bookmark-alist '("Remote Fotolia" (filename . "/scp:eqx-dev1:~/www/fotolia")))
      (add-to-list 'bookmark-alist '("Local AdobeStock" (filename . "~/src/stock-web")))
      (add-to-list 'bookmark-alist '("Local Docker" (filename . "~/src/docker-stack")))
      (add-to-list 'bookmark-alist '("Local Fotolia" (filename . "~/src/fotolia-web")))))

   (t (message "No host specific configuration loaded"))))

(provide 'override-host)

;;; override-host.el ends here
