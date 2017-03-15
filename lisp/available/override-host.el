;;; override-host.el --- Host specific file

;; Time-stamp: <2017-02-27 09:24:18>
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
      (when (member "Inconsolata" (font-family-list))
        (set-face-attribute 'default nil
                            :family "Inconsolata" ;; "DejaVu Sans Mono"
                            :height 120
                            :weight 'normal
                            :width 'normal))))

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
