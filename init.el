;;
;; Emacs config file - Pierre Lecocq
;; Load all configs from org files code blocks (~/.emacs.d/config/org/*.org)
;;

;; Prepare directories

(setq config-dir-org (format "%sconfig/org" user-emacs-directory))
(unless (file-exists-p config-dir-org)
  (error (format "Missing %s directory" config-dir-org)))

(setq config-dir-elisp (format "%sconfig/elisp" user-emacs-directory))
(unless (file-exists-p config-dir-elisp)
  (make-directory config-dir-elisp))

;; Set load path

(add-to-list 'load-path config-dir-elisp)

;; Set org config files to load

(setq org-files
      (list (format "%s/01-packages.org" config-dir-org)
            (format "%s/02-common.org" config-dir-org)
            (format "%s/03-modes.org" config-dir-org)
            (format "%s/04-keybindings.org" config-dir-org)
            (format "%s/09-%s.org" config-dir-org (downcase (car (split-string system-name "\\."))))))

;; Load org config files

(require 'org-install)
(require 'ob-tangle)

(mapc (lambda (org-file)
        (let ((el-file (replace-regexp-in-string "/org/\\([-_0-9A-Za-z]+\\)\\.org" "/elisp/\\1.el" org-file)))
          (unless (file-exists-p el-file)
            (if (file-exists-p org-file)
                (progn
                  (condition-case nil
                      (org-babel-tangle-file org-file el-file "emacs-lisp")
                    (error "Error while loading code from %s" org-file)))
              (error "Can not load config file %s" org-file)))
          (load-file el-file))
        ) org-files)

;; EOF
