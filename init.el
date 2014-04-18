;;
;; Emacs config file - Pierre Lecocq
;; Load all configs from org files code blocks (~/.emacs.d/config/*.org)
;;

(require 'org-install)
(require 'ob-tangle)

(setq org-files-dir (format "%sconfig" user-emacs-directory))
(setq machine-name (downcase (car (split-string system-name "\\."))))
(add-to-list 'load-path org-files-dir)

;; Set org config files to load

(setq org-files (list
		 (format "%s/packages.org" org-files-dir)
		 (format "%s/common.org" org-files-dir)
		 (format "%s/modes.org" org-files-dir)
		 (format "%s/keybindings.org" org-files-dir)
		 (format "%s/%s.org" org-files-dir machine-name)))

;; Load org config files

(mapc (lambda (org-file)
        (setq el-file (replace-regexp-in-string "\\.org$" ".el" org-file))
        (unless (file-exists-p el-file)
          (if (file-exists-p org-file)
              (progn
                (condition-case nil
                    (org-babel-load-file org-file)
                  (message "Error loading code from %s" org-file)))
            ;; Should be "error" instead of "message"
            (message "Can not load config file %s" org-file))
          )
        (load el-file)) org-files)

;; EOF
