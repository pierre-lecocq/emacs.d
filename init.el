;;
;; Emacs config file - Pierre Lecocq
;; Load all configs from org files code blocks (~/.emacs.d/config/*.org)
;;

(require 'org-install)
(require 'ob-tangle)

(setq org-files-config-dir (format "%sconfig" user-emacs-directory))
(add-to-list 'load-path org-files-config-dir)

;; Set org config files to load

(setq org-files (list
         (format "%s/01-packages.org" org-files-config-dir)
         (format "%s/02-common.org" org-files-config-dir)
         (format "%s/03-modes.org" org-files-config-dir)
         (format "%s/04-keybindings.org" org-files-config-dir)
         (format "%s/09-%s.org" org-files-config-dir (downcase (car (split-string system-name "\\."))))))

;; Load org config files

(mapc (lambda (org-file)
        (setq el-file (replace-regexp-in-string "\\.org$" ".el" org-file))
        (unless (file-exists-p el-file)
          (if (file-exists-p org-file)
              (progn
                (condition-case nil
                    (org-babel-load-file org-file)
                  (error "Error while loading code from %s" org-file)))
            (error "Can not load config file %s" org-file)))
        (load el-file)) org-files)

;; EOF
