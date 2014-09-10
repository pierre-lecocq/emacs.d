;;
;; Emacs config file - Pierre Lecocq
;; Load all configs from org files code blocks (~/.emacs.d/config/org/*.org)
;;

(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp" t)

;; Prepare directories
(defvar config-dir-org (format "%sconfig/org" user-emacs-directory))
(unless (file-exists-p config-dir-org)
  (error (format "Missing %s directory" config-dir-org)))

(defvar config-dir-elisp (format "%sconfig/elisp" user-emacs-directory))
(unless (file-exists-p config-dir-elisp)
  (make-directory config-dir-elisp))

;; Set org config files to load
(defvar org-files
  (list (format "%s/01-packages.org" config-dir-org)
    (format "%s/02-common.org" config-dir-org)
    (format "%s/03-autoinsert.org" config-dir-org)
    (format "%s/04-orgmode.org" config-dir-org)
    (format "%s/09-keybindings.org" config-dir-org)
    (format "%s/99-%s.org" config-dir-org (downcase (car (split-string system-name "\\."))))))

(unless (file-exists-p (car (last org-files)))
  (write-region "#+begin_src emacs-lisp\n;;\n#+end_src\n" nil (car (last org-files))))

;; Set load path
(add-to-list 'load-path config-dir-elisp)

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
          (load-file el-file))) org-files)

;; EOF
