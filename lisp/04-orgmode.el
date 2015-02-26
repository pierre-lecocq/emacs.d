;;; 04-orgmode.el --- Emacs Config - OrgMode

;;; Commentary:
;; Time-stamp: <2015-02-25 23:39:39 pierre>
;; Copyright (C) 2015 Pierre Lecocq

;;; Code:

(setq
 org-fontify-done-headline t
 org-src-fontify-natively t
 org-agenda-files (list
                   (concat org-files-dir "agenda.org")
                   ;; Add other files here ...
                   )
 org-default-notes-file (format "%sorg/notes.org" user-emacs-directory)
 org-capture-templates
 '(("t" "Todo" entry (file+headline (concat org-files-dir "agenda.org") "Tasks")
    "* TODO %?\n  %U\n  %i\n  %a\n")
   ("j" "Journal" entry (file+datetree (concat org-files-dir "notes.org"))
    "* %?\nEntered on %U\n  %i\n  %a\n")
   ("w" "Webplace" entry (file (concat org-files-dir "webplaces.org"))
    "* %^L %^g\n")))

(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "Grey55" :strike-through t)))))

(defun org-font-lock-ensure ()
  "Org font lock ensure."
  (font-lock-fontify-buffer))

;;; 04-orgmode.el ends here
