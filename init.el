;;; init.el --- Emacs Config - Main init file

;;; Commentary:
;; Time-stamp: <2015-02-25 23:39:18 pierre>
;; Copyright (C) 2015 Pierre Lecocq

(setq debug-on-error t)

;;; Code:

(setq
 user-full-name "Pierre Lecocq"
 user-mail-address "pierre.lecocq@gmail.com")

;; Generic variables

(setq
 ;; Files and path
 package-user-dir "~/.emacs.d/packages"
 custom-file "~/.emacs.d/custom.el"
 bookmark-default-file "~/.emacs.d/bookmarks"
 org-files-dir "~/.emacs.d/org"
 ;; No backups
 backup-inhibited t
 make-backup-files nil
 auto-save-default nil
 ;; Startup buffer
 inhibit-startup-message t
 inhibit-splash-screen t
 initial-scratch-message ";; Scratch buffer\n(setq debug-on-error t)\n\n"
 ;; Frame title
 frame-title-format "Emacs %f"
 ;; Kill the whole line
 kill-whole-line t)

;; Set load path

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (let ((default-directory "~/.emacs.d/packages"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Load files

(setq files-to-load
      (list custom-file
            "~/.emacs.d/lisp/01-packages.el"
            "~/.emacs.d/lisp/02-functions.el"
            "~/.emacs.d/lisp/03-autoinsert.el"
            "~/.emacs.d/lisp/04-orgmode.el"
            "~/.emacs.d/lisp/09-keybindings.el"
            (format "~/.emacs.d/lisp/99-%s.el" (downcase (car (split-string system-name "\\."))))))

(dolist (f files-to-load)
  (when (file-exists-p f)
    (load-file f)))

;; Call initialization functions

(setq init-functions
      (list #'pl--init-look-and-feel
            #'pl--init-indentation
            #'pl--init-files-modes
            #'pl--init-hooks))

(dolist (f init-functions)
  (declare-function f "~/.emacs.d/lisp/02-functions.el" nil)
  (funcall f))

;;; init.el ends here
