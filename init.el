;;
;; Bootstrap
;;
;; In .emacs, simply add this:
;;
;;     (add-to-list 'load-path "~/.emacs.d")
;;     (load "my-bootstrap")
;;

(add-to-list 'load-path "~/.emacs.d")

;; User
(setq user-full-name "Pierre Lecocq")
(setq user-mail-address "pierre.lecocq@gmail.com")

;; Changelog
(setq change-log-default-name "CHANGELOG")

;; Locale
(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq
 el-get-packages
 '(el-get
   org-mode
   yasnippet
   autopair
   auto-complete
   multiple-cursors
   auto-highlight-symbol
   php-mode-improved
   ruby-mode
   js2-mode
   magit
   shell-pop
   color-theme
   buffer-move
))

(el-get 'sync el-get-packages)

;; Load custom scripts
(load "my-behaviour")
(load "my-display")
(load "my-shell")
(load "my-keys")
