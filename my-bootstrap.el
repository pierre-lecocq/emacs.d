;;
;; Bootstrap
;;
;; In .emacs, simply add this:
;;
;;     (add-to-list 'load-path "~/.emacs.d")
;;     (load "my-bootstrap")
;;

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

;; Load custom scripts
(load "my-packages")
(load "my-behaviour")
(load "my-display")
(load "my-shell")
(load "my-keys")
