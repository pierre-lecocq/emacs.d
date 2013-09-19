;;
;; Init
;;

(add-to-list 'load-path "~/.emacs.d")

;; ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq
 el-get-packages
 '(auto-complete
   auto-highlight-symbol
   autopair
   buffer-move
   column-marker
   js2-mode
   magit
   multiple-cursors
   php-mode
   php-mode-improved
   rainbow-mode
   ruby-mode
   shell-pop
   twittering-mode
))

(el-get 'sync el-get-packages)

;; Load custom scripts
(load "my-behaviour")
(load "my-display")
(load "my-shell")
(load "my-feeds")
(load "my-keys")
