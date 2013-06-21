;;
;; Packages
;;

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
   powerline
   anything
   buffer-move
))

(el-get 'sync el-get-packages)