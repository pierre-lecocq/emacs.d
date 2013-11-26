;;
;; Init
;;

(add-to-list 'load-path "~/.emacs.d")

;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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
   autopair
   buffer-move
   color-theme
   column-marker
   flycheck
   highlight-symbol
   ido-vertical-mode
   js2-mode
   magit
   move-text
   multiple-cursors
   php-mode
   php-mode-improved
   rainbow-mode
   rhtml-mode
   ruby-mode
   shell-pop
   yaml-mode))

(el-get 'sync el-get-packages)

(defun el-get-update-once-a-week()
  (if (= 0 (% 7 (string-to-number (format-time-string "%d"))))
      (el-get-update-all)))

(el-get-update-once-a-week)

;; Load custom scripts
(load "my-behaviour")
(load "my-display")
(load "my-keys")
