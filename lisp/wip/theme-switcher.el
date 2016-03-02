(defvar pl-resource-cache-filename "~/.emacs-theme-switcher")
(defvar pl-resource-index nil)
(defvar pl-resources-list '(("tango-dark")
                            ("tango" (lambda ()
                                       (global-hl-line-mode -1)))
                            ("wombat" (lambda ()
                                        (global-hl-line-mode 1)
                                        (setq show-paren-style 'parenthesis)))
                            ("whiteboard")))

(defun pl--resource-index ()
  (interactive)
  (unless pl-resource-index
    (let ((index 0))
      (when pl-resource-cache-filename
        (if (file-readable-p pl-resource-cache-filename)
            (setq index (string-to-number (insert-file-contents pl-resource-cache-filename)))
          (write-region "" nil pl-resource-cache-filename)))
      (setq pl-resource-index index)))
  pl-resource-index)

(message "Index : %d" (pl--resource-index))

;; (defun pl--next-resource-index ()
;;   (1+ pl--resource-index))

;; (defun pl--save-resource-index (new-index)
;;   (setq pl-resource-index new-index)
;;   (when (and pl-resource-cache-filename
;;              (file-writable-p pl-resource-cache-filename))
;;     (write-region (number-to-string new-index) nil pl-resource-cache-filename)

;;     ;; (with-temp-buffer
;;     ;;   (insert new-index)
;;     ;;   (write-region (point-min) (point-max) pl-resource-cache-filename))

;;     ))

;; (defun pl-switch-resource2 ()
;;   "Switch resource between dark and light ones."
;;   (interactive)
;;   (let* ((index (pl--next-resource-index))
;;          (resource (nth index pl-resources-list))
;;          (name (car resource))
;;          (modifier (car (cdr resource))))
;;     (message "Loading theme #%d: %s" index name)
;;     (load-theme (intern name) t)
;;     (when (functionp modifier)
;;       (funcall modifier))
;;     (pl--save-resource-index index)))
