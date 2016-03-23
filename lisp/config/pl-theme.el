;;; pl-theme.el --- Emacs configuration - theme

;; Time-stamp: <2016-03-23 17:04:14>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package darkmine-theme :ensure t)
(use-package tao-theme :ensure t)

(defvar switch-theme-list '(("darkmine") ("whiteboard")))
(defvar switch-theme-cache-filename (concat files-dir "theme"))
(defvar switch-theme--index nil)
(defvar switch-theme--current nil)

(defun switch-theme--save-index (new-value)
  "Save index with NEW-VALUE."
  (setq switch-theme--index new-value)
  (when switch-theme-cache-filename
    (write-region (format "%s" new-value) nil switch-theme-cache-filename)))

(defun switch-theme--index ()
  "Get current index."
  (unless switch-theme--index
    (let ((index 0))
      (if (and switch-theme-cache-filename
               (file-exists-p switch-theme-cache-filename))
          (progn
            (unless (file-readable-p switch-theme-cache-filename)
              (error "File %s is not readable" switch-theme-cache-filename))
            (with-temp-buffer
              (insert-file-contents switch-theme-cache-filename)
              (setq switch-theme--index (string-to-number (buffer-string)))))
        (switch-theme--save-index index))))
  switch-theme--index)

(defun switch-theme--next-index ()
  "Get next index."
  (switch-theme--save-index (if (eq switch-theme--index (1- (length switch-theme-list)))
                                0
                              (1+ switch-theme--index)))
  switch-theme--index)

(defun switch-theme--theme ()
  "Get theme."
  (let ((index (if switch-theme--index (switch-theme--next-index) (switch-theme--index))))
    (nth index switch-theme-list)))

(defun switch-theme ()
  "Switch Emacs theme."
  (interactive)
  (if (= 0 (length switch-theme-list))
      (warn "Can not switch theme since `switch-theme-list' is empty")
    (let* ((theme-data (switch-theme--theme))
           (theme-name (car theme-data))
           (theme-callback (car (cdr theme-data))))
      (when switch-theme--current
        (disable-theme (intern switch-theme--current)))
      (message "Loading theme %s" theme-name)
      (setq switch-theme--current theme-name)
      (load-theme (intern theme-name) t)
      (when (functionp theme-callback)
        (funcall theme-callback)))))

(switch-theme)

(provide 'pl-theme)

;;; pl-theme.el ends here
