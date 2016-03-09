;;; php-docblock.el --- PHP docblock generator

;; Time-stamp: <2016-03-08 16:35:30>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;; This package generates a PHP docblock when `php-docblock-generate' is called
;; on an appropriate line of code.
;;
;; Features:
;;
;; - Auto detect parameters (typehinting, name, default value)
;; - Auto detect exceptions thrown in the function runtime
;; - Auto detect return calls
;; - Create a new docblock or update an existing one

;;; Code:

(defvar php-docblock--function-line-regexp
  "^[[:space:]]*\\(public\\|private\\|protected\\)+[[:space:]]+\\(static\\)?[[:space:]]*function[[:space:]]+\\([a-zA-Z0-9_-]+\\)(\\(.*\\))")

(defvar php-docblock--indent-level-value)

(defun php-docblock--indent-level (line)
  "Compute the indentation level of LINE"
  (let ((indent-level 0)
        (still-in-space t))
    (mapcar (lambda (c)
              (when still-in-space
                (if (char-equal c ?\s)
                    (setq indent-level (1+ indent-level))
                  (setq still-in-space nil))))
            line)
    (setq php-docblock--indent-level-value indent-level)))

(defun php-docblock--find-in-function (regexp index)
  "Parse line until a closed parenthese has been found or match the REGEXP and return the group at INDEX."
  (let ((alist '())
        (end-of-func nil))
    (with-current-buffer (current-buffer)
      (save-excursion
        (forward-line 1)
        (while (and (not (eobp))
                    (not end-of-func))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (when (and (string-match regexp line)
                       (not (member (match-string index line) alist)))
              (push (match-string index line) alist))
            (when (string-match (concat "^[[:space:]]\\{" (number-to-string php-docblock--indent-level-value) "\\}}\\{1\\}") line)
              (setq end-of-func t)))
          (forward-line 1))))
    (reverse alist)))

(defun php-docblock--remove-existing-docblock ()
  "Remove an existing docblock."
  (let ((start nil)
        (stop nil)
        (begining-of-docblock nil))
    (with-current-buffer (current-buffer)
      (save-excursion
        (forward-line -1)
        (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (when (string-match "^[[:space:]]+\\*\\/" line)
            (setq stop (point-at-eol))
            (while (and (not (bobp))
                        (not begining-of-docblock))
              (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (if (string-match "^[[:space:]]*\\/\\*\\*" line)
                  (progn
                    (forward-line -1)
                    (setq start (point-at-eol))
                    (setq begining-of-docblock t))
                (forward-line -1)))))))
    (when (and start
               stop
               (yes-or-no-p "Do you want to remove existing docblock? "))
      (delete-region start stop))))

;; Lists generation

(defun php-docblock--params-list (params-string)
  "Compute params list from PARAMS_STRING"
  (when params-string
    (let ((alist '())
          (groups (split-string params-string ",[[:space:]]*")))
      (while groups
        (let ((index 0)
              (type nil)
              (name nil)
              (default nil)
              (next-is-default nil)
              (words (split-string (car groups) "[[:space:]]")))
          (while words
            (let ((word (car words)))
              (when (>= (length word) 1)
                (if next-is-default
                    (progn
                      (setq default word)
                      (setq next-is-default nil))
                  (progn
                    (when (string-equal "=" word)
                      (setq next-is-default t))
                    (if (string-equal "$" (substring word 0 1))
                        (setq name word)
                      (when (= index 0)
                        (setq type word)))))))
            (setq words (cdr words))
            (setq index (1+ index)))
          (when name
            (push `((type . ,type)
                    (name . ,name)
                    (default . ,default)) alist)))
        (setq groups (cdr groups)))
      (reverse alist))))

(defun php-docblock--throws-list ()
  "Generate the throws tag."
  (php-docblock--find-in-function "[[:word:]]+\s+\\([a-zA-Z0-9-_:]*Exception\\)+" 1))

(defun php-docblock--return-list ()
  "Generate the return tag."
  (php-docblock--find-in-function "return \\(\\$?[[:word:]]+\\)" 1))

(defun php-docblock--function-line-to-list ()
  "Parse the line at point."
  (let* ((line (thing-at-point 'line t)))
    (if (string-match php-docblock--function-line-regexp line)
        (progn
          (php-docblock--indent-level line)
          (let ((alist '()))
            (push `(type . ,(match-string 1 line)) alist)
            (push `(static . ,(match-string 2 line)) alist)
            (push `(name . ,(match-string 3 line)) alist)
            (push `(params . ,(php-docblock--params-list (match-string 4 line))) alist)
            (push `(throws . ,(php-docblock--throws-list)) alist)
            (push `(return . ,(php-docblock--return-list)) alist)
            alist))
      (error "This line can not be used to generate a docblock"))))

(defun php-docblock--list-to-string (alist)
  (let ((str "")
        (name (cdr (assoc 'name alist)))
        (type (cdr (assoc 'type alist)))
        (static (cdr (assoc 'static alist)))
        (params (cdr (assoc 'params alist)))
        (throws (cdr (assoc 'throws alist)))
        (return (cdr (assoc 'return alist))))
    (setq str (concat str "/**\n * " (capitalize type)))
    (when static
      (setq str (concat str " static")))
    (setq str (concat str " function to " name))
    (when params
      (setq str (concat str "\n *"))
      (while params
        (let* ((elem (car params))
               (type (cdr (assoc 'type elem)))
               (name (cdr (assoc 'name elem)))
               (default (cdr (assoc 'default elem))))
          (setq str (concat str "\n * @param"
                            (when type (concat " " type))
                            (when name (concat " " name))
                            (when default (concat " (default value is " default ")")))))
        (setq params (cdr params))))
    (when throws
      (setq str (concat str "\n *"))
      (while throws
        (setq str (concat str "\n * @throws " (car throws)))
        (setq throws (cdr throws))))
    (when return
      (setq str (concat str "\n *\n * @return " (car return))))
    (setq str (concat str "\n */"))
    str))

;; Write

(defun php-docblock--insert (docblock-string)
  "Insert docblock at the right position."
  (save-excursion
    (let ((start (point)))
      (beginning-of-line)
      (insert (concat docblock-string "\n"))
      (indent-region start (point)))))

;; Main

;;;###autoload
(defun php-docblock-generate ()
  "Generate docblock for current line."
  (interactive)
  (unless (string-equal major-mode "php-mode")
    (error "This file is not a PHP file"))
  (let ((alist (php-docblock--function-line-to-list)))
    (php-docblock--remove-existing-docblock)
    (php-docblock--insert (php-docblock--list-to-string alist))))

(global-set-key (kbd "C-c d") 'php-docblock-generate)

;;; php-docblock.el ends here
