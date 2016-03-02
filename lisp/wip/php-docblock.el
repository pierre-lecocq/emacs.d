;;; php-docblock.el --- PHP docblock generator

;; Time-stamp: <2016-03-02 14:43:28>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;; This package generates a PHP docblock when php-docblock-generate is called
;; on an appropriate line of code.
;;
;; Features:
;;
;; - Auto detect parameters (typehinting, name, default value)
;; - Auto detect exception thrown in the function runtime
;; - Auto detect return calls
;; - Create a new docblock or update an existing one
;;
;; TODOs:
;;
;; - Multi exceptions types thrown
;; - Class level docblock (name, author, file, ...)

;;; Code:

(defvar php-docblock--function-line-regexp
  "^[[:space:]]*\\(public\\|private\\|protected\\)+[[:space:]]+\\(static\\)?[[:space:]]*function[[:space:]]+\\([a-zA-Z0-9_-]+\\)(\\(.*\\))")

;; Parsing

(defun php-docblock--parse-line-at-point ()
  "Parse the line at point."
  (let ((line (thing-at-point 'line t))
        (indent-level 0))
    (if (string-match php-docblock--function-line-regexp line)
        (progn
          (let ((still-in-space t))
            (mapcar (lambda (c)
                      (when still-in-space
                        (if (char-equal c ?\s)
                            (setq indent-level (+ indent-level 1))
                          (setq still-in-space nil)))) line))
          `((type . ,(match-string 1 line))
            (static . ,(match-string 2 line))
            (name . ,(match-string 3 line))
            (params . ,(match-string 4 line))
            (indent . ,indent-level)))
      (error "This line can not be used to generate a docblock"))))

(defun php-docblock--parse-param-string (param-string)
  "Parse the PARAM-STRING."
  (let ((index 0)
        (type nil)
        (name nil)
        (default nil)
        (next-is-default nil)
        (param-list (split-string param-string "[[:space:]]")))
    (while param-list
      (let ((elem (car param-list)))
        (when (>= (length elem) 1)
          (if next-is-default
              (progn
                (setq default elem)
                (setq next-is-default nil))
            (progn
              (when (string-equal "=" elem)
                (setq next-is-default t))
              (if (string-equal "$" (substring elem 0 1))
                  (setq name elem)
                (if (= index 0)
                    (setq type elem)))))))
      (setq param-list (cdr param-list))
      (setq index (+ index 1)))
    (concat (if type (concat type " ") "")
            name
            (if default (concat " (default is " default ")") ""))))

(defun php-docblock--parse-lines (indent-level regexp index)
  "Parse line until a closed parenthese has been found at INDENT-LEVEL or match the REGEXP and return the group at INDEX."
  (let ((thing-found nil))
    (with-current-buffer (current-buffer)
      (save-excursion
        (while (and (not (eobp))
                    (not thing-found))
          (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
            (when (string-match regexp line)
              (setq thing-found (match-string index line)))
            (when (string-match (concat "^[[:space:]]\\{" (number-to-string indent-level) "\\}}\\{1\\}") line)
              (setq thing-found t)))
          (forward-line 1))))
    thing-found))

;; Docblock chunks

(defun php-docblock--summary-string (alist)
  "Generate the summary from ALIST."
  (concat "\n* "
          (capitalize (cdr (assoc 'type alist)))
          (if (cdr (assoc 'static alist)) " static " " ")
          "function to "
          (cdr (assoc 'name alist))))

(defun php-docblock--params-string (params-string)
  "Generate the params tags from PARAMS-STRING."
  (when params-string
    (let ((str nil)
          (params-list (split-string params-string ",[[:space:]]*")))
      (while params-list
        (let ((result (php-docblock--parse-param-string (car params-list))))
          (setq str (concat str "\n* @param " result))
          (setq params-list (cdr params-list))))
      (if str (concat "\n*" str) nil))))

(defun php-docblock--throws-string (indent-level)
  "Generate the throws tag."
  (let ((str nil)
        (result (php-docblock--parse-lines indent-level "[[:word:]]+\s+\\([a-zA-Z0-9-_:]*Exception\\)+" 1)))
    (when (stringp result)
      (setq str (concat "\n*\n* @throws " result)))
    str))

(defun php-docblock--return-string (indent-level)
  "Generate the return tag."
  (let ((str nil)
        (result (php-docblock--parse-lines indent-level "return \\([[:word:]]\\)+" 1)))
    (when (stringp result)
      (setq str "\n*\n* @return"))
    str))

(defun php-docblock--docblock-string (summary-string &optional params-string return-string throws-string)
  "Generate the whole docblock with SUMMARY-STRING as mandatory argument."
  (concat "/**"
          summary-string
          params-string
          throws-string
          return-string
          "\n*/"))

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
  (let* ((alist (php-docblock--parse-line-at-point))
         (summary-string (php-docblock--summary-string alist))
         (params-string (php-docblock--params-string (cdr (assoc 'params alist))))
         (throws-string (php-docblock--throws-string (cdr (assoc 'indent alist))))
         (return-string (php-docblock--return-string (cdr (assoc 'indent alist)))))
    (php-docblock--insert (php-docblock--docblock-string summary-string params-string return-string throws-string))))

;; Keybindings

(global-set-key (kbd "C-c d") 'php-docblock-generate)

;;; php-docblock.el ends here
