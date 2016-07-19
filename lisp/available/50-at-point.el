;;; 50-at-point.el --- At point actions

;; Time-stamp: <2016-07-12 13:07:00>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(defvar se-url-fmt "https://www.google.com/?q=%s")

(defun pl-thing-at-point-or-region (qualifier)
  "Get thing at point according to QUALIFIER or the region."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point qualifier)))

(defun pl-browse-at-point ()
  "Browse url or region at point."
  (interactive)
  (browse-url (pl-thing-at-point-or-region 'url)))

(defun pl-search-at-point ()
  "Search word or region at point."
  (interactive)
  (browse-url (format se-url-fmt (pl-thing-at-point-or-region 'word))))

(defun pl-documentation-at-point ()
  "Documentation for word or region at point."
  (interactive)
  (let ((fmt (cond ((string= major-mode "lisp-mode") "http://lispdoc.com/?q=%s&search=Basic+search")
                   ((string= major-mode "ruby-mode") "http://apidock.com/ruby/search?query=%s&commit=Search")
                   ((string= major-mode "php-mode") "http://php.net/%s")
                   (t se-url-fmt))))
    (browse-url (format fmt (pl-thing-at-point-or-region 'word)))))

(global-set-key (kbd "C-x x") 'pl-browse-at-point)

;;; 50-at-point.el ends here
