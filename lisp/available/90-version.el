;;; 90-version.el --- Version specific file

;; Time-stamp: <2016-06-28 11:09:39>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(when (display-graphic-p)
  (unless (version< emacs-version "24.4")
    (toggle-frame-maximized)))

;;; 90-version.el ends here
