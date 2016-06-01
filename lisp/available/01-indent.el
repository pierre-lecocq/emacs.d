;;; 01-indent.el --- Indentation

;; Time-stamp: <2016-02-29 00:01:42>
;; Copyright (C) 2015 Pierre Lecocq

;;; Commentary:

;;; Code:

(setq-default indent-tabs-mode nil
	      tab-width 4
              c-basic-offset 4
              c-hanging-comment-ender-p nil)

(c-set-offset 'arglist-intro '+)
(c-set-offset 'arglist-close '0)

;;; 01-indent.el ends here
