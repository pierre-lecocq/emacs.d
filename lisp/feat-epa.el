;;; feat-epa.el --- EasyPG support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-24 09:53:33>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package epa-file :demand t :ensure nil
  :init (setq epa-gpg-program "gpg2")
  :config (epa-file-enable))

;;; feat-epa.el ends here
