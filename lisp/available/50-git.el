;;; 50-git.el --- Git

;; Time-stamp: <2016-09-07 08:36:10>
;; Copyright (C) 2016 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t
  :config (progn
            (custom-set-variables
             '(git-gutter:modified-sign "~")
             '(git-gutter:added-sign "+")
             '(git-gutter:deleted-sign "-"))
            (set-face-foreground 'git-gutter:modified "yellow")
            (set-face-foreground 'git-gutter:added "green")
            (set-face-foreground 'git-gutter:deleted "red")))


;;; 50-git.el ends here
