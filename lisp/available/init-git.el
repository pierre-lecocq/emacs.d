;;; init-git.el --- Git

;; Time-stamp: <2016-09-13 23:32:46>
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
            (set-face-foreground 'git-gutter:deleted "red"))
  :diminish git-gutter-mode)

(provide 'init-git)

;;; init-git.el ends here
