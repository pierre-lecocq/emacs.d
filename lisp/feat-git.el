;;; feat-git.el --- Git support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-12 10:26:39>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

(use-package git-gutter :ensure t :diminish git-gutter-mode
  :config (progn (set-face-background 'git-gutter:added nil)
                 (set-face-foreground 'git-gutter:added "green")
                 (set-face-background 'git-gutter:modified nil)
                 (set-face-foreground 'git-gutter:modified "yellow")
                 (set-face-background 'git-gutter:deleted nil)
                 (set-face-foreground 'git-gutter:deleted "red")
                 (global-git-gutter-mode +1)))

;; (use-package magit :ensure t)

;;; feat-git.el ends here
