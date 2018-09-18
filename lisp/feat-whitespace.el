;;; feat-whitepace.el --- Whitepace support -*- lexical-binding: t; -*-

;; Time-stamp: <2018-09-18 10:22:23>
;; Copyright (C) 2018 Pierre Lecocq

;;; Commentary:

;;; Code:

;; Hack for character highlight

(defun load-whitespace-character-hack ()
  "Load whitespace-character hack."
  (require 'whitespace)

  (defvar whitespace-character 'whitespace-character)
  (make-obsolete-variable 'whitespace-character "customize the face `whitespace-character' instead." "24.4")

  (defface whitespace-character '((((class mono)) :inverse-video t) (t :background "red"))
    "Face used to visualize the character at `whitespace-line-column'."
    :group 'whitespace)

  (defun whitespace-character-color-on ()
    "Turn on character color visualization."
    (when (whitespace-style-face-p)
      (setq whitespace-font-lock-keywords
            `((whitespace-point--flush-used)
              ,@(when (memq 'character whitespace-active-style)
                  `((,(format "^\\(.\\)\\{%d\\}" (+ whitespace-line-column 1)) 1 whitespace-character t)))))
      (font-lock-add-keywords nil whitespace-font-lock-keywords t)
      (font-lock-flush)))

  (add-hook #'whitespace-mode-hook #'whitespace-character-color-on)
  (add-hook #'global-whitespace-mode-hook #'whitespace-character-color-on))

;; Config

(use-package whitespace :demand t :ensure nil :diminish whitespace-mode
  :config (when (display-graphic-p)
            (let ((color (face-attribute 'default :background)))
              (set-face-attribute 'whitespace-space nil
                                  :background color
                                  :foreground color)))
  :init (progn
          (load-whitespace-character-hack)
          (setq whitespace-line-column 80
                whitespace-style '(character spaces space-mark tabs tab-mark face trailing)))
  :hook ((prog-mode . whitespace-mode)
         (before-save . whitespace-cleanup)
         (before-save . delete-trailing-whitespace)))

;;; feat-whitepace.el ends here
