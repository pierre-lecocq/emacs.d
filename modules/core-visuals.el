;;; core-visuals.el --- Visuals -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-22 11:47:55>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(defun toggle-show-paren-mode-style ()
  "Toggle 'show-paren-mode' style."
  (interactive)
  (let* ((min 'parenthesis)
         (max 'expression)
         (style (if (equal show-paren-style min) max min)))
    (setq show-paren-style style)))

(defun toggle-fill-column-indicator ()
  "Toggle 'display-fill-column-indicator'."
  (interactive)
  (global-display-fill-column-indicator-mode (if (bound-and-true-p global-display-fill-column-indicator-mode) -1 1)))

(use-package highlight-indentation :ensure t)

(defun toggle-highlight-indentation ()
  "Toggle 'highlight-indentation'."
  (interactive)
  (highlight-indentation-mode (if (bound-and-true-p highlight-indentation-mode) -1 1)))

(defun toggle-line-numbers-mode ()
  "Toggle 'line-numbers-mode'."
  (interactive)
  (global-display-line-numbers-mode (if (bound-and-true-p global-display-line-numbers-mode) -1 1)))

(defun toggle-whitespace-mode-style ()
  "Toggle 'whitespace-mode' style."
  (interactive)
  (let* ((min '(tabs tab-mark face trailing))
         (max '(tabs tab-mark spaces space-mark lines lines-tail newline newline-mark empty face trailing))
         (style (if (equal whitespace-active-style min) max min)))
    (setq whitespace-style style))
  (whitespace-turn-off)
  (whitespace-turn-on))

(progn
  (define-prefix-command 'visuals-keymap)
  (define-key visuals-keymap (kbd "<f1>") 'toggle-line-numbers-mode)
  (define-key visuals-keymap (kbd "<f2>") 'toggle-fill-column-indicator)
  (define-key visuals-keymap (kbd "<f3>") 'toggle-whitespace-mode-style)
  (define-key visuals-keymap (kbd "<f4>") 'toggle-highlight-indentation)
  (define-key visuals-keymap (kbd "<f5>") 'toggle-show-paren-mode-style))

(global-set-key (kbd "<f11>") visuals-keymap)

(provide 'core-visuals)

;;; core-visuals.el ends here
