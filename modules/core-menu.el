;;; core-menu.el --- Menu -*- lexical-binding: t; -*-

;; Time-stamp: <2020-05-29 16:17:57>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(easy-menu-define my-menu global-map "CheatSheet"
  '("CheatSheet"
    ["Open project" projectile-switch-project :active (projectile-project-p)]
    ["Open project root directory" projectile-dired :active (projectile-project-p)]
    ["Open file in project" projectile--find-file :active (projectile-project-p)]
    ["Search in project" projectile-ag :active (projectile-project-p)]
    ["Replace in project" projectile-replace :active (projectile-project-p)]
    ["Regenerate project TAGS" projectile-regenerate-tags :active (projectile-project-p)]
    ["Find in project TAGS" projectile-find-tag :active (projectile-project-p)]
    ["Run shell command at project root" projectile-run-command-in-root :active (projectile-project-p)]
    ("---")
    ["Jump to external implementation" dumb-jump-go-prefer-external]
    ["Jump to external implementation in another window" dumb-jump-go-prefer-external-other-window]
    ["Jump to somewhere (prompt)" dumb-jump-go-prompt]
    ["Jump to last point" dumb-jump-back]
    ("---")
    ["String to underscore (EmacsLisp => emacs_lisp)" string-inflection-underscore-function]
    ["String to camel case (emacs_lisp => emacsLisp)" string-inflection-camelcase-function]
    ["String to pascal case (emacs_lisp => EmacsLisp)" string-inflection-pascale-case-function]
    ["String to upcase (emacs_lisp => EMACS_LISP)" string-inflection-upcase-function]
    ["String to kebab (emacs_lisp => emacs-lisp)" string-inflection-kebab-function]
    ["String to capital underscore (emacs_lisp => Emacs-Lisp)" string-inflection-capital-underscore-function]
    ("---")
    ["Toggle imenu list" imenu-list]
    ["Toggle treemacs" treemacs]
    ["Toggle line numbers" toggle-linenum-mode]
    ["Toggle parenthesis highlight" toggle-show-paren-mode-style]
    ["Toggle indentation highlight" toggle-highlight-indentation]
    ["Toggle column indicator" toggle-fill-column-indicator]
    ["Toggle whitespaces" toggle-whitespace-mode-style]
    ("---")
    ["Magit" magit-status]
    ["Persistent notes" open-persistent-notes-buffer]
    ["View in HTML" markdown-view-html :active (string= major-mode "mardown-mode")]
    ("---")))

(global-set-key (kbd "<f12>") #'(lambda()
                                  (interactive)
                                  (x-popup-menu t my-menu)))

(provide 'core-menu)

;;; core-menu.el ends here
