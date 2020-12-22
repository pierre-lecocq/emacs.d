;;; darquiche-theme.el --- Darquiche theme -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-22 15:46:57>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(deftheme darquiche-colorized "Darquiche colorized theme.")

(let ((class '((class color) (min-colors 89)))
      (fg0 "#ffffff")
      (fg1 "#d4d4d4")
      (fg2 "#808080")
      (fg3 "#565656")
      (bg1 "#1a1a1a")
      (bg2 "#2a2a2a")
      (bg3 "#3a3a3a")
      (blue1 "#1e90ff")
      (blue2 "#275ea2")
      (orange1 "#ffa500")
      (red1 "#cc0000")
      (pink1 "#e0115f")
      (yellow1 "#fada5e"))

  (custom-theme-set-faces
   'darquiche-colorized

   `(default ((,class (:background ,bg1 :foreground ,fg1))))

   `(region ((,class (:background ,blue2))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg2))))
   `(cursor ((,class (:background ,fg1))))
   `(warning ((,class (:foreground ,orange1))))
   `(vertical-border ((,class (:foreground ,bg2))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,blue1))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,blue1 :underline t))))
   `(hl-line ((,class (:background  ,bg2))))
   `(show-paren-match-face ((,class (:background ,blue1))))

   `(font-lock-builtin-face ((,class (:foreground ,fg1))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg1))))
   `(font-lock-reference-face ((,class (:foreground ,fg1))))
   `(font-lock-constant-face ((,class (:foreground ,fg1))))
   `(font-lock-function-name-face ((,class (:foreground ,blue1))))
   `(font-lock-keyword-face ((,class (:bold t :foreground ,fg1)))) ;; bold?
   `(font-lock-string-face ((,class (:foreground ,yellow1))))
   `(font-lock-type-face ((,class (:foreground ,fg1))))
   `(font-lock-variable-name-face ((,class (:foreground ,pink1))))
   `(font-lock-warning-face ((,class (:foreground ,fg0 :bold t))))
   `(font-lock-comment-face ((,class (:foreground ,fg3))))
   `(font-lock-doc-face ((,class (:foreground ,fg2))))

   `(js2-object-property ((,class (:foreground ,fg1))))
   `(js2-function-call ((,class (:inherit font-lock-function-name-face))))
   `(js2-function-param ((,class (:foreground ,fg1))))
   `(js2-external-variable ((,class (:inherit font-lock-variable-name-face))))
   `(js2-jsdoc-tag ((,class (:inherit font-lock-doc-face))))
   `(js2-jsdoc-type ((,class (:inherit font-lock-doc-face))))
   `(js2-jsdoc-value ((,class (:inherit font-lock-doc-face))))

   `(ido-first-match ((,class (:foreground ,fg1 :bold t))))
   `(ido-only-match ((,class (:foreground ,fg1 :bold t))))
   `(ido-subdir ((,class (:foreground ,fg1 :bold t))))

   `(isearch ((,class (:bold t :background ,blue1))))
   `(isearch-fail ((,class (:bold t :background ,red1))))

   `(ffap ((,class (:foreground ,fg1))))

   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,blue1))))
   `(company-tooltip ((,class (:foreground ,fg1 :background ,bg2))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg1))))
   `(company-tooltop-annotation ((,class (:foreground ,fg2))))
   `(company-tooltip-common ((,class (:foreground ,blue1))))
   `(company-tooltip-common-selection ((,class (:foreground ,blue1 :bold t))))
   `(company-tooltip-mouse ((,class (:background ,bg3))))

   `(mode-line ((,class :box (:line-width 5 :color ,bg2) :foreground ,fg1 :background ,bg2)))
   `(mode-line-inactive ((,class :box (:line-width 5 :color ,bg2) :foreground ,fg2 :background ,bg2)))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,blue1 :background nil))))
   `(mode-line-highlight ((,class (:foreground ,blue1 :bold t))))

   `(doom-modeline-buffer-modified ((,class (:foreground ,red1))))

   `(which-func ((,class :foreground ,blue1)))

   `(dired-directory ((,class (:foreground ,blue1))))
   `(dired-warning ((,class (:foreground ,orange1))))
   `(dired-header ((,class (:bold t))))
   `(dired-ignored ((,class (:foreground ,fg2))))
   `(dired-mark ((,class (:foreground ,orange1))))
   `(dired-marked ((,class (:foreground ,orange1))))
   `(dired-flagged ((,class (:foreground ,orange1))))
   `(dired-perm-write ((,class (:foreground ,fg1))))
   `(dired-set-id ((,class (:foreground ,fg1))))
   `(dired-special ((,class (:foreground ,fg1))))
   `(dired-symlink ((,class (:foreground ,fg1))))

   `(treemacs-directory-face ((,class (:foreground ,blue1))))
   `(treemacs-directory-collapsed-face ((,class (:foreground ,blue1))))
   `(treemacs-root-face ((,class (:foreground ,blue1 :height 1.1 :underline nil))))

   `(markdown-blockquote-face ((,class :background ,bg2 :extend t)))
   `(markdown-code-face ((,class :background ,bg2 :extend t)))
   `(markdown-inline-code-face ((,class :background ,bg2)))
   `(markdown-pre-face ((,class :background ,bg2 :extend t)))
   `(markdown-hr-face ((,class :foreground ,bg2)))
   `(markdown-link-face ((,class :foreground ,blue1 :underline t)))

   `(imenu-list-entry-face-0 ((,class :foreground ,fg0)))
   `(imenu-list-entry-face-1 ((,class :foreground ,fg1)))
   `(imenu-list-entry-face-2 ((,class :foreground ,fg2)))
   `(imenu-list-entry-face-3 ((,class :foreground ,fg3)))
   `(imenu-list-entry-subalist-face-0 ((,class :foreground ,blue1 :underline t :bold t)))
   `(imenu-list-entry-subalist-face-1 ((,class :foreground ,fg0 :underline t :bold t)))
   `(imenu-list-entry-subalist-face-2 ((,class :foreground ,fg1 :underline t :bold t)))
   `(imenu-list-entry-subalist-face-3 ((,class :foreground ,fg2 :underline t :bold t)))

   `(elfeed-search-date-face ((,class :foreground ,blue1)))
   `(elfeed-search-feed-face ((,class :foreground ,fg1)))
   `(elfeed-search-tag-face ((,class :foreground ,fg2)))

   `(which-key-key-face ((,class :foreground ,blue1)))

   `(fill-column-indicator ((,class :foreground ,bg2)))

   `(bm-face ((,class :foreground ,bg2 :background ,orange1 :extend t)))

   `(ag-hit-face ((,class :foreground ,blue1)))

   `(whitespace-trailing ((,class :foreground nil :background ,red1)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'darquiche-colorized)

;;; darquiche-colorized-theme.el ends here
