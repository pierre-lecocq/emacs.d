;;; lightish-theme.el --- Lightish theme -*- lexical-binding: t; -*-

;; Time-stamp: <2020-12-21 13:48:58>
;; Copyright (C) 2020 Pierre Lecocq

;;; Commentary:

;;; Code:

(deftheme lightish "Lightish theme.")

(let ((class '((class color) (min-colors 89)))
      (fg0 "#1a1a1a")
      (fg1 "#2a2a2a")
      (fg2 "#808080")
      (fg3 "#565656")
      (bg1 "#fafafa")
      (bg2 "#eaeaea")
      (bg3 "#dadada")
      (blue1 "#1e90ff")
      (blue2 "#275ea2")
      (orange1 "#ffa500")
      (red1 "#cc0000"))

  (custom-theme-set-faces
   'lightish

   `(default ((,class (:background ,bg1 :foreground ,fg1))))

   `(region ((,class (:background ,blue2 :foreground ,bg1))))
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
   `(font-lock-comment-face ((,class (:foreground ,fg3))))
   `(font-lock-doc-face ((,class (:foreground ,fg2))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg1))))
   `(font-lock-reference-face ((,class (:foreground ,fg1))))
   `(font-lock-constant-face ((,class (:foreground ,fg1))))
   `(font-lock-function-name-face ((,class (:foreground ,fg0))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,fg1)))) ;; bold?
   `(font-lock-string-face ((,class (:foreground ,fg0))))
   `(font-lock-type-face ((,class (:foreground ,fg1))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg1))))
   `(font-lock-warning-face ((,class (:foreground ,fg0 :bold t))))

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

   `(js2-jsdoc-tag ((,class (:foreground ,fg2))))
   `(js2-jsdoc-type ((,class (:foreground ,fg2))))
   `(js2-jsdoc-value ((,class (:foreground ,fg2))))
   `(js2-function-param ((,class (:foreground ,fg1))))
   `(js2-external-variable ((,class (:foreground ,fg1))))

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

   `(fill-column-indicator ((,class :foreground ,bg2)))

   `(bm-face ((,class :background ,orange1 :extend t)))

   `(ag-hit-face ((,class :foreground ,blue1)))

   `(whitespace-trailing ((,class :foreground nil :background ,red1)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lightish)

;;; lightish-theme.el ends here
