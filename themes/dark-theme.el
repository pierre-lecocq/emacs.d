;;; dark-theme.el --- Darky theme -*- lexical-binding: t; -*-

;; Time-stamp: <2019-11-19 10:54:12>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(deftheme dark "Dark theme.")

(let ((class '((class color) (min-colors 89)))
      (default-bg               "#1A1A1A")
      (default-fg               "#FAFAFA")
      (region-bg                "#1E90FF") ;; DodgerBlue
      (comment-fg               "#6A6A6A")
      (hl-line-bg               "#2A2A2A")
      (hl-indent-line-bg        "#3A3A3A")
      (string-fg                "#FF6666")
      (mode-line-bg             "#2A2A2A")
      (mode-line-fg             "#EAEAEA")
      (mode-line-inactive-bg    "#222222")
      (mode-line-inactive-fg    "#8A8A8A")
      (vertical-border-bg       "#4A4A4A")
      (vertical-border-fg       "#4A4A4A")
      (git-gutter-modified-fg   "#FFFF00"))
  (custom-theme-set-faces
   'dark
   `(default ((,class (:background ,default-bg :foreground ,default-fg))))
   `(region ((,class (:background ,region-bg))))
   `(hl-line ((,class (:background ,hl-line-bg :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,comment-fg))))
   `(font-lock-string-face ((,class (:foreground ,string-fg))))
   `(mode-line ((,class (:background ,mode-line-bg :foreground ,mode-line-fg :box (:line-width 4 :color ,mode-line-bg :style nil)))))
   `(mode-line-inactive ((,class (:background ,mode-line-inactive-bg :foreground ,mode-line-inactive-fg :box (:line-width 4 :color ,mode-line-inactive-bg :style nil)))))
   `(vertical-border ((,class (:background ,vertical-border-bg :foreground ,vertical-border-fg))))
   `(fringe ((,class (:background ,default-bg :foreground ,default-fg))))
   `(anzu-mode-line ((,class (:foreground ,string-fg))))
   `(which-func ((,class (:background ,mode-line-bg :foreground ,mode-line-fg))))
   `(ido-subdir ((t (:inherit (dired-directory)))))
   `(ido-only-match ((t (:foreground ,default-fg :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,git-gutter-modified-fg))))
   `(all-the-icons-dired-dir-face ((,class (:foreground nil))))
   `(highlight-indentation-face ((,class (:background ,hl-indent-line-bg))))
   `(highlight-indentation-current-column-face ((,class (:background ,hl-indent-line-bg))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dark)

;;; dark-theme.el ends here
