;;; light-theme.el --- Light theme -*- lexical-binding: t; -*-

;; Time-stamp: <2019-09-03 11:22:09>
;; Copyright (C) 2019 Pierre Lecocq

;;; Commentary:

;;; Code:

(deftheme light "Light theme.")

(let ((class '((class color) (min-colors 89)))
      (default-bg               "#FFFFFF")
      (default-fg               "#2A2A2A")
      (region-bg                "#87CEFF") ;; SkyBlue1
      (comment-fg               "#6A6A6A")
      (hl-line-bg               "#F2F2F2")
      (string-fg                "#DD6666")
      (mode-line-bg             "#F2F2F2")
      (mode-line-fg             "#4A4A4A")
      (mode-line-inactive-bg    "#E2E2E2")
      (mode-line-inactive-fg    "#6A6A6A")
      (vertical-border-bg       "#FFFFFF")
      (vertical-border-fg       "#E2E2E2")
      (git-gutter-added-fg      "#66DD66")
      (git-gutter-modified-fg   "#DDDD66")
      (git-gutter-deleted-fg    "#DD6666"))
  (custom-theme-set-faces
   'light
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
   `(git-gutter:added ((,class (:foreground ,git-gutter-added-fg))))
   `(git-gutter:modified ((,class (:foreground ,git-gutter-modified-fg))))
   `(git-gutter:deleted ((,class (:foreground ,git-gutter-deleted-fg))))
   `(all-the-icons-dired-dir-face ((,class (:foreground nil))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'light)

;;; light-theme.el ends here
