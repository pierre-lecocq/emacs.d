;;
;; Look and feel
;;

(setq inhibit-startup-message t)
(setq initial-scratch-message "* Perkeleen Vittupää")
(global-font-lock-mode t)
(transient-mark-mode t)
;; (set-face-background 'highlight "#333")
;; (set-face-foreground 'highlight nil)
(line-number-mode t)
(column-number-mode t)
(display-time)
(set-frame-font "Monospace 10")

;;
;; X mode
;;

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(setq my-available-themes '(deeper-blue wombat dichromacy))
(defun my-theme-switch ()
  "Switch between themes"
  (interactive)
  (setq my-next-theme (pop my-available-themes))
  (setq my-available-themes (append my-available-themes (list my-next-theme)))
  (message "Switch to theme %s" my-next-theme)
  (load-theme my-next-theme t)
)

(defun xMode ()
  (add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))
  (global-linum-mode t)
  (global-hl-line-mode t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (my-theme-switch)
  (transparency 95))

(if window-system (xMode))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;
;; Org-mode
;;

;; Strike done tasks
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                             :weight normal
                             :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "Grey55" :strike-through t)))))
