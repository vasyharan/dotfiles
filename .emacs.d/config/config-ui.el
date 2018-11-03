;; config-ui.el -- My Emacs evil settings.
;;; Commentary:
;;; Code:

(use-package linum-relative
  :ensure t
  :delight
  :commands (linum-relative-mode)
  :hook (prog-mode . linum-relative-mode)
  :init
  (setq linum-relative-current-symbol ""
	linum-relative-format "%4s\u2502"
	linum-relative-backend 'display-line-numbers-mode))

(use-package highlight-indent-guides
  :ensure t
  :commands (highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-auto-enabled (display-graphic-p)
	highlight-indent-guides-auto-character-face-perc 10))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :config
  (setq fci-rule-character-color "#aaaaaa"
	fci-rule-character 9474))

(use-package rainbow-delimiters
  :ensure t
  :delight
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :delight
  :commands (rainbow-mode)
  :hook (prog-mode . rainbow-mode))

(use-package solarized-theme
  :ensure t)

(use-package my-solarized-theme
  :after (solarized-theme)
  :load-path "lisp/my-solarized"
  :init
  (setq solarized-distinct-fringe-background nil
	solarized-high-contrast-mode-line nil
	solarized-use-less-bold t
	solarized-use-more-italic t
	solarized-emphasize-indicators nil
	solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil
	x-underline-at-descent-line t)
  :config
  ;; (load-theme 'my-solarized-dark)
  )

;; (defun gruvbox-dark ()
;;   (interactive)
;;   (load-theme 'gruvbox-dark-hard)
;;   (set-face-attribute 'mode-line-directory nil :foreground "#a89984")
;;   (set-face-attribute 'mode-line-flycheck-separator nil :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-flycheck-error nil :foreground "#fb4933")
;;   (set-face-attribute 'mode-line-flycheck-warning nil :foreground "#fabd2f")
;;   (set-face-attribute 'mode-line-flycheck-info nil :foreground "#b8bb26")
;;   (set-face-attribute 'mode-line-evil-normal nil :background "#665c54" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-motion nil :background "#665c54" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-insert nil :background "#458588" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-emacs nil :background "#d3869b" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-visual nil :background "#fe8019" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-operator nil :background "#fb4934" :foreground "#d5c4a1")
;;   (set-face-attribute 'mode-line-evil-replace nil :background "#fb4934" :foreground "#d5c4a1"))

;; (defun gruvbox-light ()
;;   (interactive)
;;   (load-theme 'gruvbox-light-soft)
;;   (set-face-attribute 'mode-line-directory nil :foreground "#7c6f64")
;;   (set-face-attribute 'mode-line-flycheck-separator nil :foreground "#504945")
;;   (set-face-attribute 'mode-line-flycheck-error nil :foreground "#9d0006")
;;   (set-face-attribute 'mode-line-flycheck-warning nil :foreground "#b57614")
;;   (set-face-attribute 'mode-line-flycheck-info nil :foreground "#79740e")
;;   (set-face-attribute 'mode-line-evil-normal nil :background "#bdae93" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-motion nil :background "#bdae93" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-insert nil :background "#458588" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-emacs nil :background "#8f3f71" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-visual nil :background "#af3a03" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-operator nil :background "#9d0006" :foreground "#504945")
;;   (set-face-attribute 'mode-line-evil-replace nil :background "#9d0006" :foreground "#504945"))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard)
  (set-face-attribute 'mode-line-directory nil :foreground "#a89984")
  (set-face-attribute 'mode-line-flycheck-separator nil :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-flycheck-error nil :foreground "#fb4933")
  (set-face-attribute 'mode-line-flycheck-warning nil :foreground "#fabd2f")
  (set-face-attribute 'mode-line-flycheck-info nil :foreground "#b8bb26")
  (set-face-attribute 'mode-line-evil-normal nil :background "#665c54" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-motion nil :background "#665c54" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-insert nil :background "#458588" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-emacs nil :background "#d3869b" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-visual nil :background "#fe8019" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-operator nil :background "#fb4934" :foreground "#d5c4a1")
  (set-face-attribute 'mode-line-evil-replace nil :background "#fb4934" :foreground "#d5c4a1"))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t
	show-paren-highlight-openparen nil)
  (defun show-paren--locate-near-paren ()
    "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
    (let* ((ind-pos (save-excursion (back-to-indentation) (point)))
	   (eol-pos
	    (save-excursion
	      (end-of-line) (skip-chars-backward " \t" ind-pos) (point)))
	   (before (show-paren--categorize-paren (1- (point))))
	   (after (show-paren--categorize-paren (point))))
      ;; (message "%s %s" before after)
      (cond
       ;; Point is immediately outside a paren.
       ((eq (car after) -1) after)
       ((eq (car before) -1) before)
       ((eq (car after) 1) after)
       ;; Point is immediately inside a paren.
       ((and show-paren-when-point-inside-paren before))
       ((and show-paren-when-point-inside-paren after))
       ;; Point is in the whitespace before the code.
       ((and show-paren-when-point-in-periphery
	     (<= (point) ind-pos))
	(or (show-paren--categorize-paren ind-pos)
	    (show-paren--categorize-paren (1- eol-pos))))
       ;; Point is in the whitespace after the code.
       ((and show-paren-when-point-in-periphery
	     (>= (point) eol-pos))
	(show-paren--categorize-paren (1- eol-pos)))))))

(size-indication-mode)
(column-number-mode)

(setq display-time-default-load-average nil
      display-time-24hr-format t)

(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :weight 'normal
		    :width 'normal)

(defvar face-height-factor 22
  "Factor for calculating face height.")

(defun adjust-face-height (&optional frame)
  "Guess a font height for FRAME, which defaults to selected FRAME."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (let* ((monitor-attributes (frame-monitor-attributes (selected-frame)))
	 (pixel-height (nth 4 (assq 'geometry monitor-attributes)))
	 (mm-height (nth 2 (assq 'mm-size monitor-attributes)))
	 (face-height (* face-height-factor (ceiling (/ (float pixel-height) mm-height)))))
    (set-face-attribute 'default frame :height face-height)))

;; no menu bar for tty or linux
(if (and (display-graphic-p) (eq system-type "darwin"))
    (add-to-list 'initial-frame-alist '(menu-bar-lines . 1))
  (add-to-list 'initial-frame-alist '(menu-bar-lines . 0)))

(setq default-frame-alist (copy-alist initial-frame-alist))

(defun tty-ui-frame-hook ()
  "UI adjustments hook for TTY frame."
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
  (evil-terminal-cursor-changer-activate))

(defun gui-ui-frame-hook ()
  "UI adjustments hook for GUI frame."
  (if (eq system-type "darwin")
      (set-frame-parameter (selected-frame) 'menu-bar-lines 1))
  (adjust-face-height))

(defun gui-ui-init ()
  "Initialize GUI frame UI settings."
  (adjust-face-height))

(if (display-graphic-p) (gui-ui-init))

(add-hook 'tty-setup-hook 'tty-ui-frame-hook)

(use-package after-frame-hooks
  :load-path "lisp/after-frame-hooks"
  :init
  (add-hook 'after-make-tty-frame-hook 'tty-ui-frame-hook)
  (add-hook 'after-make-gui-frame-hook 'gui-ui-frame-hook))

(provide 'config-ui)
;;; config-ui.el ends here
