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
  (load-theme 'my-solarized-dark))

(show-paren-mode)
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
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0))

(defun gui-ui-frame-hook ()
  "UI adjustments hook for GUI frame."
  (if (eq system-type "darwin")
      (set-frame-parameter (selected-frame) 'menu-bar-lines 1))
  (adjust-face-height))

(defun gui-ui-init ()
  "Initialize GUI frame UI settings."
  (adjust-face-height))

(if (display-graphic-p) (gui-ui-init))

(use-package after-frame-hooks
  :load-path "lisp/after-frame-hooks"
  :init
  (add-hook 'after-make-tty-frame-hook 'tty-ui-frame-hook)
  (add-hook 'after-make-gui-frame-hook 'gui-ui-frame-hook))

(provide 'config-ui)
;;; config-ui.el ends here
