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

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/dracula-theme")
(load-theme 'dracula)

;; (use-package dracula-theme
;;   :load-path "lisp/dracula-theme/"
;;   :init
;;   (load-theme 'dracula))

(show-paren-mode)
(column-number-mode)

(setq display-time-default-load-average nil
      display-time-24hr-format t)

(set-face-attribute 'default nil
		      :family "Source Code Pro"
		      :weight 'normal
		      :width 'normal)

;; (setq ns-use-srgb-colorspace t)
;; (setq powerline-image-apple-rgb t)

(defun adjust-face-height ()
  "Guess a nice default font height."
  (interactive)
  (let* ((display-width (display-pixel-width))
	 (face-height (cond ((> display-width 1440) 150)
			    (t 140))))
    (set-face-attribute 'default (selected-frame)
			:height face-height)))

(defun tty-ui-frame-hook ()
  "UI adjustments hook for TTY frame."
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0))

(defun gui-ui-frame-hook ()
  "UI adjustments hook for GUI frame."
  (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
  (adjust-face-height)
  (adjust-frame-size))

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
