(require 'load-relative)
(require 'use-package)

(use-package linum-relative
  :delight
  :commands (linum-relative-mode)
  :hook (prog-mode . linum-relative-mode)
  :init
  (setq linum-relative-current-symbol ""
	linum-relative-format "%4s\u2502"
	linum-relative-backend 'display-line-numbers-mode))

(use-package highlight-indent-guides
  :commands (highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-auto-enabled (display-graphic-p)
	highlight-indent-guides-auto-character-face-perc 10))

(use-package fill-column-indicator
  :commands (fci-mode)
  :config
  (setq fci-rule-character-color "#aaaaaa"
	fci-rule-character 9474))

(use-package rainbow-delimiters
  :delight
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :delight
  :commands (rainbow-mode)
  :hook (prog-mode . rainbow-mode))

(use-package hl-line
  :commands (hl-line-mode)
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(use-package xclip
  :init
  (xclip-mode 1))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook #'prettify-symbols-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'(lambda () (setq show-trailing-whitespace t)))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium)
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
  :commands (show-paren--categorize-paren)
  :config
  (setq show-paren-style 'parenthesis
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t
	show-paren-highlight-openparen nil))

(size-indication-mode)
(column-number-mode)

(if (boundp 'display-time-default-load-average)
  (setq display-time-default-load-average nil))

(if (boundp 'display-time-24hr-format)
  (setq display-time-24hr-format t))

(set-face-attribute 'default nil
		    :family "InconsolataGo"
		    :weight 'normal
		    :width 'normal)

(defun adjust-face-height (&optional frame)
  "Guess a font height for FRAME, which defaults to selected FRAME."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (let* ((monitor-attributes (frame-monitor-attributes frame))
	 (pixel-width (nth 3 (assq 'geometry monitor-attributes)))
	 (face-height (cond ((= pixel-width 1440) 120)
			    (t 120))))
    (set-face-attribute 'default frame :height face-height)))

;; no menu bar for tty or linux
(add-to-list 'initial-frame-alist '(menu-bar-lines . 0))
(setq default-frame-alist (copy-alist initial-frame-alist))

(defun tty-ui-config()
  "UI adjustments hook for TTY frame."
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
  (etcc-on))

(defun gui-ui-config (&optional frame)
  "UI adjustments hook for GUI frame."
  (or frame (setq frame (selected-frame)))
  (if (eq system-type "darwin")
      (set-frame-parameter frame 'menu-bar-lines 1))
  (adjust-face-height frame))

(when (display-graphic-p) (gui-ui-config))
(add-hook 'after-make-frame-functions
	  #'(lambda (frame)
	      (when (display-graphic-p frame)
		(gui-ui-config frame))) t)

(add-hook 'tty-setup-hook #'tty-ui-config)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
