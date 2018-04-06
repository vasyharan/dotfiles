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

(use-package dracula-theme
  :load-path "lisp/dracula-theme"
  :ensure t
  :custom-face
  :init
  (load-theme 'dracula))

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
	 (face-height (cond ((> display-width 1440) 130)
			    (t 120))))
    (set-face-attribute 'default (selected-frame)
			:height face-height)))

(defun tty-ui-frame-hook ()
  "UI adjustments hook for TTY frame."
  )

(defun gui-ui-frame-hook ()
  "UI adjustments hook for GUI frame."
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

;; (defun def-powerline-faces ()
;;   "Define powerline evil faces."
;;   (unless (default-boundp 'powerline-evil-base-face)
;;     (defface powerline-evil-base-face
;;       '((t (:foreground "white" :inherit mode-line)))
;;       "Base face for powerline evil faces."
;;       :group 'powerline)
;;     (dolist (s '((powerline-evil-normal-face "green" "Evil normal state face.")
;; 		 (powerline-evil-insert-face "blue" "Evil insert state face.")
;; 		 (powerline-evil-emacs-face "violet" "Evil emacs state face.")
;; 		 (powerline-evil-replace-face "red" "Evil replace state face.")
;; 		 (powerline-evil-visual-face "orange" "Evil visual state face.")
;; 		 (powerline-evil-motion-face "magenta" "Evil motion state face.")

;; 		 (powerline-evil-operator-face "cyan" "Evil motion state face.")))
;;       (eval `(defface ,(nth 0 s)
;; 	       `((t (:background ,(nth 1 s)
;; 				 :inherit 'powerline-evil-base-face)))
;; 	       ,(nth 2 s)
;; 	       :group 'powerline)))))

;; (defun config-spaceline()
;;   "Configure spaceline."

;;   (require 'spaceline-segments)
;;   (require 'spaceline-config)

;;   (def-powerline-faces)
;;   (add-hook 'focus-in-hook 'force-mode-line-update)
;;   (setq spaceline-evil-state-faces
;;     '((normal . powerline-evil-normal-face)
;;       (insert . powerline-evil-insert-face)
;;       (emacs . powerline-evil-emacs-face)
;;       (replace . powerline-evil-replace-face)
;;       (visual . powerline-evil-visual-face)
;;       (motion . powerline-evil-motion-face)))
;;   (setq spaceline-highlight-face-func
;; 	'spaceline-highlight-face-evil-state
;; 	spaceline-minor-modes-separator " ")

;;   (spaceline-define-segment ruby-rbenv
;;     "The current ruby version. Works with `rbenv'."
;;     (when (and active
;; 	       (eq 'ruby-mode major-mode)
;; 	       (fboundp 'rbenv--active-ruby-version)
;; 	       (rbenv--active-ruby-version))
;;       (let ((name (rbenv--active-ruby-version)))
;; 	(propertize name
;; 		    'help-echo "Ruby version (via rbenv)"))))

;;   (spaceline-define-segment aw-path
;;     "The window key."
;;     (progn
;;       (aw-update)
;;       (window-parameter (selected-window) 'ace-window-path)))

;;   (spaceline-define-segment selection-info
;;     "Information about the size of the current selection, when applicable.
;;     Supports both Emacs and Evil cursor conventions."
;;     (when (or mark-active
;; 	      (and (bound-and-true-p evil-local-mode)
;; 		   (eq 'visual evil-state)))
;;       (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
;; 	     (chars (- (1+ (region-end)) (region-beginning)))
;; 	     (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
;; 			       (spaceline--column-number-at-pos (region-beginning))))))
;; 	     (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
;; 	     (rect (or (bound-and-true-p rectangle-mark-mode)
;; 		       (and evil (eq 'block evil-visual-selection))))
;; 	     (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
;; 	(cond
;; 	 (rect (format "%d×%d" lines (if evil cols (1- cols))))
;; 	 (multi-line (format "%dL" lines))
;; 	 (t (format "%dc" (if evil chars (1- chars))))))))

;;   (spaceline-compile
;;     `((((projectile-root (buffer-id buffer-modified) :separator "") remote-host)
;;        :face highlight-face)
;;       ((major-mode (ruby-rbenv :when active))
;;        :priority 0)
;;       ((org-clock) :when active :priority 0)
;;       ((minor-modes)
;;        :when active
;;        :priority 1)
;;       ((version-control)
;;        :when active
;;        :priority 2))
;;     `(
;;       ((flycheck-error flycheck-warning flycheck-info)
;;        :when active
;;        :priority 0)
;;       ((selection-info)
;;        :when active
;;        :priority 3)
;;       ((global)
;;        :when active
;;        :priority 4)
;;       ((line-column)
;;        :face highlight-face
;;        :when active)
;;       (aw-path)))

;;   (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

;; (use-package spaceline
;;   :ensure t
;;   :demand t
;;   :config
;;   ;; (config-spaceline)
;;   )


(provide 'config-ui)
;;; config-ui.el ends here
