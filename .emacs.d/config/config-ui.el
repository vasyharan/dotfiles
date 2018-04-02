;; config-ui.el -- My Emacs evil settings.
;;; Commentary:
;;; Code:

(use-package linum-relative
  :ensure t
  :delight
  :commands (linum-relative-mode)
  :init
  (add-hook 'prog-mode-hook 'linum-relative-mode)
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
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun def-powerline-faces ()
  "Define powerline evil faces."
  (unless (default-boundp 'powerline-evil-base-face)
    (defface powerline-evil-base-face
      '((t (:foreground "white" :inherit mode-line)))
      "Base face for powerline evil faces."
      :group 'powerline)
    (dolist (s '((powerline-evil-normal-face "green" "Evil normal state face.")
		 (powerline-evil-insert-face "blue" "Evil insert state face.")
		 (powerline-evil-emacs-face "violet" "Evil emacs state face.")
		 (powerline-evil-replace-face "red" "Evil replace state face.")
		 (powerline-evil-visual-face "orange" "Evil visual state face.")
		 (powerline-evil-motion-face "magenta" "Evil motion state face.")

		 (powerline-evil-operator-face "cyan" "Evil motion state face.")))
      (eval `(defface ,(nth 0 s)
	       `((t (:background ,(nth 1 s)
				 :inherit 'powerline-evil-base-face)))
	       ,(nth 2 s)
	       :group 'powerline)))))

(use-package golden-ratio
  :ensure t
  :delight
  :commands (golden-ratio-mode)
  :config
  (setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(ace-window
		evil-window-left
                evil-window-right
                evil-window-up
                evil-window-down))))

(defun config-spaceline()
  "Configure spaceline."

  (require 'spaceline-segments)
  (require 'spaceline-config)

  (def-powerline-faces)
  (add-hook 'focus-in-hook 'force-mode-line-update)
  (setq spaceline-evil-state-faces
    '((normal . powerline-evil-normal-face)
      (insert . powerline-evil-insert-face)
      (emacs . powerline-evil-emacs-face)
      (replace . powerline-evil-replace-face)
      (visual . powerline-evil-visual-face)
      (motion . powerline-evil-motion-face)))
  (setq spaceline-highlight-face-func
	'spaceline-highlight-face-evil-state
	spaceline-minor-modes-separator " ")

  (spaceline-define-segment ruby-rbenv
    "The current ruby version. Works with `rbenv'."
    (when (and active
	       (eq 'ruby-mode major-mode)
	       (fboundp 'rbenv--active-ruby-version)
	       (rbenv--active-ruby-version))
      (let ((name (rbenv--active-ruby-version)))
	(propertize name
		    'help-echo "Ruby version (via rbenv)"))))

  (spaceline-define-segment aw-path
    "The window key."
    (progn
      (aw-update)
      (window-parameter (selected-window) 'ace-window-path)))

  (spaceline-define-segment selection-info
    "Information about the size of the current selection, when applicable.
    Supports both Emacs and Evil cursor conventions."
    (when (or mark-active
	      (and (bound-and-true-p evil-local-mode)
		   (eq 'visual evil-state)))
      (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
	     (chars (- (1+ (region-end)) (region-beginning)))
	     (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
			       (spaceline--column-number-at-pos (region-beginning))))))
	     (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
	     (rect (or (bound-and-true-p rectangle-mark-mode)
		       (and evil (eq 'block evil-visual-selection))))
	     (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
	(cond
	 (rect (format "%d×%d" lines (if evil cols (1- cols))))
	 (multi-line (format "%dL" lines))
	 (t (format "%dc" (if evil chars (1- chars))))))))

  (spaceline-compile
    `((((projectile-root (buffer-id buffer-modified) :separator "") remote-host)
       :face highlight-face)
      ((major-mode (ruby-rbenv :when active))
       :priority 0)
      ((org-clock) :when active :priority 0)
      ((minor-modes)
       :when active
       :priority 1)
      ((version-control)
       :when active
       :priority 2))
    `(
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 0)
      ((selection-info)
       :when active
       :priority 3)
      ((global)
       :when active
       :priority 4)
      ((line-column)
       :face highlight-face
       :when active)
      (aw-path)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline
  :ensure t
  :demand t
  :config
  ;; (config-spaceline)
  )

(defun shorten-vc-mode-line (string)
  "Shortens mode-line STRING for command `vc-mode'."
  (cond
   ((string-prefix-p "Git-" string) (concat "g/" (substring string 4)))
   ((string-prefix-p "Git:" string) (concat "g:" (substring string 4)))
   (t string)))
(advice-add 'vc-git-mode-line-string :filter-return 'shorten-vc-mode-line)
(after 'autorevert (diminish 'auto-revert-mode "ar"))

(use-package dracula-theme
  :ensure t
  :defer t)

;; (setq ns-use-srgb-colorspace t)
;; (setq powerline-image-apple-rgb t)

(defun get-default-face-height ()
  "Guess a nice default font height."
  (let ((display-width (display-pixel-width)))
    (cond ((> display-width 1440) 130)
	  (t 120))))

(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height (get-default-face-height)
		    :weight 'normal
		    :width 'normal)

(load-theme 'dracula)
(setq frame-resize-pixelwise t)
(setq initial-frame-alist
	'((top . 0)
	  (left . 0)
	  (horizontal-scroll-bars . nil)
	  (vertical-scroll-bars . nil)
	  (menu-bar-lines . 1)))
(setq default-frame-alist
      (copy-alist initial-frame-alist))

(defvar after-make-tty-frame-hook '()
  "Hooks to run after creating a new TTY frame.")
(defvar after-make-gui-frame-hook '()
  "Hooks to run after creating a new GUI frame.")

(defun init-frame (frame)
  "Initialize newly created FRAME."
  (with-selected-frame frame
    (if (display-graphic-p)
	(run-hooks 'after-make-gui-frame-hook)
      (run-hooks 'after-make-tty-frame-hook))))

(add-hook 'after-make-frame-functions 'init-frame)
(add-hook 'after-init-hook (lambda () (init-frame (selected-frame))))

;; (run-hook-with-args
;;      (if (display-graphic-p)
;; 	 'after-make-gui-frame-hook
;;        'after-make-tty-frame-hook)
;;      (selected-frame))
;; (if (display-graphic-p)
;;     (init-gui-look-and-feel (selected-frame))
;;   (init-term-look-and-feel (selected-frame)))

(defun init-tty-frame ()
  "Initial TTY FRAME."
  (set-frame-parameter (selected-frame) 'menu-bar-lines 0))
(add-hook 'after-make-tty-frame-hook 'init-tty-frame)

(provide 'config-ui)
;;; config-ui.el ends here
