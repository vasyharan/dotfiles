;; config-base.el -- My Emacs base settings.
;;; Commentary:
;;; Code:

  (setq user-emacs-directory
    (file-name-directory user-init-file))

(defconst user-cache-directory
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Storage area for persistent files.")
(defconst user-auto-save-directory
  (expand-file-name (concat user-cache-directory "auto-save/"))
  "Emacs auto save files directory.")
(defconst user-backup-directory
  (expand-file-name (concat user-cache-directory "backup/"))
  "Emacs backup files directory.")

(unless (file-exists-p user-cache-directory)
  (make-directory user-cache-directory))
(unless (file-exists-p user-auto-save-directory)
  (make-directory user-auto-save-directory))

(setq custom-file
  (expand-file-name "custom.el" user-cache-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq user-full-name "Vaseeharan Thirukumaran"
      user-mail-address "vasyharan@gmail.com")

(setq exec-path '("~/go/bin"
		  "~/.rbenv/shims"
		  "~/.pyenv/shims/"
		  "~/.cargo/bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"))

(setq inhibit-splash-screen t		;; empty screen on start
      initial-scratch-message nil
      ring-bell-function 'ignore	;; no bells or whistles
      visible-bell nil)

;; backups
(setq backup-directory-alist `((".*" . ,user-backup-directory))
      backup-by-copying t
      delete-by-moving-to-trash t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; auto-save
(setq auto-save-file-name-transforms
      `((".*" ,user-auto-save-directory t)))

;; save history
(setq savehist-file (expand-file-name "savehist" user-cache-directory)
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(savehist-mode)

(setq recentf-save-file
      (expand-file-name "recentf" user-cache-directory))

(setq display-time-default-load-average nil
      display-time-24hr-format t)
(show-paren-mode)
(column-number-mode)
(winner-mode)

(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))

  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(use-package delight
  :ensure t
  :demand t)

(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode))

(after 'evil-leader
  (evil-leader/set-key "u" 'undo-tree-visualize))

(use-package which-key
  :ensure t
  :delight
  :config (which-key-mode))

(use-package hydra
  :ensure t
  :config
  (set-face-attribute 'hydra-face-red nil :foreground "#ff5555")
  (set-face-attribute 'hydra-face-blue nil :foreground "#8be9fd")
  (set-face-attribute 'hydra-face-pink nil :foreground "#ff79c6")
  (setq hydra-lv t
	lv-use-separator nil))

(use-package ace-window
  :ensure t
  :defer 1
  :config
  (set-face-attribute 'aw-leading-char-face nil
		      :foreground "deep sky blue" :weight 'bold)
  (set-face-attribute 'aw-mode-line-face nil
		      :inherit 'mode-line-buffer-id :foreground "green")

  (defhydra hydra-window-size (:color red)
    "Windows size"
    ("h" shrink-window-horizontally "shrink ↔")
    ("j" shrink-window "shrink ↕")
    ("k" enlarge-window "enlarge ↕")
    ("l" enlarge-window-horizontally "enlarge ↔"))

  (defhydra hydra-window-scroll (:color red)
    "Scroll other window"
    ("k" scroll-other-window-down "scroll up")
    ("j" scroll-other-window "scroll down"))

  (defhydra hydra-window-winner (:color red)
    "Scroll other window"
    ("u" winner-undo "undo")
    ("U" winner-redo "redo"))

  (setq aw-keys   '(?j ?k ?l ?\;)
	aw-dispatch-always t
	aw-dispatch-alist
	'((?c evil-window-delete)
	  (?C aw-delete-window			"Delete window")
	  (?m aw-swap-window			"Swap window")
	  (?M aw-move-window			"Move window")
	  (?n aw-flip-window			"Flip window")
	  (?_ evil-window-split			"Split window")
	  (?| evil-window-vsplit		"Split Horizontally")
	  (?s aw-split-window-fair		"Split Vertically")
	  (?o delete-other-windows)
	  (?O delete-other-windows		"Maximize window")
	  (?= balance-windows)
	  (?w hydra-window-size/body)
	  (?  hydra-window-scroll/body)
	  (?b aw-switch-buffer-in-window	"Select buffer")
	  (?u hydra-window-winner/winner-undo)
	  (?U hydra-window-winner/winner-redo))))


(defun prog-hook()
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'prog-hook)

(defun toggle-comment-region-or-line()
  "Toggle comment for region if selected, or line if not."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun define-keys (keymap key def &rest bindings)
  "See `define-key` for KEYMAP, KEY, DEF; BINDINGS are more KEY/DEF pairs."
  (declare (indent defun))
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))
(defun term-send-up    ()
  "Send up arrow in term mode."
  (interactive) (term-send-raw-string "\eOA"))
(defun term-send-down  ()
  "Send down arrow in term mode."
  (interactive) (term-send-raw-string "\eOB"))
(defun term-send-right ()
  "Send right arrow in term mode."
  (interactive) (term-send-raw-string "\eOC"))
(defun term-send-left  ()
  "Send left arrow in term mode."
  (interactive) (term-send-raw-string "\eOD"))

;; prefer horizontal splits.
(setq split-window-preferred-function 'split-window-sensibly-prefer-horizontal)
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
This does the same thing as `split-window-sensibly', only trying
to split horizontally before vertically."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
	(and (eq window (frame-root-window (window-frame window)))
	     (not (window-minibuffer-p window))
	     ;; If WINDOW is the only window on its frame and is not the
	     ;; minibuffer window, try to split it vertically disregarding
	     ;; the value of `split-height-threshold'.
	     (let ((split-height-threshold 0))
	       (when (window-splittable-p window)
		 (with-selected-window window
		   (split-window-below))))))))

;; make command/options keys behave normally.
(setq mac-option-modifier 'meta
      mac-command-modifier 'hyper)

(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

(provide 'config-base)
;;; config-base.el ends here
