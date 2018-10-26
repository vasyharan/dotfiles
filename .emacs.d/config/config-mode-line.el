;; config-mode-line.el -- Mode-line config.
;;; Commentary:
;;; Code:

(defface mode-line-directory
  '((t :foreground "#586e75"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-separator
  '((t :foreground "#586e75"))
  "Face used for flycheck separators."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-error
  '((t :foreground "#ff6e64"))
  "Face used for number of flycheck errors."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-warning
  '((t :foreground "#f2804f"))
  "Face used for number of flycheck warnings."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-info
  '((t :foreground "#69b7f0"))
  "Face used for number of flycheck infos."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-evil-normal
  '((t :foreground "#b4c342"))
  "Face used for evil normal state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-emacs
  '((t :foreground "#deb542"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-insert
  '((t :foreground "#69cabf"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-motion
  '((t :foreground "#9ea0e5"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-visual
  '((t :foreground "#f2804f"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-operator
  '((t :foreground "#f771ac"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-evil-replace
  '((t :foreground "#ff6e64"))
  "Face used for evil emacs state."
  :group 'mode-line-faces
  :group 'basic-faces)

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat ".../" output)))
       output))

(defvar mode-line-directory
  '(" "
    (:propertize
     (:eval (if (buffer-file-name) (shorten-directory default-directory 20)))
     face mode-line-directory))
  "Mode line construct for displaying the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(setq-default mode-line-modified
	      '((:propertize
		 "%1+"
		 help-echo mode-line-modified-help-echo
		 local-map (purecopy (make-mode-line-mouse-map
				      'mouse-1 #'mode-line-toggle-modified))
		 mouse-face mode-line-highlight)))

(defvar mode-line-editable
  '(" "
    (:propertize
     (:eval (if buffer-read-only "ro" ""))
     help-echo mode-line-read-only-help-echo
     local-map (purecopy (make-mode-line-mouse-map
			  'mouse-1 #'mode-line-toggle-read-only))
     mouse-face mode-line-highlight))
  "Mode line construct for displaying whether current buffer is editable.")
(put 'mode-line-editable 'risky-local-variable t)

(use-package evil
  :init

  (setq evil-normal-state-tag	(propertize " No " 'face 'mode-line-evil-normal)
	evil-emacs-state-tag	(propertize " Em " 'face 'mode-line-evil-emacs)
	evil-insert-state-tag	(propertize " In " 'face 'mode-line-evil-insert)
	evil-motion-state-tag	(propertize " Mo " 'face 'mode-line-evil-motion)
	evil-visual-state-tag	(propertize " Vi " 'face 'mode-line-evil-visual)
	evil-operator-state-tag	(propertize " Op " 'face 'mode-line-evil-operator)
	evil-replace-state-tag	(propertize " Re " 'face 'mode-line-evil-replace)
	evil-mode-line-format '(after . mode-line-front-space)))

(setq mode-line-front-space '(:eval (if (display-graphic-p) " " "")))

(defvar mode-line-selected-window (frame-selected-window)
  "The selected window for setting mode-line.")
(defun mode-line-set-selected-window ()
  "Set the variable `mode-lien-selected-window` appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq mode-line-selected-window (frame-selected-window))
    (force-mode-line-update)))
(defun mode-line-unset-selected-window ()
  "Unset the variable `mode-line-selected-window` and update the mode-line."
  (setq mode-line-selected-window nil)
  (force-mode-line-update))
(defun mode-line-selected-window-active ()
  "Return whether the current window is active."
  (eq mode-line-selected-window (selected-window)))
(add-hook 'window-configuration-change-hook 'mode-line-set-selected-window)
(add-hook 'focus-in-hook 'mode-line-set-selected-window)
(add-hook 'focus-out-hook 'mode-line-unset-selected-window)
(defadvice handle-switch-frame
    (after mode-line-set-selected-window-after-switch-frame activate)
  "Make mode-line aware of frame change."
  (mode-line-set-selected-window))
(defadvice select-window (after mode-line-select-window activate)
  "Make mode-line aware of window change."
  (mode-line-set-selected-window))

(after 'flycheck
  (defun flycheck-mode-line-status-text (&optional status)
    "Get a text describing STATUS for use in the mode line.

    STATUS defaults to `flycheck-last-status-change' if omitted or
    nil."
    (let ((status-text
	   (pcase (or status flycheck-last-status-change)
	     (`not-checked "-")
	     (`no-checker "-")
	     (`running "*")
	     (`errored "!")
	     (`finished
	      (let-alist (flycheck-count-errors flycheck-current-errors)
		(if (or .error .warning .info)
		    (concat
		     (propertize ":" 'face 'mode-line-flycheck-separator)
		     (propertize (format "%s" (or .error 0)) 'face 'mode-line-flycheck-error)
		     (propertize "/" 'face 'mode-line-flycheck-separator)
		     (propertize (format "%s" (or .warning 0)) 'face 'mode-line-flycheck-warning)
		     (propertize "/" 'face 'mode-line-flycheck-separator)
		     (propertize (format "%s" (or .info 0)) 'face 'mode-line-flycheck-info))
		  "")))
	     (`interrupted ".")
	     (`suspicious "?"))))
      (concat flycheck-mode-line-prefix status-text))))

(setq mode-line-position
      '(
	(-4 " %p")
	(size-indication-mode ("/" (-4 "%I")))
	(line-number-mode (" %l" (column-number-mode ":%c")))
	))

(setq-default mode-line-format
	      '(:eval
		(let* ((active (mode-line-selected-window-active)))
		  (if active
		      (list "%e"
			    'mode-line-front-space
			    'evil-mode-line-tag
			    ;; 'mode-line-editable
			    'mode-line-directory
			    'mode-line-buffer-identification
			    'mode-line-modified
			    " "
			    'mode-line-modes
			    '(flycheck-mode flycheck-mode-line)
			    'mode-line-position
			    'mode-line-misc-info)
		    (list "%e"
			  'mode-line-directory
			  'mode-line-buffer-identification
			  'mode-line-modified)))))


(provide 'config-mode-line)
;;; config-mode-line.el ends here
