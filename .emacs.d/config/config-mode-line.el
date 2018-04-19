;; config-mode-line.el -- Mode-line config.
;;; Commentary:
;;; Code:

(defface mode-line-directory
  '((t :foreground "#565761"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-separator
  '((t :foreground "#565761"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-error
  '((t :foreground "#ff5555"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-warning
  '((t :foreground "#ffb86c"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)
(defface mode-line-flycheck-info
  '((t :foreground "#8be9fd"))
  "Face used for buffer identification parts of the mode line."
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
  '(:propertize
    (:eval (if (buffer-file-name) (shorten-directory default-directory 20)))
                face mode-line-directory)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(setq-default mode-line-modified
  (list (propertize
	 "%1+"
	 'help-echo 'mode-line-modified-help-echo
	 'local-map (purecopy (make-mode-line-mouse-map
			       'mouse-1 #'mode-line-toggle-modified))
	 'mouse-face 'mode-line-highlight)
	" "))

(defvar mode-line-editable
  (list
   '(:propertize
     (:eval (if buffer-read-only "ro" ""))
     help-echo mode-line-read-only-help-echo
     local-map (purecopy (make-mode-line-mouse-map
			  'mouse-1 #'mode-line-toggle-read-only))
     mouse-face mode-line-highlight)
   " ")
    "Mode line construct for displaying whether current buffer is editable.")
(put 'mode-line-editable 'risky-local-variable t)

(use-package evil
  :init
  (setq evil-normal-state-tag	(propertize "No " 'face '((:weight bold :foreground "#50fa7b")))
	evil-emacs-state-tag	(propertize "Em " 'face '((:weight bold :foreground "#f1fa8c")))
	evil-insert-state-tag	(propertize "In " 'face '((:weight bold :foreground "#8be9fd")))
	evil-motion-state-tag	(propertize "Mo " 'face '((:weight bold :foreground "#bd93f9")))
	evil-visual-state-tag	(propertize "Vi " 'face '((:weight bold :foreground "#ffb86c")))
	evil-operator-state-tag	(propertize "Op " 'face '((:weight bold :foreground "#ff79c6")))
	evil-replace-state-tag	(propertize "Re " 'face '((:weight bold :foreground "#ff5555")))
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

(use-package flycheck
  :config
  (defun flycheck-mode-line-status-text (&optional status)
    "Get a text describing STATUS for use in the mode line.

    STATUS defaults to `flycheck-last-status-change' if omitted or
    nil."
    (let ((status-text
	   (pcase (or status flycheck-last-status-change)
	     (`not-checked "")
	     (`no-checker "-")
	     (`running "*")
	     (`errored "!")
	     (`finished
	      (let-alist (flycheck-count-errors flycheck-current-errors)
		(if (or .error .warning .info)
		    (concat
		     (propertize (format "%s" (or .error 0)) 'face 'mode-line-flycheck-error)
		     (propertize "/" 'face 'mode-line-flycheck-separator)
		     (propertize (format "%s" (or .warning 0)) 'face 'mode-line-flycheck-warning)
		     (propertize "/" 'face 'mode-line-flycheck-separator)
		     (propertize (format "%s" (or .info 0)) 'face 'mode-line-flycheck-info))
		  "")))
	     (`interrupted ".")
	     (`suspicious "?"))))
      (if (string= status-text "") "" (concat status-text " ")))))

(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%c")))
	(-4 " %p")
	(size-indication-mode ("/" (-4 "%I")))))

(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun mode-line-format (values)
  "Render a list of mode line VALUES."
  ;; (mapconcat 'format-mode-line values "")
  (concat values)
  )

(defun mode-line-active-text ()
  "Text to render as the mode-line for the selected window."
  (let ((lhs (list 'mode-line-front-space
		   'evil-mode-line-tag
		   'mode-line-modified
		   " "
		   'mode-line-directory
		   'mode-line-buffer-identification
		   ;; 'mode-line-position
		   " "
		   '(flycheck-mode flycheck-mode-line)
		   " "
		   'mode-line-modes
		   'mode-line-misc-info))
	(rhs (list
	      'mode-line-position
	      " "
		   )))
    (mode-line-render (mode-line-format lhs) (mode-line-format rhs))))

(defun mode-line-inactive-text ()
  "Text to render as the mode-line for the selected window."
  (let ((lhs (list 'mode-line-front-space
		   'evil-mode-line-tag
		   'mode-line-modified
		   " "
		   'mode-line-directory))
	(rhs (list)))
    (concat (mapconcat 'format-mode-line lhs ""))))

(defun mode-line-text ()
  "Text to render as the mode-line."
  (if (mode-line-selected-window-active) (mode-line-active-text)
    (mode-line-inactive-text)))

;; (setq-default mode-line-format
;; 	      '(:eval (mode-line-text)))
(setq-default mode-line-format
	      '(:eval
		(let* ((active (mode-line-selected-window-active)))
		  (if active
		      (list "%e"
			    'mode-line-front-space
			    'evil-mode-line-tag
			    'mode-line-editable
			    'mode-line-directory
			    'mode-line-buffer-identification
			    'mode-line-modified
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
