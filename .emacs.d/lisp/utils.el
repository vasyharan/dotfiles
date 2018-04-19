;; utils.el -- Utility functions.
;;; Commentary:
;;; Code:

(defun toggle-comment-region-or-line()
  "Toggle comment for region if selected, or line if not."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun define-keys (keymap key def &rest bindings)
  "See `define-key` for KEYMAP, KEY, DEF; BINDINGS are more KEY/DEF pairs."
  ;; (declare (indent defun))
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defun global-set-keys (key def &rest bindings)
  "See `global-set-key' for KEY, DEF; BINDINGS are more KEY/DEF pairs."
  ;; (declare (indent defun))
  (while key
    (global-set-key key def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defun global-unset-keys (key &rest keys)
  "See `global-unset-key` for KEY; KEYS are more keys."
  ;; (declare (indent defun))
  (while key
    (global-unset-key key)
    (setq key (pop keys))))

(defconst user-config-directory
  (expand-file-name (concat user-emacs-directory "config/"))
  "Storage area for persistent files.")

(defun load-config-file (file)
  "Load FILE from user's config directory."
  (load-file (expand-file-name (concat user-config-directory file))))

(provide 'utils)
;;; utils.el ends here
