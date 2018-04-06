;; config-mode-line.el -- Mode-line config.
;;; Commentary:
;;; Code:

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

(setq evil-normal-state-tag   (propertize "N> " 'face '((:foreground "#50fa7b")))
      evil-emacs-state-tag    (propertize "E> " 'face '((:foreground "#f1fa8c")))
      evil-insert-state-tag   (propertize "I> " 'face '((:foreground "#8be9fd")))
      evil-motion-state-tag   (propertize "M> " 'face '((:foreground "#bd93f9")))
      evil-visual-state-tag   (propertize "V> " 'face '((:foreground "#ffb86c")))
      evil-operator-state-tag (propertize "O> " 'face '((:foreground "#ff79c6"))))

(setq mode-line-front-space '(:eval (if (display-graphic-p) " " ""))
      evil-mode-line-format '(after . mode-line-front-space))

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-directory
		mode-line-buffer-identification
		mode-line-modified
		" "
		;; mode-line-position
		(flycheck-mode flycheck-mode-line)
		" "
		mode-line-modes
		mode-line-misc-info))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(provide 'config-mode-line)
;;; config-mode-line.el ends here
