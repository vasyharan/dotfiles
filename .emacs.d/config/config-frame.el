;; config-frame.el -- Frame configuration.
;;; Commentary:
;;; Code:

(setq frame-resize-pixelwise t
      initial-frame-alist '((horizontal-scroll-bars . nil)
			    (vertical-scroll-bars . nil)))
;; no menu bar for tty
(if (display-graphic-p)
    (add-to-list 'initial-frame-alist '(menu-bar-lines . 1))
  (add-to-list 'initial-frame-alist '(menu-bar-lines . 0)))

(setq default-frame-alist (copy-alist initial-frame-alist))

(defun frame-magnet (width-ratio height-ratio &optional pin-x pin-y)
  (let* ((frame-width (floor (* (display-pixel-width) width-ratio)))
	 (frame-height (floor (* (display-pixel-height) height-ratio)))
	 (frame-x (cond
		   ((eq pin-x 'left) 0)
		   ((eq pin-x 'right) -1)
		   (t 0)))
	 (frame-y (cond
		   ((eq pin-x 'top) 0)
		   ((eq pin-x 'bottom) -1)
		   (t 0)))
	 (fullscreen (frame-parameter nil 'fullscreen)))

    (set-frame-size nil frame-width frame-height t)
    (set-frame-parameter nil 'left frame-x)
    (set-frame-parameter nil 'top frame-y)
    (if fullscreen
      (set-frame-parameter nil 'fullscreen nil))))


(defun adjust-frame-size ()
  "Adjust frame size on creation based on display."
  (cond ((> (display-pixel-width) 1440) (frame-magnet 0.66 1 'left))
	(t (set-frame-parameter nil 'fullscreen 'maximized))))

(defun gui-frame-init ()
  "Initialize GUI frame settings."
  (adjust-frame-size))

(if (display-graphic-p) (gui-frame-init))

(global-set-keys
 (kbd "H-C-h") (lambda () (interactive) (frame-magnet 0.66 1 'left))
 (kbd "H-C-l") (lambda () (interactive) (frame-magnet 0.66 1 'right))
 (kbd "H-<return>") 'toggle-frame-maximized)

(provide 'config-frame)
;;; config-frame.el ends here
