(require 'load-relative)

(setq frame-resize-pixelwise t
      initial-frame-alist '((horizontal-scroll-bars . nil)
			    (vertical-scroll-bars . nil)))

(defun frame-magnet (width-ratio height-ratio &optional pin-x pin-y)
  "Resize frame to WIDTH-RATIO and HEIGHT-RATIO of display.
Optionally pin the frame to the display by setting:
PIN-X to one of 'left or 'left, and
PIN-Y to one of 'top or 'bottom"
  (let* ((frame-width (floor (* (display-pixel-width) width-ratio)))
	 (frame-height (floor (* (display-pixel-height) height-ratio)))
	 (frame-x (cond
		   ((eq pin-x 'left) 0)
		   ((eq pin-x 'right) -1)
		   (t nil)))
	 (frame-y (cond
		   ((eq pin-x 'top) 0)
		   ((eq pin-x 'bottom) -1)
		   (t nil)))
	 (fullscreen (frame-parameter nil 'fullscreen)))

    (set-frame-size nil frame-width frame-height t)
    (if (numberp frame-x)
	(set-frame-parameter nil 'left frame-x))
    (if (numberp frame-y)
	(set-frame-parameter nil 'top frame-y))
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
 (kbd "s-C-h") (lambda () (interactive) (frame-magnet 0.66 1 'left))
 (kbd "s-C-l") (lambda () (interactive) (frame-magnet 0.66 1 'right))
 (kbd "s-<return>") 'toggle-frame-maximized)

(provide-me "config-")
