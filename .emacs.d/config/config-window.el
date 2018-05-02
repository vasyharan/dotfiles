;; config-window.el -- Emacs window settings.
;;; Commentary:
;;; Code:

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :custom-face
  (aw-leading-char-face ((t (:foreground "aquamarine1" :weight bold))))
  ;; (aw-mode-line-face ((t (:foreground "SlateGray3"))))
  :init
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
	  (?_ evil-window-split			"Split Vertically")
	  (?| evil-window-vsplit		"Split Horizontally")
	  (?s split-window-sensibly		"Split")
	  (?o delete-other-windows)
	  (?O delete-other-windows		"Maximize window")
	  (?= balance-windows)
	  (?w hydra-window-size/body)
	  (?  hydra-window-scroll/body)
	  (?b aw-switch-buffer-in-window	"Select buffer")
	  (?u hydra-window-winner/winner-undo)
	  (?U hydra-window-winner/winner-redo))))

(use-package golden-ratio
  :ensure t
  :delight
  :commands (golden-ratio-mode)
  :config
  (setq golden-ratio-extra-commands
	'(ace-window
	  magit-status
	  magit-show-commit
	  magit-process-buffer
	  evil-window-left
	  evil-window-right
	  evil-window-up
	  evil-window-down
	  windmove-left
	  windmove-right
	  windmove-down
	  windmove-up)))

(winner-mode)
(golden-ratio-mode)

;; prefer horizontal splits.
(defun split-window-sensibly (&optional window)
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
;; (setq split-width-threshold 160
;;       split-window-preferred-function 'split-window-sensibly)


(provide 'config-window)
;;; config-window.el ends here
