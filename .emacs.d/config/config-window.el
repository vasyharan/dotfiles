;; config-window.el -- Emacs window settings.
;;; Commentary:
;;; Code:

(defun tmux-windmove (windmove-func tmux-direction)
  (interactive)
  (condition-case nil
      (funcall windmove-func)
    (error (shell-command-to-string
	    (concat "tmux select-pane " tmux-direction)))))

(defun tmux-windmove-left ()
  (interactive)
  (tmux-windmove 'windmove-left "-L"))
(defun tmux-windmove-down ()
  (interactive)
  (tmux-windmove 'windmove-down "-D"))
(defun tmux-windmove-up ()
  (interactive)
  (tmux-windmove 'windmove-up "-U"))
(defun tmux-windmove-right ()
  (interactive)
  (tmux-windmove 'windmove-right "-R"))

(after 'evil
  (evil-define-key '(motion insert) 'global
    (kbd "M-h") 'tmux-windmove-left
    (kbd "M-j") 'tmux-windmove-down
    (kbd "M-k") 'tmux-windmove-up
    (kbd "M-l") 'tmux-windmove-right))

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :custom-face
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

  (setq aw-keys   '(?H ?J ?K ?L)
	aw-dispatch-always t
	aw-dispatch-alist
	'((?h windmove-left)
	  (?j windmove-down)
	  (?k windmove-up)
	  (?l windmove-right)
	  (?c evil-window-delete)
	  (?C aw-delete-window			"Delete window")
	  (?m aw-swap-window			"Swap window")
	  (?M aw-move-window			"Move window")
	  (?n aw-flip-window			"Flip window")
	  (?_ evil-window-split)
	  (?| evil-window-vsplit)
	  (?s split-window-sensibly		"Split")
	  (?o delete-other-windows)
	  (?O delete-other-windows)
	  (?= balance-windows			"Balance")
	  (?w hydra-window-size/body)
	  (? evil-window-mru)
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
	  windmove-up
	  tmux-windmove-left
	  tmux-windmove-right
	  tmux-windmove-down
	  tmux-windmove-up)
	golden-ratio-exclude-modes
	'(flycheck-error-list-mode)
	golden-ratio-exclude-buffer-names
	'(" *Org tags*" " *Org todo*" "*Flycheck Errors*")
	golden-ratio-exclude-buffer-regexp
	'("\\`\\*Flycheck errors\\*\\'")))

(winner-mode)
(golden-ratio-mode)

(use-package shackle
  :ensure t
  :init
  (shackle-mode)
  :config
  (setq shackle-rules
	'(("\\`\\*Flycheck errors\\*\\'" :regexp t :align 'below :size 0.3))))

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
