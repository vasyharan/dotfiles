(require 'load-relative)
(require 'use-package)

(use-package tmux-windmove
  :load-path "elisp"
  :after (evil utils evil-leader)
  :commands (tmux-windmove-left tmux-windmove-right tmux-windmove-up tmux-windmove-down)
  :init
  (evil-leader/set-key
    "h" 'help-command)
  (evil-define-key '(motion insert) 'global
    (kbd "C-h") 'tmux-windmove-left
    (kbd "C-j") 'tmux-windmove-down
    (kbd "C-k") 'tmux-windmove-up
    (kbd "C-l") 'tmux-windmove-right)
  (global-set-keys
   (kbd "C-h") 'tmux-windmove-left
   (kbd "C-j") 'tmux-windmove-down
   (kbd "C-k") 'tmux-windmove-up
   (kbd "C-l") 'tmux-windmove-right))

(use-package winner
  :commands (winner-undo winner-redo)
  :init (winner-mode))

(use-package ace-window
  ;; :after (evil hydra winner)
  :commands (ace-window)
  :init
  (evil-define-key '(motion insert) 'global
    (kbd "C-w") 'ace-window)
  (global-set-key
   (kbd "C-w") 'ace-window)

  :config
  (require 'hydra)
  (defhydra hydra-window-size (:color red)
    "Window size"
    ("h" shrink-window-horizontally "shrink ↔")
    ("j" shrink-window "shrink ↕")
    ("k" enlarge-window "enlarge ↕")
    ("l" enlarge-window-horizontally "enlarge ↔"))

  (defhydra hydra-window-scroll (:color red)
    "Scroll other window"
    ("k" scroll-other-window-down "scroll up")
    ("j" scroll-other-window "scroll down"))

  (defhydra hydra-window-winner (:color red)
    "Winner mode"
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
  	  (?U hydra-window-winner/winner-redo)))
  )

;; (use-package golden-ratio
;;   :delight
;;   :commands (golden-ratio-mode)
;;   :defines (golden-ratio-extra-commands
;; 	    golden-ratio-exclude-modes
;; 	    golden-ratio-exclude-buffer-names
;; 	    golden-ratio-exclude-buffer-regexp)
;;   :init
;;   (golden-ratio-mode)
;;   :config
;;   (setq golden-ratio-extra-commands
;; 	'(ace-window
;; 	  magit-status
;; 	  magit-show-commit
;; 	  magit-process-buffer
;; 	  evil-window-left
;; 	  evil-window-right
;; 	  evil-window-up
;; 	  evil-window-down
;; 	  windmove-left
;; 	  windmove-right
;; 	  windmove-down
;; 	  windmove-up
;; 	  tmux-windmove-left
;; 	  tmux-windmove-right
;; 	  tmux-windmove-down
;; 	  tmux-windmove-up
;; 	  quit-window)
;; 	golden-ratio-exclude-modes
;; 	'(flycheck-error-list-mode)
;; 	golden-ratio-exclude-buffer-names
;; 	'(" *Org tags*" " *Org todo*" "*Flycheck Errors*")
;; 	golden-ratio-exclude-buffer-regexp
;; 	'("\\`\\*Flycheck errors\\*\\'")))

(use-package shackle
  :commands (shackle-mode)
  :init (shackle-mode)
  :defines (shackle-rules)
  :config
  (setq shackle-rules
	'(("\\`\\*Flycheck errors\\*\\'" :regexp t :align 'below :size 0.3))))

(defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
	(and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it vertically disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
	 (not (window-minibuffer-p window))
	 (let ((split-height-threshold 0))
	   (when (window-splittable-p window)
	     (with-selected-window window
	       (split-window-below))))))))

(setq split-height-threshold 60
      split-width-threshold 160)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
