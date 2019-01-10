(require 'load-relative)

(defun tmux-windmove (windmove-func tmux-direction)
  (condition-case nil
      (funcall windmove-func)
    (error
     (evil-force-normal-state)
     (shell-command-to-string
      (concat "tmux if-shell -F \"#{window_zoomed_flag}\" '' 'select-pane " tmux-direction "'")))))

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

(provide-me)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
