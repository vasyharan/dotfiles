;;; private/tmux/autoloads.el -*- lexical-binding: t; -*-

(defun tmux/windmove (windmove-func tmux-direction)
  (condition-case nil
      (funcall windmove-func)
    (error
     ;; (when (featurep! :feature evil)
     ;;   (unless (evil-emacs-state-p)
     ;;     (evil-force-normal-state)))
     (shell-command-to-string
      (concat "tmux if-shell -F \"#{window_zoomed_flag}\" '' 'select-pane " tmux-direction "'")))))

;;;###autoload
(defun tmux/windmove-left ()
  (interactive)
  (tmux/windmove 'windmove-left "-L"))

;;;###autoload
(defun tmux/windmove-right ()
  (interactive)
  (tmux/windmove 'windmove-right "-R"))

;;;###autoload
(defun tmux/windmove-up ()
  (interactive)
  (tmux/windmove 'windmove-up "-U"))

;;;###autoload
(defun tmux/windmove-down ()
  (interactive)
  (tmux/windmove 'windmove-down "-D"))
