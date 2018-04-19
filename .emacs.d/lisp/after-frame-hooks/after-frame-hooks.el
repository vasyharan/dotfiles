(defvar after-make-tty-frame-hook '()
  "Hooks to run after creating a new TTY frame.")
(defvar after-make-gui-frame-hook '()
  "Hooks to run after creating a new GUI frame.")

(defun after-frame-hooks-handler (frame)
  "Run appropriate GUI or TTY after frame hooks for newly created FRAME."
  (with-selected-frame frame
    (if (display-graphic-p frame)
	(run-hooks 'after-make-gui-frame-hook)
      (run-hooks 'after-make-tty-frame-hook))))

(add-hook 'after-make-frame-functions 'after-frame-hooks-handler)
(add-hook 'emacs-startup-hook (lambda () (after-frame-hooks-handler (selected-frame))))

(provide 'after-frame-hooks)
