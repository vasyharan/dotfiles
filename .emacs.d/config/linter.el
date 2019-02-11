(require 'use-package)
(require 'load-relative)

(use-package flycheck
  :commands (flycheck-mode)
  :delight
  :hook (prog-mode . flycheck-mode)
  :config
  (add-hook 'flycheck-error-list-mode-hook
	    (lambda ()
	      (setq mode-line-format
		    '(:eval
		      (let* ((active (mode-line-selected-window-active)))
			(if active
			    (list "%e"
				  'mode-line-front-space
				  'evil-mode-line-tag
				  ;; 'mode-line-editable
				  'mode-line-directory
				  'mode-line-buffer-identification
				  'mode-line-modified
				  " "
				  'mode-line-modes) ""))))))

  (defun flycheck-list-errors-toggle ()
    "Toggle flycheck error list window."
    (interactive)
    (let* ((flycheck-buffer (get-buffer flycheck-error-list-buffer))
	   (flycheck-window (get-buffer-window flycheck-buffer)))
      (if (and flycheck-window flycheck-buffer)
	  (delete-window flycheck-window) (flycheck-list-errors-focus))))
  (defun flycheck-list-errors-focus ()
    "Show the flycheck error list for the current buffer and focus it."
    (interactive)
    (flycheck-list-errors)
    (select-window (get-buffer-window (get-buffer flycheck-error-list-buffer))))
  (after 'evil-leader (evil-leader/set-key "ee" 'flycheck-list-errors-focus)))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
