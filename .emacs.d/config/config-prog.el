;; config-prod.el -- Emacs prog mode.
;;; Commentary:
;;; Code:

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun default-prog-hook()
  "Default `prog-mode' hook."
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'default-prog-hook)

(provide 'config-prog)
;;; config-prog.el ends here
