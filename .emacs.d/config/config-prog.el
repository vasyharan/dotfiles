;; config-prod.el -- Emacs prog mode.
;;; Commentary:
;;; Code:

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package lsp-mode
  :ensure t
  :commands (lsp-ruby-enable))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode))

(defun default-prog-hook()
  "Default `prog-mode' hook."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'default-prog-hook)

(provide 'config-prog)
;;; config-prog.el ends here
