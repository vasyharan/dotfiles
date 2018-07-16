;;; config-syntax-checking.el -- Emacs config for syntax checking.
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :delight
  :hook (prog-mode . flycheck-mode)
  :config
  (add-hook 'flycheck-error-list-mode-hook (lambda () (setq mode-line-format nil))))

;; (use-package flymake
;;   :commands (flymake-mode)
;;   :hook (prog-mode . flymake-mode))

(after 'evil-leader (evil-leader/set-key "e" 'flycheck-list-errors))

(provide 'config-syntax-checking)
;;; config-syntax-checking.el ends here
