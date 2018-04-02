;;; config-code-completion.el -- Emacs config for code completion.
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :delight
  :commands (company-mode)
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.5
	company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-complete)

  (defun on-off-fci-before-company(command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))

  (advice-add 'company-call-frontends :before #'on-off-fci-before-company))

(use-package yasnippet
  :ensure t
  :delight (yas-minor-mode "" "yasnippet")
  :commands (yas-minor-mode)
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(setq tags-revert-without-query t
      large-file-warning-threshold nil)
(add-to-list 'safe-local-variable-values
	     '(counsel-etags-tags-program . "ctags --languages=ruby -e -L"))

(provide 'config-code-completion)
;; config-code-completion.el ends here
