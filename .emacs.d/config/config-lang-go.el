(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package company-go
  :ensure t
  :commands (company-go))

(use-package go-rename
  :ensure t
  :commands (go-rename))

(defun config-go-mode ()
  "Custom settings for Golang."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode t
	company-backends '(company-go))
  (evil-define-key 'motion go-mode-map
    (kbd "C-]") 'godef-jump
    (kbd "M-]") 'godef-jump-other-window))

(add-hook 'go-mode-hook 'config-go-mode)

(provide 'config-lang-go)
