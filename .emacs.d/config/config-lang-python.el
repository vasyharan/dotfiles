;;; config-lang-python.el -- Emacs config for python.
;;; Commentary:
;;; Code:


(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :interpreter "python")

(use-package pyenv-mode
  :ensure t
  :commands (pyenv-mode))

(use-package company-jedi
  :ensure t
  :commands (company-jedi))

(defun config-python-mode ()
  "Custom settings for Python."
  (setq-local fci-rule-column 80)

  (pyenv-mode)
  (subword-mode +1)
  (electric-pair-mode +1)
  (electric-indent-mode +1)

  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'config-python-mode)

(provide 'config-lang-python)
;;; config-lang-python.el ends here
