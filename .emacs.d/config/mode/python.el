(require 'load-relative)
(require 'use-package)

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python")

(use-package pyenv-mode
  :commands (pyenv-mode))

;; (use-package company-jedi
;;   :ensure t
;;   :commands (company-jedi))
(use-package lsp-python
  :commands (lsp-python-enable))

(defun config-python-mode ()
  "Custom settings for Python."
  ;; (setq-local fci-rule-column 80)
  ;; (add-to-list 'company-backends 'company-jedi)

  (pyenv-mode)
  (subword-mode +1)
  (electric-pair-mode +1)
  (electric-indent-mode +1))

(add-hook 'python-mode-hook 'config-python-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
