;;; config-lang.el -- My Emacs js config.
;;; Commentary:
;;; Code:

;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'")
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package company-flow
  :commands (company-flow)
  :ensure t)

(use-package flycheck-flow
  :ensure t)

(use-package add-node-modules-path
  :commands (add-node-modules-path)
  :ensure t)

(use-package prettier-js
  :commands (prettier-js prettier-js-mode)
  :ensure t)

(defun config-js-mode()
  ;; (load-file "~/.emacs.d/flow-for-emacs/flow.el")
  (setq tab-width 2
	evil-shift-width 2
	js-indent-level 2
	indent-tabs-mode nil
	company-backends '(company-flow)
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers '(javascript-jshint)))

  (add-node-modules-path)
  (prettier-js-mode)

  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(add-hook 'rjsx-mode-hook 'config-js-mode)

(provide 'config-lang-js)
;;; config-lang-js.el ends here
