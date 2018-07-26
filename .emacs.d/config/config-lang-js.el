;;; config-lang.el -- My Emacs js config.
;;; Commentary:
;;; Code:

;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'")
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

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
  "Configure Javascript mode."
  (setq tab-width 2
	evil-shift-width 2
	js-indent-level 2
	indent-tabs-mode nil
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil)
  (set (make-local-variable 'company-backends) '(company-flow))
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers '(javascript-jshint)))

  ;; (add-node-modules-path)
  (prettier-js-mode)

  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
  ;; (eglot-ensure)
  (lsp-javascript-flow-enable))

(defun config-json-mode()
  "Configure JSON mode."
  (prettier-js-mode))

(add-hook 'json-mode-hook 'config-json-mode)
(add-hook 'rjsx-mode-hook 'config-js-mode)

(provide 'config-lang-js)
;;; config-lang-js.el ends here
