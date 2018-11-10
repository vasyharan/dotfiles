(require 'use-package)
(require 'load-relative)

;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'")

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package company-flow
  :commands (company-flow))

(use-package flycheck-flow
  :defer t)

(use-package add-node-modules-path
  :commands (add-node-modules-path))

(use-package prettier-js
  :commands (prettier-js prettier-js-mode))

(use-package nodenv
  :commands (nodenv-mode))

(use-package lsp-javascript-flow
  :commands (lsp-javascript-flow-enable)
  :config
  (setq lsp-javascript-flow-server-args '("--no-auto-download"
					  "--try-flow-bin")))
(use-package lsp-javascript-typescript
  :commands (lsp-javascript-typescript-enable))

(use-package lsp-typescript
  :commands (lsp-typescript-enable))

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

  (prettier-js-mode)

  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
  (lsp-javascript-flow-enable))

(defun config-json-mode()
  "Configure JSON mode."
  (prettier-js-mode))

(add-hook 'json-mode-hook 'config-json-mode)
(add-hook 'rjsx-mode-hook 'config-js-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
