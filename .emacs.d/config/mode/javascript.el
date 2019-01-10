(require 'use-package)
(require 'load-relative)

;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'")

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package add-node-modules-path
  :commands (add-node-modules-path))

(use-package prettier-js
  :commands (prettier-js prettier-js-mode))

(use-package nodenv
  :commands (nodenv-mode))

(defun config-js-mode()
  "Configure Javascript mode."
  (setq tab-width 2
	js-indent-level 2
	indent-tabs-mode nil
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil)
  (after 'evil
    (setq evil-shift-width 2))
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers '(javascript-jshint)))

  (add-node-modules-path)
  (prettier-js-mode)

  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (after 'lsp-ui-flycheck
    (flycheck-add-next-checker 'lsp-ui 'javascript-eslint))
  )

(defun config-json-mode()
  "Configure JSON mode."
  (prettier-js-mode))

(add-hook 'json-mode-hook 'config-json-mode)
(add-hook 'rjsx-mode-hook 'config-js-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
