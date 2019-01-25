(require 'use-package)
(require 'load-relative)

(use-package rjsx-mode
  :mode "\\.js\\'" "\\.tsx\\'")
(use-package typescript-mode
  :mode "\\.ts\\'")
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
	typescript-indent-level 2
	indent-tabs-mode nil
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil)
  (if (boundp 'evil-shift-width)
      (setq evil-shift-width 2))

  (add-node-modules-path)
  (prettier-js-mode))

(after 'flycheck
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  ;; (after 'lsp-ui-flycheck
  ;;   (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
  ;;   (flycheck-add-next-checker 'lsp-ui 'javascript-tslint))
  (if (boundp 'flycheck-disabled-checkers)
      (setq-default flycheck-disabled-checkers
		    (append flycheck-disabled-checkers '(javascript-jshint)))))

(defun config-json-mode()
  "Configure JSON mode."
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'json-mode-hook 'config-json-mode)
(add-hook 'rjsx-mode-hook 'config-js-mode)
(add-hook 'typescript-mode-hook 'config-js-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
