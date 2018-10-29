;; config-prod.el -- Emacs prog mode.
;;; Commentary:
;;; Code:

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

(use-package hl-line
  :commands (hl-line-mode)
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(use-package flyspell
  :commands (flyspell-prog-mode)
  :hook (prog-mode . flyspell-prog-mode))

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs `(ruby-mode . ("pay" "exec" "scripts/bin/typecheck" "--lsp")))
						     ;; "--debug-log-file=/tmp/rtdbg")))
  (add-to-list 'eglot-server-programs `((js-mode js2-mode rjsx-mode) . ("flow-language-server" "--stdio" "--no-auto-download" "--try-flow-bin"))))

(use-package lsp-mode
  :ensure t
  :commands (lsp-ruby-enable)
  :config
  (defconst lsp-ruby--get-root
    (lsp-make-traverser
     #'(lambda (dir)
	 (directory-files dir nil "\\(Rakefile\\|Gemfile\\)"))))
  (lsp-define-stdio-client
   lsp-ruby "ruby"
   lsp-ruby--get-root
   '("pay" "exec" "scripts/bin/typecheck" "--lsp")
     ;; "--debug-log-file=/tmp/rtdbg")
   ))

(use-package lsp-javascript-flow
  :ensure t
  :commands (lsp-javascript-flow-enable)
  :config
  (setq lsp-javascript-flow-server-args '("--no-auto-download"
					  "--try-flow-bin")))
(use-package lsp-javascript-typescript
  :ensure t
  :commands (lsp-javascript-typescript-enable))

(use-package lsp-typescript
  :ensure t
  :commands (lsp-typescript-enable))

(use-package cquery
  :ensure t
  :commands (lsp-cquery-enable))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(defun default-prog-hook()
  "Default `prog-mode' hook."
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'default-prog-hook)

(provide 'config-prog)
;;; config-prog.el ends here
