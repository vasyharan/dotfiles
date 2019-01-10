(require 'use-package)
(require 'load-relative)

(use-package lsp-mode
  :commands (lsp)
  :init
  (setq lsp-prefer-flymake nil)
  :config
  (defun lsp-sorbet--ls-command ()
    "Generate the language server startup command."
    '("pay" "exec" "scripts/bin/typecheck" "--lsp" "-v"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-sorbet--ls-command)
		    :major-modes '(ruby-mode)
		    :priority -1
		    :server-id 'sorbet-ls))

  (defun lsp-flow--ls-command ()
    "Generate the language server startup command."
    (let ((lsp-clients-flow-server "flow-language-server")
	  (lsp-clients-flow-server-args '("--stdio")))
	  `(,lsp-clients-flow-server
	    ,@lsp-clients-flow-server-args)))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-flow--ls-command)
		    :major-modes '(typescript-mode js-mode js2-mode rjsx-mode)
		    :priority -1
		    :server-id 'flow-ls)))

(use-package lsp-ui
  :after (flycheck)
  :commands (lsp-ui-mode)
  ;; :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :commands (company-lsp))

;; (use-package sorbet-lsp
;;   :load-path "elisp"
;;   :after (lsp-mode)
;;   :commands (sorbet-lsp-restart))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
