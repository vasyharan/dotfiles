(require 'use-package)
(require 'load-relative)

;; (use-package eglot
;;   :commands (eglot eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 	       `(ruby-mode . ("pay" "exec" "scripts/bin/typecheck" "--lsp")))
;;   (add-to-list 'eglot-server-programs
;; 	       `((js-mode js2-mode rjsx-mode) .
;; 		 ("flow-language-server" "--stdio" "--no-auto-download" "--try-flow-bin"))))

(use-package lsp-mode
  :commands (lsp-ruby-enable)
  :config
  (defconst lsp-ruby--get-root
    (lsp-make-traverser
     #'(lambda (dir)
	 (directory-files dir nil "\\(Rakefile\\|Gemfile\\)"))))
  (lsp-define-stdio-client
   lsp-ruby "ruby"
   lsp-ruby--get-root
   '("pay" "exec" "scripts/bin/typecheck" "--lsp")))

;; (use-package cquery
;;   :commands (lsp-cquery-enable))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
