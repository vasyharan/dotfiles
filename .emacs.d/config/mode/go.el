(require 'load-relative)
(require 'use-package)

(use-package go-mode
  :mode "\\.go\\'"
  :interpreter ("go" . go-mode)
  :config
  (setq gofmt-command "goimports"))

(use-package gotest
  :commands (go-run
	     go-test-current-test
	     go-test-current-file
	     go-test-current-project
	     go-test-current-coverage
	     go-test-current-benchmark
	     go-test-current-file-benchmarks
	     go-test-current-project-benchmarks))

(defun config-go-mode()
  "Configure Go mode."
  (setq tab-width 2
	indent-tabs-mode t)
  (add-hook 'before-save-hook 'gofmt-before-save nil t))

(add-hook 'go-mode-hook 'config-go-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
