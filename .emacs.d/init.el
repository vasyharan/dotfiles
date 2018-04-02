;; init.el -- Emacs initialization file.
;;; Commentary:
;;; Code:

;; (package-initialize)

(load-file (concat (file-name-directory load-file-name)
		   "config/config-bootstrap.el"))

(require 'config-base)

(require 'config-evil)
(require 'config-ui)
(require 'config-vcs)
;; (require 'config-org)
(require 'config-completion)
(require 'config-code-completion)
(require 'config-syntax-checking)

(require 'config-lang-go)
(require 'config-lang-js)
(require 'config-lang-python)
(require 'config-lang-ruby)
(require 'config-lang-rust)
(require 'config-lang-scala)
(require 'config-lang-thrift)

(require 'pay-mode)

(provide 'init)
;;; init.el ends here
