(require 'load-relative)
(require 'use-package)

(use-package thrift
  :mode ("\\.thrift\\'" . thrift-mode))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
