(require 'use-package)
(require 'load-relative)

(use-package pay-server
  :load-path "elisp"
  :after (flycheck inf-ruby yasnippet)
  :commands (pay-enable-appropriate-mode)
  :config
  (after 'evil-leader
    (evil-leader/set-key-for-mode 'ruby-mode
      "cc"	'pay-test-reverify
      "cf"	'pay-test-verify-current-buffer
      "cl"	'pay-test-verify-current-line
      "cn"	'pay-test-verify-current-test
      "cyy"	'pay-test-kill-reverify
      "cyn"	'pay-test-kill-verify-current-test
      "cyl"	'pay-test-kill-verify-current-line
      "cyf"	'pay-test-kill-verify-current-buffer
      "cb"	'pay-kill-break-current-line
      "ca"      'pay-test-mark-current-test
      "cA"      'pay-test-unmark-current-test
      "cm"	'pay-test-verify-marked
      "cM"      'pay-test-clear-marks)
    (evil-leader/set-key-for-mode 'pay-compilation-mode
      "cL"	'pay-find-test-log)))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
