;;; config-pay-mode.el -- My Emacs pay-mode settings.
;;; Commentary:
;;; Code:

(use-package pay-server
  :load-path "lisp/pay-server"
  :init
  (add-hook 'ruby-mode 'pay-enable-appropriate-mode)
  ;; :delight pay-mode pay-test-mode
  :config
  (after 'evil-leader
    (evil-leader/set-key-for-mode 'pay-mode
      "cc"	'pay-test-reverify
      "cf"	'pay-test-verify-current-buffer
      "cl"	'pay-test-verify-current-line
      "cn"	'pay-test-verify-current-test
      "cyy"	'pay-test-kill-reverify
      "cyn"	'pay-test-kill-verify-current-test
      "cyl"	'pay-test-kill-verify-current-line
      "cyf"	'pay-test-kill-verify-current-buffer
      "cb"	'pay-test-break-current-line
      "ca"      'pay-test-mark-current-test
      "cA"      'pay-test-unmark-current-test
      "cm"	'pay-test-verify-marked
      "cM"      'pay-test-clear-marks)))

(provide 'config-pay-mode)
;;; config-pay-mode.el ends here
