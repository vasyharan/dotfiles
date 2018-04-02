;;; config-pay-mode.el -- My Emacs pay-mode settings.
;;; Commentary:
;;; Code:

(use-package pay-mode
  :load-path "lisp/pay-mode/")

(after 'evil-leader
  (evil-leader/set-key
    "cn"	'pay-test-name
    "cl"	'pay-test-point
    "cf"	'pay-test-file
    "cc"	'pay-test-redo
    "cyn"	'pay-kill-test-name
    "cyl"	'pay-kill-test-point
    "cyf"	'pay-kill-test-file
    "cyr"	'pay-kill-test-redo))

(provide 'config-pay-mode)
;;; config-pay-mode.el ends here
