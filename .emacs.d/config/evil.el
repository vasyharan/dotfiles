(require 'load-relative)
(require 'use-package)

(use-package evil
  :init
  (setq evil-toggle-key "C-`"
	evil-search-module 'evil-search
	evil-default-cursor t)
  (use-package evil-leader
    :commands (global-evil-leader-mode evil-leader/set-leader evil-leader/set-key)
    :init (global-evil-leader-mode)
    :config
    (setq evil-leader/no-prefix-mode-rx '())
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "<SPC>"	'execute-extended-command
      ":"	'eval-expression
      "="	'align-regexp
      ";"	'comment-or-uncomment-line-or-region))
  (evil-mode)
  :config
  ;; center after search
  (advice-add 'evil-search-next :after
	      (lambda (&rest _) (evil-scroll-line-to-center nil)))
  (advice-add 'evil-search-previous :after
	      (lambda (&rest _) (evil-scroll-line-to-center nil))))

(use-package evil-terminal-cursor-changer
  :after (evil)
  :commands (etcc-on etcc-off)
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  )

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
