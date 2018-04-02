;;; config-evil.el -- My Emacs evil settings.
;;; Commentary:
;;; Code:

(use-package evil-leader
  :ensure t
  :config
  (setq evil-leader/no-prefix-mode-rx '())

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>"	'execute-extended-command
    ";"		'toggle-comment-region-or-line
    ":"		'eval-expression
    "="		'align-regexp))

(use-package evil
  :ensure t
  :config
  (global-evil-leader-mode)

  ;; center after search
  (advice-add 'evil-search-next :after
	      (lambda (&rest _) (evil-scroll-line-to-center nil)))
  (advice-add 'evil-search-previous :after
	      (lambda (&rest _) (evil-scroll-line-to-center nil)))

  ;; code jumps
  (define-keys evil-motion-state-map
    (kbd "C-]") 'xref-find-definitions
    (kbd "M-]") 'xref-find-definitions-other-window
    (kbd "C-T") 'pop-tag-mark)

  (defun global-unset-keys (key &rest keys)
    "See `global-unset-key` for KEY; KEYS are more keys."
    (declare (indent defun))
    (while key
      (global-unset-key key)
      (setq key (pop keys))))

  ;; unbind C-w
  (define-key evil-motion-state-map (kbd "C-w") 'ace-window)
  (global-set-key (kbd "C-w") 'ace-window)
  (global-unset-keys (kbd "C-j")
		     (kbd "C-k"))

  ;; term kets
  (evil-define-key 'normal term-raw-map
    (kbd "p") 'term-paste
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down)

  (evil-define-key 'insert term-raw-map
    (kbd "C-c C-d") 'term-send-eof
    (kbd "C-c C-z") 'term-stop-subjob
    (kbd "<tab>") 'term-send-tab
    (kbd "C-k") 't
    (kbd "C-j") 'term-send-down)

  ;; better key bindings for xref buffer
  (evil-define-key 'normal xref--xref-buffer-mode-map
    (kbd "j") 'xref-next-line
    (kbd "k") 'xref-prev-line
    (kbd "J") 'evil-next-line
    (kbd "K") 'evil-prev-line
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-prev-line
    (kbd "o") 'xref-show-location-at-point
    (kbd "q") 'quit-window
    (kbd "RET") 'xref-goto-xref)

  (evil-mode 1))

(provide 'config-evil)
;;; config-evil.el ends here
