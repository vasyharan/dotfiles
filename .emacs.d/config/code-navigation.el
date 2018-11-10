(require 'use-package)
(require 'load-relative)

(use-package xref
  :after (evil utils)
  :defines (evil-motion-state-map)
  :config
  (define-keys evil-motion-state-map
    (kbd "C-]") 'xref-find-definitions
    (kbd "M-]") 'xref-find-definitions-other-window
    (kbd "C-T") 'pop-tag-mark)

  (evil-define-key 'normal xref--xref-buffer-mode-map
    (kbd "j") 'xref-next-line
    (kbd "k") 'xref-prev-line
    (kbd "J") 'evil-next-line
    (kbd "K") 'evil-prev-line
    (kbd "C-n") 'evil-next-line
    (kbd "C-p") 'evil-prev-line
    (kbd "o") 'xref-show-location-at-point
    (kbd "q") 'quit-window
    (kbd "RET") 'xref-goto-xref))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
