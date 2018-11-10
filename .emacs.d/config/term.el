(require 'use-package)
(require 'load-relative)
(require 'utils)

(after 'evil
  (evil-define-key 'normal term-raw-map
    (kbd "p") 'term-paste
    (kbd "C-p") 'term-send-up
    (kbd "C-n") 'term-send-down)

  (evil-define-key 'insert term-raw-map
    (kbd "C-c C-d") 'term-send-eof
    (kbd "C-c C-z") 'term-stop-subjob
    (kbd "<tab>") 'term-send-tab
    (kbd "C-k") 't
    (kbd "C-n") 'term-send-down
    (kbd "C-p") 'term-send-up)

  (evil-define-key '(motion normal) comint-mode-map
    (kbd "i") 'comint-goto-end-and-insert)
  (evil-define-key '(motion normal) inf-ruby-mode-map
    (kbd "i") 'comint-goto-end-and-insert)

  (evil-define-key 'insert comint-mode-map
    (kbd "C-p") 'comint-previous-input
    (kbd "<Up>") 'comint-previous-input
    (kbd "<Down>") 'comint-next-input))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
