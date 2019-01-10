(require 'load-relative)
(require 'use-package)

(use-package ensime
  :pin melpa-stable
  :after (evil)
  :config
  (setq ensime-startup-notification nil)
  (evil-define-key '(motion) scala-mode-map
    (kbd "C-]") 'ensime-edit-definition
    (kbd "M-]") 'ensime-edit-definition-other-window
    (kbd "C-T") 'pop-tag-mark))

(use-package scala-mode
  :mode "\\.scala\\'"
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
