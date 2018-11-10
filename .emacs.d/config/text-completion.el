(require 'use-package)
(require 'load-relative)

(use-package company
  :delight
  :commands (company-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay nil ;; 0.5
	company-selection-wrap-around t)
  (evil-define-key 'insert 'global (kbd "C-n") 'company-complete)
  (evil-define-key 'insert 'global (kbd "C-p") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-complete)
  (define-key company-active-map (kbd "C-r") 'company-filter-candidates))

(use-package yasnippet
  :delight (yas-minor-mode "" "yasnippet")
  :commands (yas-minor-mode)
  :hook (prog-mode . yas-minor-mode)
  :after (evil)
  :config
  (evil-define-key 'insert 'global (kbd "C-f") 'yas-expand))

(use-package yasnippet-snippets
  :after (yasnippet)
  :defer t
  :config
  (yas-reload-all))

(use-package etags
  :config
  (setq tags-revert-without-query t
	large-file-warning-threshold nil))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
