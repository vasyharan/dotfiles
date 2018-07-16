;; config-prod.el -- Emacs prog mode.
;;; Commentary:
;;; Code:

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(use-package hl-line
  :commands (hl-line-mode)
  :hook (prog-mode . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(use-package flyspell
  :commands (flyspell-prog-mode)
  :hook (prog-mode . flyspell-prog-mode))

(use-package eglot
  :ensure t
  :commands (eglot)
  :init
  (setq eglot-server-programs '((rust-mode . (eglot-rls "rls"))
				(python-mode . ("pyls"))
				((js-mode js2-mode rjsx-mode) . ("flow-language-server" "--stdio" "--no-auto-download" "--try-flow-bin"))
				(sh-mode . ("bash-language-server" "start"))
				((c++-mode c-mode) . (eglot-cquery "cquery"))
				(ruby-mode . ("solargraph" "socket" "--port" :autoport))
				(php-mode . ("php" "vendor/felixfbecker/language-server/bin/php-language-server.php")))))

(use-package lsp-mode
  :ensure t
  :commands (lsp-ruby-enable))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode))

(defun default-prog-hook()
  "Default `prog-mode' hook."
  (setq-default show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'default-prog-hook)

(provide 'config-prog)
;;; config-prog.el ends here
