;;; config-lang-haskell.el -- My Emacs Haskell config.
;;; Commentary:
;;; Code:

;; (use-package js2-mode
;;   :ensure t
;;   :mode "\\.js\\'")
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(defun config-haskell-mode()
  "Configure Haskell mode.")

(add-hook 'haskell-mode-hook 'config-haskell-mode)

(provide 'config-lang-haskell)
;;; config-lang-haskell.el ends here
