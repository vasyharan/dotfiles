;;; config-syntax-checking.el -- Emacs config for syntax checking.
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :delight
  :hook (prog-mode . flycheck-mode)
  :config
  (add-hook 'flycheck-error-list-mode-hook (lambda () (setq mode-line-format nil))))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (flycheck-pos-tip-mode))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
   :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :color pink
   :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "filter")
  ("j"  flycheck-next-error                                       "next")
  ("k"  flycheck-previous-error                                   "previous")
  ("gg" flycheck-first-error                                      "first")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "last")
  ("q"  nil                                                       "quit" :color blue))

;; (after 'evil-leader (evil-leader/set-key "e" 'hydra-flycheck/body))
(after 'evil-leader (evil-leader/set-key "e" 'flycheck-list-errors))

(provide 'config-syntax-checking)
;;; config-syntax-checking.el ends here
