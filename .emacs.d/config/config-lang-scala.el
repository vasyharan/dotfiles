(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode)
  :mode "\\.scala\\'")

(defun config-scala-mode ()
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'scala-mode-hook 'config-scala-mode)
(provide 'config-lang-scala)
