(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package racer
  :ensure t
  :commands (racer-mode))
(use-package flycheck-rust
  :ensure t
  :commands (flycheck-rust-setup))

(defun config-rust-mode ()
  "Custom settings for Rust."
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode t
	racer-rust-src-path (expand-file-name "~/.rustup/toolchains/beta-x86_64-apple-darwin/lib/rustlib/src/rust/src")
	rust-format-on-save t)

  (evil-define-key 'motion rust-mode-map
    (kbd "C-]") 'racer-find-definition
    ;; (kbd "M-]") 'godef-jump-other-window
    )

  (racer-mode)
  (eldoc-mode)
  (flycheck-rust-setup))

(add-hook 'rust-mode-hook 'config-rust-mode)

(provide 'config-lang-rust)
