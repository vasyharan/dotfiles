;;; config-lang-ruby.el -- Emacs config for ruby.
;;; Commentary:
;;; Code:


(use-package ruby-mode
  :ensure t
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter "ruby")

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(use-package web-mode :ensure t
  :mode "\\.erb\\'")

(use-package ruby-tools
  :ensure t
  :delight
  :commands (ruby-tools-mode))

(use-package rbenv
  :ensure t
  :commands (rbenv-use-corresponding)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil
	ruby-insert-encoding-magic-comment nil))

(use-package inf-ruby
  :ensure t
  :commands (inf-ruby-minor-mode))

(defun config-ruby-mode ()
  "Custom settings for Ruby."
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode nil
	ruby-electric-expand-delimiters-list nil)
  (setq-local prettify-symbols-alist '(("lambda"	. ?λ)
				       ("->"            . ?λ)
				       (">="		. ?≥)
				       ("<="		. ?≤)))

  (global-rbenv-mode)
  (rbenv-use-corresponding)

  (ruby-tools-mode +1)
  (superword-mode +1)
  (inf-ruby-minor-mode)
  (modify-syntax-entry ?_ "w")
  (electric-pair-mode +1))

(defun config-web-mode ()
  "Custom settings for Ruby."
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode nil))

(add-hook 'ruby-mode-hook 'config-ruby-mode)
(add-hook 'enh-ruby-mode-hook 'config-ruby-mode)
(add-hook 'web-mode-hook 'config-web-mode)

(provide 'config-lang-ruby)
;;; config-lang-ruby.el ends here
