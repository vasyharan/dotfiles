(require 'load-relative)
(require 'use-package)

(use-package ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter "ruby")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package web-mode
  :mode "\\.erb\\'")

(use-package ruby-tools
  :delight
  :commands (ruby-tools-mode))

(use-package rbenv
  :commands (rbenv-use-corresponding)
  :config
  (setq rbenv-show-active-ruby-in-modeline nil
	ruby-insert-encoding-magic-comment nil))

(use-package inf-ruby
  :commands (inf-ruby-minor-mode))

(defun config-ruby-mode ()
  "Custom settings for Ruby."
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode nil
	ruby-electric-expand-delimiters-list nil)
  (setq-local prettify-symbols-alist
	      '(("lambda"	. ?λ)
		("->"            . ?λ)
		(">="		. ?≥)
		("<="		. ?≤)))
  (require 'rbenv)
  (global-rbenv-mode)
  (rbenv-use-corresponding)
  (ruby-tools-mode +1)
  (superword-mode +1)
  (inf-ruby-minor-mode)
  (modify-syntax-entry ?_ "w")
  (electric-pair-mode +1)
  (add-to-list 'hs-special-modes-alist
		`(ruby-mode
		  ,(rx (or "def" "class" "module" "do" "{" "["))	; Block start
		  ,(rx (or "}" "]" "end"))				; Block end
		  ,(rx (or "#" "=begin"))				; Comment start
		  ruby-forward-sexp nil)))

(defun config-web-mode ()
  "Custom settings for Ruby."
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode nil))

(add-hook 'ruby-mode-hook 'config-ruby-mode)
(add-hook 'enh-ruby-mode-hook 'config-ruby-mode)
(add-hook 'web-mode-hook 'config-web-mode)

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
