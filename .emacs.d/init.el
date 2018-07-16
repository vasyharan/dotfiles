;; init.el -- Emacs initialization file.
;;; Commentary:
;;; Code:

;; better performance?
(setq-default garbage-collection-messages t)

(defun set-standard-gc-cons-threshold ()
  "Reset `gc-cons-threshold' to the standard value."
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))

(defun set-high-gc-cons-threshold ()
  "Set `gc-cons-threshold' to 1GB."
  (setq gc-cons-threshold (* 1024 1024 1024)))

(unless after-init-time
  (set-high-gc-cons-threshold)
  (add-hook 'emacs-startup-hook #'set-standard-gc-cons-threshold t))

;; avoid flickering?
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; setup packages.
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 10)
				   ("melpa-stable" . 1)))

(package-initialize)
(setq use-package-verbose t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; directory constants
(setq user-emacs-directory
      (file-name-directory user-init-file))
(defconst user-cache-directory
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Storage area for persistent files.")
(defconst user-auto-save-directory
  (expand-file-name (concat user-cache-directory "auto-save/"))
  "Emacs auto save files directory.")
(defconst user-backup-directory
  (expand-file-name (concat user-cache-directory "backup/"))
  "Emacs backup files directory.")

(unless (file-exists-p user-cache-directory)
  (make-directory user-cache-directory))
(unless (file-exists-p user-auto-save-directory)
  (make-directory user-auto-save-directory))

;; custom file.
(setq custom-file
  (expand-file-name "custom.el" user-cache-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file t)

;; make command/options keys behave normally.
(if (eq system-type "darwin")
    (setq mac-option-modifier 'meta
	  mac-command-modifier 'hyper))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t		;; empty screen on start
      initial-scratch-message nil
      ring-bell-function 'ignore	;; no bells or whistles
      visible-bell nil)

(setq user-full-name "Vaseeharan Thirukumaran"
      user-mail-address "vasyharan@gmail.com")

(setq exec-path '("~/go/bin"
		  "~/.rbenv/shims"
		  "~/.pyenv/shims/"
		  "~/.cargo/bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"))

;; backups
(setq backup-directory-alist `((".*" . ,user-backup-directory))
      backup-by-copying t
      delete-by-moving-to-trash t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; auto-save
(setq auto-save-file-name-transforms
      `((".*" ,user-auto-save-directory t)))

;; save history
(use-package savehist
  :commands (savehist-mode)
  :init
  (setq savehist-file (expand-file-name "savehist" user-cache-directory)
	history-length t
	history-delete-duplicates t
	savehist-save-minibuffer-history t
	savehist-additional-variables
	'(kill-ring
	  search-ring
	  regexp-search-ring))
  (savehist-mode))

;; recent files.
(use-package recentf
  :init
  (setq recentf-save-file
	(expand-file-name "recentf" user-cache-directory)))

(use-package after
  :load-path "lisp/after"
  :commands (after))

(use-package pay-server
  :load-path "lisp/pay-server"
  :init
  (add-hook 'ruby-mode 'pay-enable-appropriate-mode)
  :delight pay-mode pay-test-mode
  :config
  (after 'evil-leader
    (evil-leader/set-key
      "cc"	'pay-test-reverify
      "cf"	'pay-test-verify-current-buffer
      "cl"	'pay-test-verify-current-line
      "cn"	'pay-test-verify-current-test
      "cy"	'pay-test-kill-last
      "cb"	'pay-test-break-current-line
      "ca"      'pay-test-mark-current-test
      "cA"      'pay-test-unmark-current-test
      "cm"	'pay-test-verify-marked
      "cM"      'pay-test-clear-marks)
    (evil-leader/set-key-for-mode 'pay-compilation-mode
      "d" 'inf-ruby-switch-from-compilation)
    (evil-leader/set-key-for-mode 'inf-ruby-mode
      "d" 'inf-ruby-maybe-switch-to-compilation)))

(use-package utils
  :load-path "lisp"
  :demand t
  :commands (define-keys
	      global-set-keys
	      global-unset-keys
	      toggle-comment-region-or-line
	      load-config-file)
  :config
  (if (eq system-type "darwin")
      (global-set-keys
       [(hyper v)] 'yank
       [(hyper c)] 'kill-ring-save
       [(hyper s)] 'save-buffer
       [(hyper q)] 'save-buffers-kill-emacs))

  (after 'evil-leader
    (evil-leader/set-key
      ";"		'toggle-comment-region-or-line)))

(use-package delight
  :ensure t
  :commands delight)

(use-package subword
  :delight superword-mode)

(use-package autorevert
  :delight auto-revert-mode
  :commands (auto-revert-mode))

(use-package undo-tree
  :ensure t
  :delight
  :commands (undo-tree-mode)
  :config
  (after 'evil-leader
    (evil-leader/set-key "u" 'undo-tree-visualize)))

(use-package which-key
  :ensure t
  :delight
  :commands (which-key-mode)
  :init
  (which-key-mode))

(use-package flyspell
  :ensure t
  :delight
  :commands (flyspell-prog-mode flyspell-mode))

(use-package hydra
  :ensure t
  :defer t
  :custom-face
  :config
  (setq hydra-lv t
	lv-use-separator nil))

(use-package xclip
  :ensure t
  :init
  (xclip-mode))

(load-config-file "config-evil.el")
(load-config-file "config-completion.el")

(load-config-file "config-frame.el")
(load-config-file "config-window.el")
(load-config-file "config-ui.el")
(load-config-file "config-mode-line.el")

(load-config-file "config-vcs.el")
(load-config-file "config-prog.el")
(load-config-file "config-code-completion.el")
(load-config-file "config-syntax-checking.el")

(load-config-file "config-org.el")
(load-config-file "config-lang-ruby.el")
(load-config-file "config-lang-js.el")

(defun minibuffer-setup ()
  "Minibuffer setup hook."
  (setq inhibit-message t)
  (set-high-gc-cons-threshold))

(defun minibuffer-exit ()
  "Minibuffer exit hook."
  (setq inhibit-message nil)
  (set-standard-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'minibuffer-exit)

(provide 'init)
;;; init.el ends here
