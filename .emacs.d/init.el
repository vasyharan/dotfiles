;; We do not use package-initialize, we use use-package!
;; Emacs 25 puts back package-initialize if it's not found commented out.
;; (package-initialize)

;; extra heap on initialize
;; (setq-default garbage-collection-messages t)
(unless after-init-time
  (setq gc-cons-threshold (* 1024 1024 1024))
  (add-hook 'emacs-startup-hook `(lambda ()
             (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
             (garbage-collect)) t))

;; avoid flickering
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(eval-and-compile
  (setq package-enable-at-startup nil
	package-user-dir "~/.emacs.d/elpa/"
	package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ("melpa" . "https://melpa.org/packages/")
			   ("melpa-stable" . "https://stable.melpa.org/packages/"))
	package-archive-priorities '(("melpa" . 10)
				     ("melpa-stable" . 1))))

(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
	(require 'package)
	(package-initialize)
	(unless (package-installed-p 'use-package)
	  (package-refresh-contents)
	  (package-install 'use-package))
	(require 'use-package)
	(setq use-package-always-ensure t)
	(let ((package-user-dir-real (file-truename package-user-dir)))
	  ;; The reverse is necessary, because outside we mapc
	  ;; add-to-list element-by-element, which reverses.
	  (nreverse (apply #'nconc
			   (mapcar #'(lambda (path)
				       (if (string-prefix-p package-user-dir-real path)
					   (list path) nil)) load-path))))))

(eval-when-compile
  (require 'use-package))

(use-package load-relative)

(use-package goto-chg)
(use-package undo-tree
  :delight
  :after (evil-leader)
  :commands (undo-tree-mode)
  :config
  (evil-leader/set-key "u" 'undo-tree-visualize))

(use-package delight
  :commands delight)

(use-package utils
  :load-path "elisp")

;; directory constants
(setq user-emacs-directory
      (file-name-directory user-init-file))
(eval-and-compile
  (defconst user-cache-directory
    (expand-file-name (concat user-emacs-directory "cache/"))
    "Storage area for persistent files.")
  (defconst user-auto-save-directory
    (expand-file-name (concat user-cache-directory "auto-save/"))
    "Emacs auto save files directory.")
  (defconst user-backup-directory
    (expand-file-name (concat user-cache-directory "backup/"))
    "Emacs backup files directory."))

(eval-when-compile
  (unless (file-exists-p user-cache-directory)
    (make-directory user-cache-directory))
  (unless (file-exists-p user-auto-save-directory)
    (make-directory user-auto-save-directory)))

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
		  "~/.nodenv/shims/"
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

(use-package subword
  :delight superword-mode)

(use-package autorevert
  :commands (auto-revert-mode))

(use-package which-key
  :delight
  :commands (which-key-mode)
  :init
  (which-key-mode))

(use-package flyspell
  :ensure t
  :commands (flyspell-prog-mode flyspell-mode))

(use-package hydra
  :defer t
  :config
  (setq hydra-lv t
	lv-use-separator nil))

(require-relative-list
 '("config/evil"
   "config/completion"
   "config/frame"
   "config/window"
   "config/mode-line"
   "config/term"
   "config/user-interface"
   "config/version-control"
   "config/linter"
   "config/code-navigation"
   "config/text-completion"
   "config/language-server"
   "config/pay-server"
   "config/mode/org"
   "config/mode/javascript"
   "config/mode/ruby"
   "config/mode/python") "config-")

(if (daemonp)
    (require 'magit))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
