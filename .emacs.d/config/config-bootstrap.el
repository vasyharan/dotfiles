;; config-bootstrap.el -- My emacs config bootstrap file.
;;; Commentary:
;;; Code:
(defun add-to-load-path (dir)
  "Add DIR to `load-path'."
  (add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
  "Add DIR to `load-path' if it exists."
  (when (file-exists-p dir) (add-to-load-path dir)))

(add-to-load-path
 (expand-file-name (concat user-emacs-directory "config/")))

;; hide these before anything else.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; better performance?
(setq gc-cons-threshold (* 100 1024 1024 1024))

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
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(provide 'config-bootstrap)
;;; config-bootstrap.el ends here
