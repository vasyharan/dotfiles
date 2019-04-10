;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-localleader-key "\\")
(load! "+bindings")

(setq exec-path '("~/go/bin"
		  "~/.rbenv/shims"
		  "~/.pyenv/shims/"
		  "~/.nodenv/shims/"
		  "~/.cargo/bin"
		  "~/.bin"
		  "/usr/local/bin"
		  "/usr/bin"
		  "/bin"
		  "/usr/sbin"
		  "/sbin"))

(setq-default confirm-kill-emacs nil)

(setq ;; doom-font (font-spec :family "Fira Code Retina" :size 11)
      doom-font (font-spec :family "InconsolataGo" :size 12)
      doom-modeline-height 15
      doom-modeline-persp-name t
      doom-modeline-lsp t
      doom-modeline-icon nil
      doom-modeline-buffer-file-name-style 'truncate-with-project
      doom-theme 'doom-gruvbox-dark)

(setq persp-emacsclient-init-frame-behaviour-override nil)

(if (executable-find "gfind")
    (setq find-program "gfind"))

(after! lsp
  (setq lsp-auto-guess-root nil)

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection 'lsp-scala--server-command)
                    :major-modes '(scala-mode)
                    :server-id 'scala)))

(after! highlight-indent-guides
  (add-hook 'highlight-indent-guides-mode-hook #'highlight-indent-guides-auto-set-faces))

(after! ivy-xref
  (setq ivy-xref-use-file-path t))

(after! dumpjump
  (setq dumb-jump-max-find-time 10))

(def-package! evil-terminal-cursor-changer
  :commands (etcc-on etcc-off)
  ;; :hook (tty-setup . etcc-on)
  :init
  (setq evil-motion-state-cursor 'box   ; █
        evil-visual-state-cursor 'box   ; █
        evil-normal-state-cursor 'box   ; █
        evil-insert-state-cursor 'bar   ; ⎸
        evil-emacs-state-cursor  'hbar) ; _
  )

(add-hook 'pay-mode-hook #'evil-normalize-keymaps)
(set-popup-rule! "^\\*pay-compilation*"
  :height 0.33
  :quit nil)

(defun gui-ui-config (&optional frame)
  "UI adjustments hook for GUI frame."
  (or frame (setq frame (selected-frame)))
  (when (display-graphic-p frame)
    (if (eq system-type "darwin")
        (set-frame-parameter frame 'menu-bar-lines 1))))

(gui-ui-config)
(add-hook 'after-make-frame-functions #'gui-ui-config t)
