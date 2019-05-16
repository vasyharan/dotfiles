;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; (setq doom-localleader-key "SPC m")
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

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            yaml-mode))

(if (executable-find "gfind")
    (setq find-program "gfind"))

(after! lsp                            
  (setq lsp-auto-guess-root nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil)

  (defun lsp-ruby-sorbet-activate-p (_filename _mode)
    (and (bound-and-true-p pay-mode) nil))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection '("pay" "exec" "scripts/bin/typecheck" "--lsp"))
                    :activation-fn 'lsp-ruby-sorbet-activate-p
                    :server-id 'sorbet)))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override nil))

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

(after! ruby-mode
  (setq ruby-insert-encoding-magic-comment nil))

(after! enh-ruby-mode
  (setq enh-ruby-add-encoding-comment-on-save nil))

(after! forge
  (add-to-list 'forge-alist '("git.corp.stripe.com"
                              nil
                              "git.corp.stripe.com"
                              forge-github-repository)))

(after! git-link
  (setq git-link-open-in-browser t
        git-link-use-commit t)
  (add-to-list 'git-link-remote-alist
               '("git\\.corp\\.stripe\\.com" git-link-github)))

(def-package! puppet-mode
  :mode "\\.pp\\'"
  :commands (puppet-mode))

(def-package! protobuf-mode
  :mode "\\.proto\\'"
  :commands (protobuf-mode))

(def-package! bazel-mode
  :commands (bazel-mode))

(after! sbt-mode
  (set-popup-rules!
    '(("^\\*sbt*" :ttl nil))))

(add-hook 'pay-mode-hook #'evil-normalize-keymaps)
(set-popup-rule! "^\\*pay-compilation*"
  :height 0.33
  :quit nil)

(defun +javascript|add-root-node-modules-path ()
  "Add current project's `node_modules/.bin` to `exec-path', so js tools
prioritize project-local packages over global ones."
  (make-local-variable 'exec-path)
  (cl-pushnew (expand-file-name "node_modules/.bin/"
                                (doom-project-root))
              exec-path :test #'string=))
(add-hook '+javascript-npm-mode-hook #'+javascript|add-root-node-modules-path)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components/.+\\.ts\\'" . rjsx-mode))

(defun gui-ui-config (&optional frame)
  "UI adjustments hook for GUI frame."
  (or frame (setq frame (selected-frame)))
  (if (and (display-graphic-p frame)
           (eq system-type "darwin"))
      (set-frame-parameter frame 'menu-bar-lines 1)
    (set-frame-parameter frame 'menu-bar-lines 0)))
(add-hook 'after-make-frame-functions #'gui-ui-config t)
