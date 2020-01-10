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

(setenv "JAVA_HOME"
        (string-trim (shell-command-to-string
                      "/usr/libexec/java_home -v 1.8")))

(setq-default confirm-kill-emacs nil)

(setq doom-font (if (string= system-type "darwin")
                    (font-spec :family "Fira Code Retina" :size 10)
                  (font-spec :family "InconsolataGo" :size 12))
      doom-modeline-height 8
      doom-modeline-persp-name t
      doom-modeline-lsp t
      doom-modeline-icon nil
      doom-modeline-buffer-file-name-style 'truncate-with-project
      ;; doom-theme 'doom-Iosvkem
      ;; doom-theme 'doom-solarized-dark
      doom-theme 'doom-gruvbox
      )

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            yaml-mode))

(after! doom-themes
  (doom-themes-visual-bell-config))

(if (executable-find "gfind")
    (setq find-program "gfind"))

(after! lsp
  (setq lsp-auto-guess-root nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil)

  (defun lsp-ruby-sorbet-activate-p (_filename _mode)
    (and (bound-and-true-p pay-mode) t))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection '("pay" "exec" "scripts/bin/typecheck" "--lsp"))
                    :activation-fn 'lsp-ruby-sorbet-activate-p
                    :server-id 'sorbet)))

(defun +default/search--symbol-or-region (&optional initial)
  (cond ((stringp initial) initial)
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        (t
         (thing-at-point 'symbol t))))

(defun +default/search-project-for-symbol-or-region (&optional arg symbol)
  "Conduct a text search in the current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list current-prefix-arg (+default/search--symbol-or-region)))
  (+default/search-project-for-symbol-at-point arg symbol))

(after! xref
  (add-hook 'xref-backend-functions #'etags--xref-backend))

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
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))

(def-package! evil-cleverparens
  :hook ((lisp-mode clojure-mode) . evil-cleverparens-mode))

(after! sbt-mode
  (set-popup-rules!
    '(("^\\*sbt*" :ttl nil))))

(add-hook 'pay-mode-hook #'evil-normalize-keymaps)
(defun +pay|disable-slow-things ()
  (smartparens-mode -1)
  (electric-indent-mode -1)
  (highlight-indent-guides-mode -1)
  (highlight-numbers-mode -1)
  (setq-local flycheck-check-syntax-automatically '(save)))
(add-hook 'pay-mode-hook #'evil-normalize-keymaps)
(add-hook 'pay-mode-hook #'+pay|disable-slow-things t)
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
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(defun gui-ui-config (&optional frame)
  "UI adjustments hook for GUI frame."
  (message "gui-ui-config")
  (or frame (setq frame (selected-frame)))
  (if (and (display-graphic-p frame)
           (string= system-type "darwin"))
      (set-frame-parameter frame 'menu-bar-lines 1)
    (set-frame-parameter frame 'menu-bar-lines 0)))
;; (gui-ui-config)
(add-to-list 'after-make-frame-functions #'gui-ui-config)
