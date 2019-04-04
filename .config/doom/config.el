;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+bindings")

(setq-default confirm-kill-emacs nil
              ;; display-line-numbers 'relative
              )
(setq find-program "gfind"
      lsp-auto-guess-root nil)

(setq doom-font (font-spec :family "Hack" :size 11)
      doom-theme 'doom-gruvbox-dark
      ;; display-line-numbers 'relative
      )
