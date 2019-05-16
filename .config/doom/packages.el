;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))

(package! evil-easymotion :disable t)
(package! evil-snipe :disable t)
(package! evil-escape :disable t)
(package! magit-todos :disable t)

(package! enh-ruby-mode :disable t)
(package! robe :disable t)

(package! doom-themes :recipe
  (:fetcher git
   :url "file://~/Workspace/emacs-doom-themes"
   :branch "gruvbox-dark"
   :files ("*.el" "themes/*.el")))

(package! evil-terminal-cursor-changer :disable t)
(package! ialign)

(package! puppet-mode)
(package! protobuf-mode)
(package! bazel-mode)
