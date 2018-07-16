;;; my-solarized-light-theme.el --- My Solarized light theme variant.
;;; Commentary:
;;; Code:
(require 'solarized)
(require 'my-solarized-theme)

(deftheme my-solarized-light "The light variant of the Solarized colour theme")

(create-solarized-theme 'light 'my-solarized-light 'my-solarized-theme)

(provide-theme 'my-solarized-light)
;;; my-solarized-light-theme ends here
