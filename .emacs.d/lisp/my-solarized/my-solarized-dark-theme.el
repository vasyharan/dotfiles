;;; my-solarized-dark-theme.el --- My Solarized dark theme variant.
;;; Commentary:
;;; Code:
(require 'solarized)
(require 'my-solarized-theme)

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")

(create-solarized-theme 'dark 'my-solarized-dark 'my-solarized-theme)

(provide-theme 'my-solarized-dark)
;;; my-solarized-dark-theme ends here
