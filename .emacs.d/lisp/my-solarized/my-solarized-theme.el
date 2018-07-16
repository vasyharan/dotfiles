;;; my-solarized-theme.el --- My Solarized for Emacs.
;;; Commentary:
;;; Code:
(defun my-solarized-theme (theme-name)
  "My solarized child theme."
  (custom-theme-set-faces
   theme-name
   `(mode-line-directory ((,class (:underline ,s-mode-line-underline :foreground ,s-mode-line-fg))))
   `(mode-line-evil-normal ((,class (:underline ,s-mode-line-underline :foreground ,green-hc))))
   `(mode-line-evil-emacs ((,class (:underline ,s-mode-line-underline :foreground ,yellow-hc))))
   `(mode-line-evil-insert ((,class (:underline ,s-mode-line-underline :foreground ,blue-hc))))
   `(mode-line-evil-motion ((,class (:underline ,s-mode-line-underline :foreground ,violet-hc))))
   `(mode-line-evil-operator ((,class (:underline ,s-mode-line-underline :foreground ,magenta-hc))))
   `(mode-line-evil-replace ((,class (:underline ,s-mode-line-underline :foreground ,red-hc))))
   `(mode-line-flycheck-separator ((,class (:underline ,s-mode-line-underline :foreground ,s-mode-line-fg ))))
   `(mode-line-flycheck-error ((,class (:underline ,s-mode-line-underline :foreground ,red-hc))))
   `(mode-line-flycheck-warning ((,class (:underline ,s-mode-line-underline :foreground ,orange-hc))))
   `(mode-line-flycheck-info ((,class (:underline ,s-mode-line-underline :foreground ,blue-hc))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'my-solarized-theme)
;;; my-solarized-theme.el ends here
