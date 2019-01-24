;;; config-lang-cxx.el -- My Emacs C/C++ config.
;;; Commentary:
;;; Code:

(use-package clang-format
  :ensure t
  :commands (clang-format-buffer))

(use-package cmake-mode
  :ensure t)

(defun config-c++-mode ()
  "Custom settings for C++."
  (add-hook 'before-save-hook #'clang-format-buffer nil t)
  (setq tab-width 2
	evil-shift-width 2
	indent-tabs-mode nil))

(add-hook 'c++-mode-hook 'config-c++-mode)

(provide 'config-lang-go)
;;; config-lang-cxx.el ends here
