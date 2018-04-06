;; init.el -- Emacs completion.
;;; Commentary:
;;; Code:

(defun counsel-git-or-find-file ()
  "Invoke counsel-git if in a repo otherwise counsel-find-file."
  (interactive)
  (if (locate-dominating-file default-directory ".git")
      (counsel-git)
    (counsel-find-file)))

(use-package ivy
  :ensure t
  :delight
  :commands (ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
	ivy-height 10
	ivy-count-format ""
	ivy-initial-inputs-alist nil
	ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  (ivy-mode))

(use-package counsel
  :ensure t
  :commands (counsel-find-file
	     counsel-M-x
	     counsel-git
	     counsel-rg
	     counsel-recentf
	     locate-dominating-file)
  :init
  (after 'evil-leader
    (evil-leader/set-key
      "<SPC>" 'counsel-M-x
      "f" 'counsel-git-or-find-file
      "F" 'counsel-find-file
      "b" 'ivy-switch-buffer
      "k" 'kill-this-buffer
      "w" 'save-buffer
      "W" 'save-some-buffers
      "pp" 'counsel-git
      "ps" 'counsel-rg
      "pr" 'counsel-recentf)))

(use-package counsel-etags
  :ensure t
  :commands (counsel-etags-find-tag-at-point
	     counsel-etags-find-tag))

(provide 'config-completion)
;;; config-completion.el ends here
