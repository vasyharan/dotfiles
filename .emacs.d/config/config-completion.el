(use-package ivy
  :ensure t
  :delight
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
	ivy-height 10
	ivy-count-format ""
	ivy-initial-inputs-alist nil
	ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :demand t)

(defun counsel-locate-git-root ()
  "Locate the root of the git repository containing the current buffer."
  (or (locate-dominating-file default-directory ".git")
      (error "Not in a git repository")))

(defun counsel-git-or-find-file ()
  "Invoke counsel-git if in a repo otherwise counsel-find-file."
  (interactive)
  (if (locate-dominating-file default-directory ".git")
      (counsel-git)
    (counsel-find-file)))

(after 'evil-leader
  (evil-leader/set-key
    "<SPC>" 'counsel-M-x
    "f" 'counsel-git-or-find-file
    "F" 'counsel-find-file
    "b" 'ivy-switch-buffer
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "pp" 'counsel-git
    "ps" 'counsel-rg
    "pr" 'counsel-recentf))

(use-package counsel-etags
  :ensure t
  :demand t)

(provide 'config-completion)
