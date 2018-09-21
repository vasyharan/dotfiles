;;; config-vcs.el -- Emacs config for version control systems.
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :after (evil-leader ivy)
  :commands (magit-status magit-blame)
  :init
  (evil-leader/set-key
    "gb" 'magit-blame
    "gg" 'magit-status)
  :config
  ;; (magit-auto-revert-mode -1)
  (setq magit-refresh-status-buffer nil
	magit-completing-read-function 'ivy-completing-read
	magit-commit-squash-confirm nil
	magit-repository-directories '(("~/stripe" . 1))
	vc-handled-backends (delq 'Git vc-handled-backends)))

(use-package smerge-mode
  :init
  (setq smerge-auto-leave t)
  (defhydra hydra-smerge
    (:foreign-keys run
		   :pre (smerge-start-session)
		   :post (when (bound-and-true-p smerge-mode)
			   (smerge-mode -1)))
    "smerge"
    ("n" smerge-next "next")
    ("p" smerge-prev "prev")
    ("j" evil-next-line)
    ("k" evil-previous-line)
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("m" smerge-keep-mine "mine")
    ("o" smerge-keep-other "other")
    ("c" smerge-keep-current "current")
    ("C" smerge-combine-with-next "combine")
    ("R" smerge-refine "refine")
    ("u" undo-tree-undo)
    ("U" undo-tree-redo)
    ("q" nil :exit t))
  (evil-leader/set-key "gm" 'hydra-smerge/body))

(use-package evil-magit
  :ensure t
  :after magit
  :config
  (evil-magit-define-key evil-magit-state 'magit-mode-map "C-w" 'ace-window))

(use-package git-link
  :ensure t
  :commands (git-link)
  :init
  (evil-leader/set-key
    "go" 'git-link)
  :config
  (setq git-link-open-in-browser t
	git-link-use-commit t)
  (add-to-list 'git-link-remote-alist
      '("git\\.corp\\.stripe\\.com" git-link-github)))

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine)
  :after hydra
  :init
  (defhydra hydra-git-timemachine
    (:foreign-keys run
		   :pre (let (golden-ratio-mode)
			  (unless (bound-and-true-p git-timemachine-mode)
			    (call-interactively 'git-timemachine)))
		   :post (when (bound-and-true-p git-timemachine-mode)
			   (git-timemachine-quit)))
    "git-timemachine"
    ("c" git-timemachine-show-current-revision "current")
    ("g" git-timemachine-show-nth-revision "goto")
    ("p" git-timemachine-show-previous-revision "prev")
    ("n" git-timemachine-show-next-revision "next")
    ("o" git-link nil)
    ("q" nil :exit t))
  (evil-leader/set-key
    "gt" 'hydra-git-timemachine/body))

(defun shorten-vc-mode-line (string)
  "Shortens mode-line STRING for command `vc-mode'."
  (cond
   ((string-prefix-p "Git-" string) (concat "g/" (substring string 4)))
   ((string-prefix-p "Git:" string) (concat "g:" (substring string 4)))
   (t string)))
(advice-add 'vc-git-mode-line-string :filter-return 'shorten-vc-mode-line)

(provide 'config-vcs)
;;; config-vcs.el ends here
