(require 'load-relative)
(require 'use-package)

(use-package ivy
  :delight
  :commands (ivy-mode)
  :init (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
	ivy-height 10
	ivy-count-format ""
	ivy-initial-inputs-alist nil
	ivy-re-builders-alist '((t   . ivy--regex-ignore-order))))

(use-package ivy-xref
  :after (ivy)
  :commands (ivy-xref-show-xrefs)
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config
  (defun ivy-xref-make-collection (xrefs)
    (let ((collection nil)
	  (root (locate-dominating-file default-directory ".git")))
      (dolist (xref xrefs)
	(with-slots (summary location) xref
	  (let* ((line (xref-location-line location))
		 (file (xref-location-group location))
		 (file-rel)
		 (candidate nil))
	    (setq candidate (concat
			     ;; (car (reverse (split-string file "\\/")))
			     (if root
				 (file-relative-name file root)
			       (shorten-directory file 35))
			     (when (string= "integer" (type-of line))
			       (concat ":" (int-to-string line)))
			     ":\t"
			     (string-trim-left summary)))
	    (push `(,candidate . ,location) collection))))
      (nreverse collection))))

(use-package counsel
  :after (ivy evil-leader)
  :commands (counsel-find-file
	     counsel-M-x
	     counsel-git
	     counsel-git-grep
	     counsel-rg
	     counsel-recentf
	     counsel-locate-git-root
	     locate-dominating-file)
  :init
  (setq counsel-git-cmd "rg --files --color never --")

  (defun counsel-git-or-find-file ()
    "Invoke counsel-git if in a repo otherwise counsel-find-file."
    (interactive)
    (condition-case nil
	(progn
	  (counsel-locate-git-root)
	  (counsel-git))
      (error (counsel-find-file))))

  (defun counsel-find-file-at-root ()
    "Invoke `counsel-find-file' at the root of the repo."
    (interactive)
    (condition-case nil
	(let* ((default-directory (expand-file-name (counsel-locate-git-root))))
	  (counsel-find-file))
      (error (counsel-find-file))))

  (defun counsel-rg-word-at-point ()
    (interactive)
    (counsel-rg (evil-find-thing t 'evil-word)))

  (evil-leader/set-key
    "<SPC>" 'counsel-M-x
    "ff" 'counsel-find-file-at-root
    "f." 'counsel-find-file
    "fp" 'counsel-git-or-find-file
    "f/" 'counsel-rg
    "f*" 'counsel-rg-word-at-point
    "f#" 'counsel-rg-word-at-point
    "fr" 'counsel-recentf
    "fo" 'counsel-pay-find-other
    "fb" 'ivy-switch-buffer
    "ep" 'previous-error
    "en" 'next-error
    "b" 'ivy-switch-buffer
    "k" 'kill-this-buffer
    "w" 'save-buffer
    "W" 'save-some-buffers))

(use-package flyspell-correct-ivy
  :commands (flyspell-correct-wrapper)
  :init
  (evil-leader/set-key
    "s" 'flyspell-correct-wrapper)
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
