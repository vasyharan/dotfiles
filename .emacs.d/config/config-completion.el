;; init.el -- Emacs completion.
;;; Commentary:
;;; Code:

(defun counsel-git-or-find-file ()
  "Invoke counsel-git if in a repo otherwise counsel-find-file."
  (interactive)
  (if (locate-dominating-file default-directory ".git")
      (counsel-git)
    (counsel-find-file)))

(defun counsel-find-file-at-root ()
  "Invoke `counsel-find-file' at the root of the repo."
  (interactive)
  (let ((root-directory (locate-dominating-file default-directory ".git")))
    (if root-directory
	(let ((default-directory root-directory))
	  (counsel-find-file))
      (counsel-find-file))))

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

(use-package ivy-xref
  :ensure t
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
  :ensure t
  :commands (counsel-find-file
	     counsel-M-x
	     counsel-git
	     counsel-git-grep
	     counsel-rg
	     counsel-recentf
	     locate-dominating-file)
  :init
  (after 'evil-leader
    (evil-leader/set-key
      "<SPC>" 'counsel-M-x
      "ff" 'counsel-find-file-at-root
      "f." 'counsel-find-file
      "fp" 'counsel-git-or-find-file
      "fs" 'counsel-rg
      "fg" 'counsel-git-grep
      "fr" 'counsel-recentf
      "fo" 'counsel-pay-find-other
      "b" 'ivy-switch-buffer
      "k" 'kill-this-buffer
      "w" 'save-buffer
      "W" 'save-some-buffers)))

;; (use-package counsel-etags
;;   :ensure t
;;   :commands (counsel-etags-find-tag-at-point
;; 	     counsel-etags-find-tag))

(provide 'config-completion)
;;; config-completion.el ends here
