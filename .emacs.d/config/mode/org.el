(require 'use-package)
(require 'load-relative)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :after (evil-leader)
  :init
  (evil-leader/set-key
    "I" 'gtd-inbox
    "G" 'gtd
    "o" 'org-agenda
    "K" 'org-capture)
  :commands (gtd)
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-enforce-todo-dependencies t
	org-log-into-drawer t
	org-log-done (quote time)
	org-log-redeadline (quote time)
	org-log-reschedule (quote time)
	org-src-fontify-natively t
	org-default-notes-file "~/.notes.org"
	org-agenda-files `("~/org/gtd.org"
			   "~/org/inbox.org")
	org-refile-targets '(("~/org/gtd.org" :level . 1)
			     ("~/org/inbox.org" :level . 1)
			     ("~/org/someday.org" :level . 1))
	org-todo-keywords '((sequence "TODO(t)"
				      "STARTED(s!)"
				      "BLOCKED(b@/!)"
				      "WAITING(w!/!)"
				      "|"
				      "DONE(d!)"
				      "CANCELED(c@)")))

  (defun gtd-inbox()
      (interactive)
      (find-file "~/org/inbox.org"))
  (defun gtd()
      (interactive)
      (find-file "~/org/gtd.org"))

  (evil-define-key 'normal org-mode-map
    (kbd "C-w") 'ace-window)
  (evil-leader/set-key-for-mode 'org-mode
    "t" 'org-todo)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; calendar keys
  (defmacro define-org-calendar-keys (key def &rest bindings)
    (declare (indent defun))
    (let ((expanded nil))
      (while key
	(push `(define-key org-read-date-minibuffer-local-map ,key
		 (lambda () (interactive)
		   (org-eval-in-calendar ,def))) expanded)
	(setq key (pop bindings)
	      def (pop bindings)))
      `(progn ,@expanded)))

  (define-org-calendar-keys
    (kbd "M-h") '(calendar-backward-day 1)
    (kbd "M-l") '(calendar-forward-day 1)
    (kbd "M-k") '(calendar-backward-week 1)
    (kbd "M-j") '(calendar-forward-week 1)
    (kbd "M-H") '(calendar-backward-month 1)
    (kbd "M-L") '(calendar-forward-month 1)
    (kbd "M-K") '(calendar-backward-year 1)
    (kbd "M-J") '(calendar-forward-year 1)))

(use-package org-capture
  :ensure nil
  :after (org)
  :commands (org-capture)
  :config
  (evil-define-key 'normal org-capture-mode-map
    (kbd "+") 'org-priority-up
    (kbd "-") 'org-priority-down)
  (evil-define-key '(normal insert) org-capture-mode-map
    (kbd "C-=" ) 'org-priority-up
    (kbd "C--" ) 'org-priority-down)
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file+headline "~/org/inbox.org" "Unfiled")
	   "* TODO %i%?")
	  ("j" "Jira" entry
	   (file+headline "~/org/inbox.org" "Unfiled")
	   "* TODO %?\n  :ISSUE(s): [[https://jira.corp.stripe.com/browse/VERPLAT-%^{item}][VERPLAT-%\\1]]\n  %u")))
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-agenda
  :ensure nil
  :after (org evil-leader)
  :commands (org-agenda)
  :config
  ;; (add-to-list 'evil-leader/no-prefix-mode-rx "org-agenda-mode")
  (setq org-agenda-custom-commands
	'(("i" "Inbox" todo "TODO")
	  ("o" "Today"
	   ((agenda "" ((org-agenda-span 2)
			(org-agenda-sorting-strategy
			 (quote ((agenda time-up priority-down tag-up) )))
			(org-deadline-warning-days 0)))))))

  (define-keys org-agenda-mode-map
    (kbd "C-w") 'ace-window
    (kbd "j")	'org-agenda-next-item
    (kbd "k")	'org-agenda-previous-item
    (kbd "n")	'org-agenda-next-date-line
    (kbd "p")	'org-agenda-previous-date-line
    (kbd "M-j")	'org-agenda-next-item
    (kbd "M-k")	'org-agenda-previous-item
    (kbd "M-h")	'org-agenda-earlier
    (kbd "M-l")	'org-agenda-later

    (kbd "K")	'org-agenda-capture
    (kbd "N")	'org-add-note

    (kbd "u")	'org-agenda-undo
    (kbd "U")	'org-agenda-redo))

(use-package org-clock
  :ensure nil
  :after (org)
  :defines user-cache-directory
  :config
  (org-clock-persistence-insinuate)
  (setq org-clock-persist-file (concat user-cache-directory "org-clock-save.el")
	org-clock-persist t
	org-clock-idle-time 10))

(use-package org-bullets
  :after (org)
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide-me "config-")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
