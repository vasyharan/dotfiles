;;; config-org.el -- My Emcas org settings.
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :after (evil-leader)
  :init
  (evil-leader/set-key
    "a" 'org-agenda)
  (setq org-clock-persist-file (concat user-cache-directory
				       "org-clock-save.el")
	org-id-locations-file (concat user-cache-directory
				      "org-id-locations")
	org-publish-timestamp-directory (concat user-cache-directory
						"org-timestamps")
	org-special-ctrl-a/e t
	org-enforce-todo-dependencies t
	org-log-into-drawer t
	org-log-done (quote time)
	org-log-redeadline (quote time)
	org-log-reschedule (quote time)
	org-src-fontify-natively t
	org-clock-persist t)

  :config
  (setq org-directory "~/org"
	org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-todo-keywords
	'((sequence "TODO(t!)" "|" "DONE(d!)")
	  (sequence "JIRA(j!)" "BLOCKED(b!)" "INREVIEW(r!)" "|" "DONE(d!)")
	  (sequence "|" "CANCELED(c!)"))
	org-agenda-custom-commands
	'(("a" "Agenda for the week" ((agenda "") (alltodo ""))))
	org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %?\n  %u")
	  ("j" "Jira" entry (file+headline org-default-notes-file "Tasks")
	   "* JIRA %?\n  :PROPERTIES:\n  :ISSUE(s): [[https://jira.corp.stripe.com/browse/%^{project|COMPLENG}-%^{item}][%\\1-%\\2]]\n  :END:\n  %u")))
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (evil-define-key 'normal org-mode-map
    (kbd "C-H-j") 'org-shiftmetadown
    (kbd "C-H-k") 'org-shiftmetaup
    (kbd "C-w") 'ace-window)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (define-key org-read-date-minibuffer-local-map (kbd "M-h")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-l")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-k")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-j")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-H")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-L")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-K")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-J")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-year 1)))))

(use-package org-capture
  :after (org)
  :commands (org-capture)
  :config
  (evil-define-key 'normal org-capture-mode-map
    (kbd "+") 'org-priority-up
    (kbd "-") 'org-priority-down)
  (evil-define-key '(normal insert) org-capture-mode-map
    (kbd "C-=" ) 'org-priority-up
    (kbd "C--" ) 'org-priority-down)
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-agenda
  :after (org)
  :commands (org-agenda)
  :config
  ;; (add-to-list 'evil-leader/no-prefix-mode-rx "org-agenda-mode")

  (define-keys org-agenda-mode-map
    (kbd "j")		'org-agenda-next-item
    (kbd "k")		'org-agenda-previous-item
    (kbd "n")		'org-agenda-next-date-line
    (kbd "p")		'org-agenda-previous-date-line
    (kbd "M-j")		'org-agenda-next-item
    (kbd "M-k")		'org-agenda-previous-item
    (kbd "M-h")		'org-agenda-earlier
    (kbd "M-l")		'org-agenda-later

    (kbd "K")		'org-agenda-capture

    (kbd "u")		'org-agenda-undo
    (kbd "U")		'org-agenda-redo))

(use-package org-bullets
  :ensure t
  :after (org)
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'config-org)
;;; config-org.el ends here
