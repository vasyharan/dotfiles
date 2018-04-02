;;; config-org.el -- My Emcas org settings.
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :commands (org-agenda)
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
  :config
  (evil-define-key 'normal org-capture-mode-map
    (kbd "+") 'org-priority-up
    (kbd "-") 'org-priority-down)
  (evil-define-key '(normal insert) org-capture-mode-map
    (kbd "C-=" ) 'org-priority-up
    (kbd "C--" ) 'org-priority-down)
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-agenda
  :config
  ;; (add-to-list 'evil-leader/no-prefix-mode-rx "org-agenda-mode")

  (defhydra hydra-org-agenda (:exit t :foreign-keys warn)
    ("c" hydra-org-clock/body "clock")
    ("d" hydra-org-dates/body "dates")
    ("h" hydra-org-entry/body "entry")
    ("K" org-agenda-capture "capture")
    ("q" nil nil))

  (defhydra hydra-org-clock (:exit t :foreign-keys warn)
    "Clock"
    ("x" org-agenda-clock-cancel "cancel")
    ("j" org-agenda-clock-goto "goto" :exit nil)
    ("i" org-agenda-clock-in "in")
    ("o" org-agenda-clock-out "out")
    ("q" nil nil :hint nil))

  (defhydra hydra-org-dates (:exit t :foreign-keys warn)
    "Dates"
    ("t" org-agenda-date-prompt "timestamp")
    ("d" org-agenda-deadline "deadline")
    ("s" org-agenda-schedule "schedule")
    ("+" org-agenda-do-date-later "later" :exit nil)
    ("-" org-agenda-do-date-earlier "earlier" :exit nil)
    ("q" nil nil :hint nil))

  (defhydra hydra-org-entry (:exit t :foreign-keys warn)
    "Entry"
    ("A" org-agenda-archive-default "archive")
    ("x" org-agenda-kill "kill")
    ("p" org-agenda-priority "priority")
    ("r" org-agenda-refile "refile")
    (":" org-agenda-set-tags "tag")
    ("t" org-agenda-todo "todo")
    ("q" nil nil :hint nil))

  (define-keys org-agenda-mode-map
    ;; (kbd "<SPC>")	'hydra-org-agenda/body

    ;; (kbd "RET")	'org-agenda-show-and-scroll-up
    ;; (kbd "M-RET")	'org-agenda-switch-to

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
  :commands (org-bullets-mode)
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun config-org-agenda-mode()

  ;; Hydra for org agenda (graciously taken from Spacemacs)
  (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
				   :post (setq which-key-inhibit nil)
				   :hint none)
    "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   link-hint-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo)))

(provide 'config-org)
;;; config-org.el ends here
