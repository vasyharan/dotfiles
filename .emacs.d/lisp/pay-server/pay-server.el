;;; pay-server.el --- An emacs mode for working in pay-server.
;;; Commentary:
;;; Code:

(defcustom pay-command '("pay")
  "Default command for pay-server."
  :type 'list
  :group 'pay-server)

(defcustom pay-test-subcommand '("test")
  "Default sub command for running tests in pay-server."
  :type 'list
  :group 'pay-server)

(defface pay-test-fringe-face
  '((t :inherit success :foreground "#bd93f9"))
  "Flycheck face for fringe error indicators."
  :group 'pay-server-faces)

(defun pay-project-root ()
  "Retrieve the root directory of pay-server."
  (or (locate-dominating-file default-directory ".git")
      (error "Not in pay-server")))

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'pay-test-fringe-bitmap-arrow
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b10011000
            #b01101100
            #b10110110
            #b11011011
            #b10110110
            #b01101100
            #b10011000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000)))

(define-derived-mode pay-compilation-mode compilation-mode "pay-test"
  (add-hook 'compilation-filter-hook 'pay-transform-traces)
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun pay-transform-traces ()
  "Transform traces from pay commands."
  (read-only-mode 1)
  (let* ((start-marker (if (boundp 'compilation-filter-start) compilation-filter-start (point-min))))
    (save-excursion
      (goto-char start-marker)
      (save-excursion
	(beginning-of-line)
	(while (re-search-forward "\\/pay\\/src\\/pay-server\\/" nil t)
	  (replace-match "")))))
  (read-only-mode -1))

(defun colorize-compilation-buffer ()
  "Decode ANSI code color and apply it."
  (read-only-mode 1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode -1))

(defvar pay-test--last-command nil
  "Variable to store the last test command.")

(defvar pay-test--test-markers nil
  "Variable to store list of marked tests.")

(defun pay-test--command (tests &optional fail-fast verbose)
  "Generate command for TEST, optionally FAIL-FAST and / or be VERBOSE."
  (mapconcat
   (lambda (test)
     (let* ((test-filename (car test))
	    (test-names (alist-get 'names (cdr test)))
	    (test-lines (alist-get 'lines (cdr test)))
	    (args (list
		   (if fail-fast "-f")
		   (if verbose "-v")
		   test-filename
		   (cond (test-names (mapcar (lambda (name) (list "-n" name)) test-names))
			 (test-lines (mapcar (lambda (line) (list "-l" (number-to-string line))) test-lines))))))
       (mapconcat 'shell-quote-argument
		  (-flatten
		   (--remove (eq nil it) (list pay-command pay-test-subcommand args))) " ")))
   tests "; "))

(defun pay-test--save-buffers ()
  (let ((pay-root (pay-project-root)))
    (save-some-buffers
     (not compilation-ask-about-save)
     (lambda ()
       (string-prefix-p (file-truename pay-root) (file-truename (buffer-file-name)))))))

(defun pay-test--run (tests &optional fail-fast verbose)
  "Test specified TESTS, optionally FAIL-FAST and / or be VERBOSE."
  (let* ((pay-root (pay-project-root))
	 (default-directory pay-root)
	 (compilation-scroll-output t))
    (setq pay-test--last-command (list tests fail-fast verbose))
    (pay-test--save-buffers)
    ;; (message (pay-test--command tests fail-fast verbose))
    (compilation-start (pay-test--command tests fail-fast verbose) 'pay-compilation-mode)))

(defun pay-test--make-overlay (beg)
  (let* ((end (save-excursion (end-of-line) (point)))
	 (overlay (make-overlay beg end)))
    (overlay-put overlay 'pay-test-overlay t)
    (overlay-put overlay 'before-string
		 (propertize "!" 'display
			     (list 'left-fringe
				   'pay-test-fringe-bitmap-arrow
				   'pay-test-fringe-face)))
    overlay))

(defun pay-test--current-test (&optional pos)
  "Find the closest test to POS or (current point)."
  (let ((opoint (or pos (point))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char opoint)
	(end-of-line)
	(if (re-search-backward
	     "\\(it\\) \\(\"\\([^\"\\]*\\(\\.[^\"\\]*\\)*\\)\"\\|'\\([^'\\]*\\(\\.[^'\\]*\\)*\\)'\\)" nil t)
	    (let* ((test-filename (buffer-file-name (current-buffer)))
		   (test-raw-name (or (match-string-no-properties 3) (match-string-no-properties 5)))
		   (test-name (format "/%s/" (replace-regexp-in-string "#{[^}]*}" ".*" test-raw-name nil t)))
		   (test-line (line-number-at-pos))
		   (test-pos (point)))
	      (list test-pos test-filename test-name test-line))
	  (error "No test found"))))))

(defun pay-test--marker-find-pred (marker test-filename &optional test-line test-pos)
  (seq-let [marker-filename marker-line marker-marker] marker
    (and (string= marker-filename test-filename)
	 (or (and (not test-line) (not test-pos))
	     (eq marker-line test-line)
	     (eq (marker-position marker-marker) test-pos)))))

(defun pay-test-reverify ()
  "Re verify."
  (interactive)
  (if pay-test--last-command
      (apply 'pay-test--run pay-test--last-command)
    (error "No previous test run")))

(defun pay-test-kill-last ()
  "Kill last verify command."
  (interactive)
  (if pay-test--last-command
      (let ((command (apply 'pay-test--command pay-test--last-command)))
	(kill-new command)
	(message (format "Copied `%s'!" command)))
    (error "No previous test run")))

(defun pay-test-verify-current-buffer ()
  "Verify current buffer."
  (interactive)
  (pay-test--run (list (list (file-relative-name (buffer-file-name) (pay-project-root))))))

(defun pay-test-verify-current-line ()
  "Verify at current point."
  (interactive)
  (pay-test--run (list (list (file-relative-name (buffer-file-name) (pay-project-root))
			     (cons 'lines (list (line-number-at-pos)))))))

(defun pay-test-verify-current-test ()
  "Verify at current point."
  (interactive)
  (let ((current-test (pay-test--current-test)))
    (if current-test
	(let* ((test-filename (nth 1 current-test))
	       (test-name (nth 2 current-test)))
	  (pay-test--run (list (list (file-relative-name test-filename (pay-project-root))
				     (cons 'names (list test-name)))))))))

(defun pay-test-break-current-line ()
  "Copy command to break at current line."
  (interactive)
  (let* ((file-name (file-relative-name (buffer-file-name) (pay-project-root)))
	 (line-no (line-number-at-pos))
	 (break-command (format "break %s:%d" file-name line-no)))
    (kill-new break-command)
    (message (format "Copied `%s'!" break-command))))

(defun pay-test-clear-marks()
  "Remove all marked test."
  (interactive)
  (dolist (test-marker pay-test--test-markers)
    (let ((marker (nth 2 test-marker))
	  (overlay (nth 3 test-marker)))
      (if overlay (delete-overlay overlay))
      (if marker (set-marker marker nil))))
  (setq pay-test--test-markers nil))

(defun pay-test--update-marker (marker)
  (seq-let [test-filename test-line marker overlay] marker
    (list test-filename (line-number-at-pos (marker-position marker)) marker overlay)))

(defun pay-test--update-markers ()
  (let* ((current-filename (buffer-file-name (current-buffer)))
	 (updated-markers
	  (seq-map
	   (lambda (marker)
	     (cond ((pay-test--marker-find-pred marker current-filename)
		    (pay-test--update-marker marker))
		   (t marker))) pay-test--test-markers)))
    (setq pay-test--test-markers updated-markers)))

(defun pay-test--kill-marker (marker)
  (seq-let [test-filename test-line marker overlay] marker
    (if overlay (delete-overlay overlay))
    (if marker (set-marker marker nil))
    (list test-filename test-line nil nil)))

(defun pay-test--kill-markers ()
  (let* ((current-filename (buffer-file-name (current-buffer)))
	 (updated-markers
	  (seq-map
	   (lambda (marker)
	     (cond ((pay-test--marker-find-pred marker current-filename)
		    (pay-test--kill-marker marker))
		   (t marker))) pay-test--test-markers)))
    (setq pay-test--test-markers updated-markers)))

(defun pay-test--restore-marker (marker)
  (seq-let [test-filename test-line marker overlay] marker
    (save-excursion
      (goto-char (point-min))
      (forward-line test-line)
      (let ((current-test (pay-test--current-test)))
	(if current-test
	  (seq-let [test-pos test-filename test-name test-line] current-test
	    (let ((overlay (pay-test--make-overlay test-pos))
		  (marker (make-marker)))
	      (set-marker marker test-pos)
	      (list test-filename test-line marker overlay)))
	  marker)))))

(defun pay-test--restore-markers ()
  (message "%s" (buffer-file-name (current-buffer)))
  (let* ((current-filename (buffer-file-name (current-buffer)))
	 (updated-markers
	  (seq-map
	   (lambda (marker)
	     (cond ((pay-test--marker-find-pred marker current-filename)
		    (pay-test--restore-marker marker))
		   (t marker))) pay-test--test-markers)))
    (setq pay-test--test-markers updated-markers)))

(defun pay-test-mark-current-test ()
  "Add the current test to the list of marked tests."
  (interactive)
  (let ((current-test (pay-test--current-test)))
    (when current-test
	(let* ((test-pos (nth 0 current-test))
	       (test-filename (nth 1 current-test))
	       (test-name (nth 2 current-test))
	       (test-line (nth 3 current-test))
	       (marker (make-marker))
	       (overlay (pay-test--make-overlay test-pos)))
	  (set-marker marker test-pos)
	  (add-to-list 'pay-test--test-markers
		       (list test-filename test-line marker overlay))
	  (message (format "Marked `%d: %s'" test-line test-name))))))

(defun pay-test-unmark-current-test ()
  "Remove the current test from the list of marked tests."
  (interactive)
  (let ((current-test (pay-test--current-test)))
    (when current-test
      (seq-let [test-pos test-filename test-name test-line] current-test
	(let* ((test-marker
		(seq-find
		 (lambda (marker)
		   (pay-test--marker-find-pred marker test-filename test-line test-pos))
		 pay-test--test-markers))
	       (updated-markers
		(seq-remove
		 (lambda (marker) (eq marker test-marker))
		 pay-test--test-markers)))
	  (setq pay-test--test-markers updated-markers)
	  (seq-let [_ _ marker overlay] test-marker
		   (if overlay (delete-overlay overlay))
		   (if marker (set-marker marker nil))))))))

(defun pay-test-verify-marked ()
  "Verifies marked test."
  (interactive)
  (pay-test--save-buffers)
  (let* ((markers-per-test-filename
	 (seq-group-by #'car pay-test--test-markers))
	 (tests
	  (mapcar
	   (lambda (e) (let ((test-filename (car e))
			(test-markers (cdr e)))
		    (list (file-relative-name test-filename (pay-project-root))
			  (cons 'lines
				(mapcar (lambda (m) (nth 1 m)) test-markers)))))
	   markers-per-test-filename)))
    (pay-test--run tests)))


;;;###autoload
(define-minor-mode pay-mode
  "Minor mode for pay test files"
  :lighter " pay"
  :group 'pay-server
  (if pay-mode
      (progn
        ;; (when (boundp 'yas-extra-modes)
        ;;   (if (fboundp 'yas-activate-extra-mode)
        ;;       ;; Yasnippet 0.8.1+
        ;;       (yas-activate-extra-mode 'pay-mode)
        ;;     (make-local-variable 'yas-extra-modes)
        ;;     (add-to-list 'yas-extra-modes 'pay-mode)
        ;;     (yas--load-pending-jits)))
	(when (boundp 'inf-ruby-minor-mode)
	  (add-to-list 'inf-ruby-implementations '("pay" . "pay console"))
	  (setq inf-ruby-default-implementation "pay")
	  (setq-local flycheck-ruby-rubocop-executable (concat (pay-project-root) "scripts/bin/rubocop"))))))

(define-minor-mode pay-test-mode
  "Minor mode for pay test files"
  ;; :lighter " paytest"
  :group 'pay-server
  (pay-mode)
  (when pay-test-mode
    (add-hook 'after-save-hook 'pay-test--update-markers nil t)
    (add-hook 'kill-buffer-hook 'pay-test--kill-markers nil t)
    (add-hook 'find-file-hook 'pay-test--restore-markers nil t)))

(defvar pay-test-snippets-dir
  (let ((current-file-name (or load-file-name (buffer-file-name))))
    (expand-file-name "snippets" (file-name-directory current-file-name)))
  "The directory containing pay-test snippets.")

(defun pay-console ()
  "Start a pay console."
  (interactive)
  (if (boundp 'inf-ruby-minor-mode)
      (inf-ruby "pay")
    (error "Inf ruby is required for pay console")))

(defun pay-test-install-snippets ()
  "Add `pay-test-snippets-dir' to `yas-snippet-dirs' and load\
snippets from it."
  (let ((yasnippet-available (require 'yasnippet nil t)))
    (if yasnippet-available
        (progn
          (add-to-list 'yas-snippet-dirs pay-test-snippets-dir t)
          (yas-load-directory pay-test-snippets-dir)))))

(defconst pay-test-file-name-re "\\/test\\/"
  "The regex to identify test files.")

(defun pay-test-file-p (file-name)
  "Return non-nil if the specified FILE-NAME is a test."
  (and (pay-file-p file-name)
       (numberp (string-match pay-test-file-name-re file-name))))

(defun pay-file-p (file-name)
  "Return non-nil if the specified FILE-NAME is a test."
    (string-prefix-p (file-truename (pay-project-root)) (file-truename file-name)))

(defun pay-buffer-is-pay-test-p ()
  "Return non-nil if the current buffer is a pay test."
  (and (buffer-file-name)
       (pay-test-file-p (buffer-file-name))))

(defun pay-buffer-is-pay-file-p ()
  "Return non-nil if the current buffer is a test."
  (and (buffer-file-name)
       (pay-file-p (buffer-file-name))))

;;;###autoload
(defun pay-enable-appropriate-mode ()
  (cond ((pay-buffer-is-pay-test-p) (pay-test-mode))
	((pay-buffer-is-pay-file-p) (pay-mode))))

;;;###autoload
(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
  (add-hook hook 'pay-enable-appropriate-mode))

(provide 'pay-server)
;;; pay-server.el ends here
