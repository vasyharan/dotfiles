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
            #b00011000
            #b00001100
            #b00000110
            #b00000011
            #b00000110
            #b00001100
            #b00011000
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

(defvar pay-test--test-markers (list)
  "Variable to store list of marked tests.")

(defun pay-test--command (filenames &optional names lines fail-fast verbose)
  (let* ((args (list
		(if fail-fast "-f")
		(if verbose "-v")
		filenames
		(if names (mapcar (lambda (name) (list "-n" name)) names))
		(if lines (mapcar (lambda (line) (list "-l" (number-to-string line))) lines)))))
	 (mapconcat 'shell-quote-argument
		(-flatten
		 (--remove (eq nil it) (list pay-command pay-test-subcommand args))) " ")))

(defun pay-test--run (filenames &optional names lines fail-fast verbose)
  (let* ((pay-root (pay-project-root))
	 (default-directory pay-root)
	 (compilation-scroll-output t))
    (setq pay-test--last-command (list filenames names lines fail-fast verbose))
    (save-some-buffers
     (not compilation-ask-about-save)
     (lambda ()
       (string-prefix-p (file-truename pay-root) (file-truename (buffer-file-name)))))
    (compilation-start
     (pay-test--command filenames names lines fail-fast verbose)
     'pay-compilation-mode)))

(defun pay-test--extract-test-name ()
  (save-excursion
    (save-restriction
      (widen)
      (end-of-line)
      (if (re-search-backward "\\(it\\) \\(\"\\([^\"\\]*\\(\\.[^\"\\]*\\)*\\)\"\\|'\\([^'\\]*\\(\\.[^'\\]*\\)*\\)'\\)" nil t)
	  (let ((test-name (or (match-string 3) (match-string 5))))
	    (format "/%s/" (replace-regexp-in-string "#{[^}]*}" ".*" test-name nil t)))))))

(defun pay-test--add-overlay (beg)
  (let* ((end (save-excursion (end-of-line) (point)))
	 (overlay (make-overlay beg end)))
    (overlay-put overlay 'pay-test-overlay t)
    (overlay-put overlay 'before-string
		 (propertize "!" 'display
			     (list 'left-fringe
				   'pay-test-fringe-bitmap-arrow
				   'pay-test-fringe-face)))))

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
  (pay-test--run (list (file-relative-name (buffer-file-name) (pay-project-root)))))

(defun pay-test-verify-current-line ()
  "Verify at current point."
  (interactive)
  (pay-test--run (list (file-relative-name (buffer-file-name) (pay-project-root)))
		 nil
		 (list (line-number-at-pos))))

(defun pay-test-verify-current-test ()
  "Verify at current point."
  (interactive)
  (let ((test-name (pay-test--extract-test-name)))
    (if test-name
	(pay-test--run (list (file-relative-name (buffer-file-name) (pay-project-root)))
		 (list test-name))
      (error "No test found"))))

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
  (remove-overlays nil nil 'pay-test-overlay t)
  (setq pay-test--test-markers (list)))

(defun pay-test-mark-current-test ()
  "Add the test at current line to the list of test to run."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (end-of-line)
      (if (re-search-backward "\\(it\\) \\(\"\\([^\"\\]*\\(\\.[^\"\\]*\\)*\\)\"\\|'\\([^'\\]*\\(\\.[^'\\]*\\)*\\)'\\)" nil t)
	  (let ((test-name (or (match-string 3) (match-string 5)))
		(test-line (line-number-at-pos))
		(test-marker (make-marker)))
	    (set-marker test-marker (point))
	    (pay-test--add-overlay (point))
	    (add-to-list 'pay-test--test-markers test-marker)
	    (message (format "Marked `%d: %s'" test-line test-name)))))))

(defun pay-test-verify-marked ()
  "Verifies markerd test."
  (interactive)
  (if (eq 0 (length pay-test--test-markers))
      (error "No marked tests")
    (let ((line-nos
	   (mapcar (lambda (m) (line-number-at-pos (marker-position m)))
		   pay-test--test-markers))
	  (filenames
	   (delete-dups (mapcar (lambda (m) (file-relative-name
					(buffer-file-name (marker-buffer m))
					(pay-project-root)))
				pay-test--test-markers))))
      (pay-test--run filenames nil line-nos))))

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
	  (setq inf-ruby-default-implementation "pay")))))

(define-minor-mode pay-test-mode
  "Minor mode for pay test files"
  ;; :lighter " paytest"
  :group 'pay-server
  (pay-mode))

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
