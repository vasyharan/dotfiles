(require 'load-relative)
 
(defun comment-or-uncomment-line-or-region()
  "Toggle comment for region if selected, or line if not."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun define-keys (keymap key def &rest bindings)
  "See `define-key` for KEYMAP, KEY, DEF; BINDINGS are more KEY/DEF pairs."
  ;; (declare (indent defun))
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defun global-set-keys (key def &rest bindings)
  "See `global-set-key' for KEY, DEF; BINDINGS are more KEY/DEF pairs."
  ;; (declare (indent defun))
  (while key
    (global-set-key key def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defun global-unset-keys (key &rest keys)
  "See `global-unset-key` for KEY; KEYS are more keys."
  ;; (declare (indent defun))
  (while key
    (global-unset-key key)
    (setq key (pop keys))))

(defconst user-config-directory
  (expand-file-name (concat user-emacs-directory "config/"))
  "Storage area for persistent files.")

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat ".../" output)))
       output))

(defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))

(defun term-send-up    ()
  "Send up arrow in term mode."
  (interactive) (term-send-raw-string "\eOA"))

(defun term-send-down  ()
  "Send down arrow in term mode."
  (interactive) (term-send-raw-string "\eOB"))

(defun term-send-right ()
  "Send right arrow in term mode."
  (interactive) (term-send-raw-string "\eOC"))

(defun term-send-left  ()
  "Send left arrow in term mode."
  (interactive) (term-send-raw-string "\eOD"))

(defun comint-goto-end-and-insert ()
  (interactive)
  (if (not (comint-after-pmark-p))
      (progn (comint-goto-process-mark)
             (evil-append-line nil))
    (evil-insert 1)))

(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))

  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(provide-me)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
