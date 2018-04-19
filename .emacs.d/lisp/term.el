;; term.el -- Term utilities.
;;; Commentary:
;;; Code:

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

(provide 'term)
;;; term.el ends here
