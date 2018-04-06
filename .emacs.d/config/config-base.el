;; config-base.el -- My Emacs base settings.
;;; Commentary:
;;; Code:




(setq display-time-default-load-average nil
      display-time-24hr-format t)

(show-paren-mode)
(column-number-mode)
(winner-mode)

(use-package ace-window
  :ensure t
  :defer 1
  :custom-face
  (aw-leading-char-face ((t (:foreground "brightblue" :weight 'bold))))
  (aw-mode-line-face ((t (:foreground "green"))))
  :config
  ;; (set-face-attribute 'aw-leading-char-face nil
  ;; 		      :foreground "brightblue" :weight 'bold)
  ;; (set-face-attribute 'aw-mode-line-face nil
  ;; 		      :inherit 'mode-line-buffer-id :foreground "green")

  (defhydra hydra-window-size (:color red)
    "Windows size"
    ("h" shrink-window-horizontally "shrink ↔")
    ("j" shrink-window "shrink ↕")
    ("k" enlarge-window "enlarge ↕")
    ("l" enlarge-window-horizontally "enlarge ↔"))

  (defhydra hydra-window-scroll (:color red)
    "Scroll other window"
    ("k" scroll-other-window-down "scroll up")
    ("j" scroll-other-window "scroll down"))

  (defhydra hydra-window-winner (:color red)
    "Scroll other window"
    ("u" winner-undo "undo")
    ("U" winner-redo "redo"))

  (setq aw-keys   '(?j ?k ?l ?\;)
	aw-dispatch-always t
	aw-dispatch-alist
	'((?c evil-window-delete)
	  (?C aw-delete-window			"Delete window")
	  (?m aw-swap-window			"Swap window")
	  (?M aw-move-window			"Move window")
	  (?n aw-flip-window			"Flip window")
	  (?_ evil-window-split			"Split window")
	  (?| evil-window-vsplit		"Split Horizontally")
	  (?s aw-split-window-fair		"Split Vertically")
	  (?o delete-other-windows)
	  (?O delete-other-windows		"Maximize window")
	  (?= balance-windows)
	  (?w hydra-window-size/body)
	  (?  hydra-window-scroll/body)
	  (?b aw-switch-buffer-in-window	"Select buffer")
	  (?u hydra-window-winner/winner-undo)
	  (?U hydra-window-winner/winner-redo))))


(provide 'config-base)
;;; config-base.el ends here
