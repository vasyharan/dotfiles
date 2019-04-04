;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

(map! :n "C-h" #'tmux/windmove-left
      :n "C-j" #'tmux/windmove-down
      :n "C-k" #'tmux/windmove-up
      :n "C-l" #'tmux/windmove-right

      :nv "M-a" #'evil-numbers/dec-at-pt)
