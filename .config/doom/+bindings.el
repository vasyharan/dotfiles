;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

(map! :n "C-h" #'tmux/windmove-left
      :n "C-j" #'tmux/windmove-down
      :n "C-k" #'tmux/windmove-up
      :n "C-l" #'tmux/windmove-right

      :nv "C-M-a" #'evil-numbers/dec-at-pt

      (:map evil-window-map
        "C-s" #'ace-swap-window)

      (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "C-j"   #'ivy-immediate-done))

      (:map comint-mode-map
        :mn  "i"      #'comint-goto-end-and-insert
        :mn  "a"      #'comint-goto-end-and-insert
        :i   "C-p"    #'comint-previous-input
        :i   "C-n"    #'comint-next-input
        :i   "<up>"   #'comint-previous-input
        :i   "<down>" #'comint-next-input)
      (:map inf-ruby-mode-map
        :mn  "i" #'comint-goto-end-and-insert
        :mn  "a" #'comint-goto-end-and-insert))

;; <leader>
(map! :leader
      :desc "Eval expression"       ":"    #'eval-expression
      :desc "M-x"                   ";"    #'execute-extended-command)

(map! :localleader
      :map pay-test-mode-map
      :prefix "t"
      "a" #'pay-test-buffer
      "s" #'pay-test-block)
