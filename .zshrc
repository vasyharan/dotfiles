# history
HISTFILE=~/.history
HISTSIZE=10000
SAVEHIST=10000
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

# aliases
alias ll='ls -l'
alias vim=nvim
alias g=git
alias ec='emacsclient -nw'
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

bindkey -e
autoload -U select-word-style
select-word-style bash

[ -f ~/.zshenv ] && source ~/.zshenv
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/stripe/space-commander/bin/sc-aliases ] && \
    source ~/stripe/space-commander/bin/sc-aliases


# zplug
source ~/.zplug/init.zsh

zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "dracula/zsh", as:theme

zplug "mafredri/zsh-async", from:github

PURE_GIT_PULL=0
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme

zplug "rupa/z", use:z.sh
# zplug "aperezdc/zsh-fzy

zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"

zplug load # --verbose
