[ -f ~/.zshenv ] && source ~/.zshenv
export EDITOR='emacsclient -nw'

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
alias et='emacsclient -nw'
alias ec='emacsclient -nc'
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

bindkey -e
autoload -U select-word-style
select-word-style bash

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/stripe/space-commander/bin/sc-aliases ] && \
    source ~/stripe/space-commander/bin/sc-aliases

# envs
which rbenv 2>&1 >/dev/null && eval "$(rbenv init -)"

export PYENV_ROOT="/Users/haran/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
if command -v pyenv 2>&1 >/dev/null; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# zplug
source ~/.zplug/init.zsh

zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "dracula/zsh", as:theme
zplug "mafredri/zsh-async", from:github

PURE_GIT_PULL=0
VIRTUAL_ENV_DISABLE_PROMPT=1
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme

zplug "rupa/z", use:z.sh

zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"

zplug load # --verbose
