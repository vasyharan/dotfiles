[ -f ~/.zshenv ] && source ~/.zshenv

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
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias vim=nvim
alias g=git
alias ll='ls -l'

alias emacs='env TERM=xterm-24bits emacs'
alias emacsclient='env TERM=xterm-24bits emacsclient'
# alias sbt='env TERM=xterm sbt'
alias sbt='env TERM=xterm-color sbt'

function e() {
  if [[ $PWD == "/Users/haran/stripe/pay-server" ]]; then
    emacsclient -t -s pay-server $*
  else
    emacsclient -t $*
  fi
}
function ec() {
  if [[ $PWD == "/Users/haran/stripe/pay-server" ]]; then
    emacsclient -nc -s pay-server $*
  else
    emacsclient -nc $*
  fi
}

function mkcd() {
    mkdir -p $1 && cd $1
}

export EDITOR='env TERM=xterm-24bits emacsclient -nw'

bindkey -e
bindkey '^[H' run-help
autoload -U select-word-style
select-word-style bash

export FZF_CTRL_T_COMMAND="rg --files"
export FZF_DEFAULT_COMMAND="rg --files"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/stripe/space-commander/bin/sc-aliases ] && \
    source ~/stripe/space-commander/bin/sc-aliases

# envs
which rbenv 2>&1 >/dev/null && eval "$(rbenv init -)"
which nodenv 2>&1 >/dev/null && eval "$(nodenv init -)"

export PYENV_ROOT="/Users/haran/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
if command -v pyenv 2>&1 >/dev/null; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# zplug
source ~/.zplug/init.zsh

zplug "zplug/zplug", hook-build:'zplug --self-manage'

# zplug "dracula/zsh", as:theme

PURE_GIT_PULL=0
VIRTUAL_ENV_DISABLE_PROMPT=1
zplug "mafredri/zsh-async", from:github
# zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
zplug "dfurnes/purer", use:pure.zsh, from:github, as:theme

zplug "rupa/z", use:z.sh

zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"

zplug load # --verbose

# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
