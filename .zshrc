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

DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent # pushdtohome
alias cd=pushd

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

# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
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
command -v rbenv 2>&1 >/dev/null &&
  function rbenv() {
    eval "$(command rbenv init -)"
    rbenv "$@"
  }

command -v nodenv 2>&1 >/dev/null &&
  function nodenv() {
    eval "$(command nodenv init -)"
    nodenv "$@";
  }

export PYENV_ROOT="${HOME}/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
command -v pyenv 2>&1 >/dev/null &&
  function pyenv() {
    eval "$(command pyenv init -)"
    eval "$(command pyenv virtualenv-init -)"
    pyenv "$@"
  }

# pure prompt
PURE_GIT_PULL=0
VIRTUAL_ENV_DISABLE_PROMPT=1

# zgen
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  zgen load "mafredri/zsh-async"
  zgen load "dfurnes/purer" "pure.zsh"
  zgen load "rupa/z" "z.sh"
  zgen load "zsh-users/zsh-autosuggestions"
  zgen load "zsh-users/zsh-history-substring-search"
  zgen save
fi
# zgen

# zplug {{{
# source ~/.zplug/init.zsh
# zplug "zplug/zplug", hook-build:'zplug --self-manage'

# zplug "mafredri/zsh-async", from:github
# zplug "dfurnes/purer", use:pure.zsh, from:github, as:theme
# zplug "rupa/z", use:z.sh
# zplug "zsh-users/zsh-autosuggestions"
# zplug "zsh-users/zsh-history-substring-search"
# zplug load # --verbose
# }}}
