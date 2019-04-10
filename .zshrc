[ -f ~/.zshenv ] && source ~/.zshenv
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

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

# dir stacks
DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent # pushdtohome

# aliases
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias vim=nvim
alias vimdiff='nvim -d'
alias g=git
alias ll='ls -l'
alias dh='dirs -v'
alias cd=pushd
alias emacs='env TERM=xterm-24bits emacs'
alias emacsclient='env TERM=xterm-24bits emacsclient'
alias sbt='env TERM=xterm-color sbt'
alias e='emacsclient -t $@'
alias ec='emacsclient -nc $@'
function mkcd() {
    mkdir -p $1 && cd $1
}

# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
export EDITOR='env TERM=xterm-24bits emacsclient -nw'

bindkey -e
# bindkey '^E' run-help
autoload -U select-word-style
select-word-style bash

fancy-ctrl-z () {
  emulate -LR zsh
  if [[ $#BUFFER -eq 0 ]]; then
    bg
    zle redisplay
  else
    zle push-input
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

export FZF_CTRL_T_COMMAND="rg --files"
export FZF_DEFAULT_COMMAND="rg --files"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/stripe/space-commander/bin/sc-aliases ] && \
    source ~/stripe/space-commander/bin/sc-aliases

# envs
command -v rbenv &>/dev/null &&
  function rbenv() {
    eval "$(command rbenv init -)"
    rbenv "$@"
  }

command -v nodenv &>/dev/null &&
  function nodenv() {
    eval "$(command nodenv init -)"
    nodenv "$@";
  }

export PYENV_ROOT="${HOME}/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export PYTHON_CONFIGURE_OPTS="--enable-shared"
command -v pyenv &>/dev/null &&
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
  # zgen load "dfurnes/purer" "pure.zsh"
  zgen load "sindresorhus/pure" "pure.zsh"
  zgen load "rupa/z" "z.sh"
  zgen load "zsh-users/zsh-autosuggestions"
  zgen load "zsh-users/zsh-history-substring-search"
  zgen save
fi
# zgen

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
