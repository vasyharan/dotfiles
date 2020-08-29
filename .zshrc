# [ -f ~/.zshenv ] && source ~/.zshenv
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

autoload -Uz compinit; compinit
autoload -Uz bashcompinit; bashcompinit
typeset -aU path

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
# alias vim=nvim
# alias vimdiff='nvim -d'
alias g=git
alias ls='ls -G'
alias dircolors=gdircolors
alias ll='ls -l'
alias dh='dirs -v'
alias cd=pushd
# alias emacs='env TERM=xterm-24bits emacs'
# alias emacsclient='env TERM=xterm-24bits emacsclient'
# alias e='emacsclient -t $@'
# alias ec='emacsclient -nc $@'
alias sbt='env TERM=xterm-color sbt'
alias tmux='tmux -2'
alias lg=lazygit
alias colours='curl -s https://gist.githubusercontent.com/HaleTom/89ffe32783f89f403bba96bd7bcd1263/raw/ | bash'

command -v nvim &>/dev/null && alias vim=nvim

function mkcd() {
    mkdir -p $1 && cd $1
}

# export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=10"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
# export EDITOR='env TERM=xterm-24bits emacsclient -nw'
export EDITOR=nvim

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

command -v ruby &>/dev/null &&
  function ruby() {
    eval "$(command rbenv init -)"
    unset -f ruby
    command ruby "$@"
  }

command -v bundle &>/dev/null &&
  function bundle() {
    eval "$(command rbenv init -)"
    unset -f bundle
    command bundle "$@"
  }

command -v nodenv &>/dev/null &&
  function nodenv() {
    eval "$(command nodenv init -)"
    nodenv "$@";
  }

command -v yarn &>/dev/null &&
  function yarn() {
    eval "$(command nodenv init -)"
    unset -f yarn
    command yarn "$@";
  }

command -v esy &>/dev/null &&
  function esy() {
    eval "$(command nodenv init -)"
    unset -f esy
    command esy "$@";
  }

command -v npm &>/dev/null &&
  function npm() {
    eval "$(command nodenv init -)"
    unset -f npm
    command npm "$@";
  }

command -v npx &>/dev/null &&
  function npx() {
    eval "$(command nodenv init -)"
    unset -f npx
    command npx "$@";
  }

# for coc.vim
command -v nvim &>/dev/null &&
  function nvim() {
    eval "$(command nodenv init -)"
    unset -f nvim
    command nvim "$@";
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

command -v python &>/dev/null &&
  function python() {
    eval "$(command pyenv init -)"
    eval "$(command pyenv virtualenv-init -)"
    unset -f python
    command python "$@"
  }

command -v pip &>/dev/null &&
  function pip() {
    eval "$(command pyenv init -)"
    eval "$(command pyenv virtualenv-init -)"
    unset -f pip
    command pip "$@"
  }

# command -v pipenv &>/dev/null &&
  function pipenv() {
    eval "$(command pyenv init -)"
    eval "$(command pyenv virtualenv-init -)"
    unset -f pipenv
    command pipenv "$@"
  }

function set-theme() {
  unlink $HOME/.config/nvim/background.vim 2>/dev/null
  ln -s $HOME/.config/nvim/background-$1.vim $HOME/.config/nvim/background.vim
  unlink $HOME/.tmux/theme.conf 2>/dev/null
  ln -s $HOME/.tmux/theme-$1.conf $HOME/.tmux/theme.conf
  tmux source-file $HOME/.tmux/theme.conf
  tmux source-file $HOME/.tmux.conf
}

PASSWORD_STORE_GENERATED_LENGTH=32

# pure prompt
PURE_GIT_PULL=0
VIRTUAL_ENV_DISABLE_PROMPT=1

# zsh autosuggestions
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=10'

# Intellij
if [[ "$TERMINAL_EMULATOR" == "JetBrains-JediTerm" ]]; then
  bindkey "∫" backward-word # Option-b
  bindkey "ƒ" forward-word  # Option-f
  bindkey "∂" delete-word   # Option-d
fi

# zgen
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  zgen load "mafredri/zsh-async"
  zgen load "dfurnes/purer" "pure.zsh"
  # zgen load "sindresorhus/pure" "pure.zsh"
  # zgen load "reobin/typewritten" "typewritten.zsh-theme"
  # zgen load "rupa/z" "z.sh"
  zgen load "agkozak/zsh-z"
  zgen load "zsh-users/zsh-autosuggestions"
  zgen load "zsh-users/zsh-history-substring-search"
  zgen load "joel-porquet/zsh-dircolors-solarized.git"
  zgen load "Aloxaf/fzf-tab"
  zgen load "jackwish/bazel"
  zgen save
fi
# zgen

# zstyle ':completion:*' menu select

# {{{ fzf-tab
# disable sort when completing options of any command
zstyle ':completion:complete:*:options' sort false

# use input as query string when completing zlua
zstyle ':fzf-tab:complete:_zlua:*' query-string input

# (experimental, may change in the future)
# some boilerplate code to define the variable `extract` which will be used later
# please remember to copy them

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm,cmd -w -w"
zstyle ':fzf-tab:complete:kill:argument-rest' extra-opts --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap

# give a preview of directory by exa when completing cd
zstyle ':fzf-tab:complete:cd:*' extra-opts --preview=$extract'exa -1 --color=always $realpath'

FZF_TAB_COMMAND=(
    fzf
    --ansi   # Enable ANSI color support, necessary for showing groups
    --expect='$continuous_trigger,$print_query,ctrl-f,tab' # For continuous completion and print query
    '--color=hl:$(( $#headers == 0 ? 108 : 255 ))'
    --nth=2,3 --delimiter='\x00'  # Don't search prefix
    --layout=reverse --height='${FZF_TMUX_HEIGHT:=75%}'
    --tiebreak=begin -m --bind=tab:down,btab:up,change:top,ctrl-space:toggle --cycle
    '--query=$query'   # $query will be expanded to query string at runtime.
    '--header-lines=$#headers' # $#headers will be expanded to lines of headers at runtime
    --print-query
)
zstyle ':fzf-tab:*' command $FZF_TAB_COMMAND
# }}}

# {{{ git❤️fzf
is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --height 50% "$@" --border
}

gf() {
  is_in_git_repo || return
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1}) | head -500' |
  cut -c4- | sed 's/.* -> //'
}

gb() {
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
  sed 's/^..//' | cut -d' ' -f1 |
  sed 's#^remotes/##'
}

gt() {
  is_in_git_repo || return
  git tag --sort -version:refname |
  fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {} | head -'$LINES
}

gh() {
  is_in_git_repo || return
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
  fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always | head -'$LINES |
  grep -o "[a-f0-9]\{7,\}"
}

gr() {
  is_in_git_repo || return
  git remote -v | awk '{print $1 "\t" $2}' | uniq |
  fzf-down --tac \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' |
  cut -d$'\t' -f1
}

join-lines() {
  local item
  while read item; do
    echo -n "${(q)item} "
  done
}

bind-git-helper() {
  local c
  for c in $@; do
    eval "fzf-g$c-widget() { local result=\$(g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-g$c-widget"
    eval "bindkey '^g^$c' fzf-g$c-widget"
  done
}
bind-git-helper f b t r h
unset -f bind-git-helper
# }}}
