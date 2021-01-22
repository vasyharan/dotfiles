# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

[ -f ~/.zshenv ] && source ~/.zshenv
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
typeset -aU path

# {{{ history
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
# }}}
# {{{ dir stacks
DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent # pushdtohome
# }}}
# {{{ aliases
alias dot="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias passg="git --git-dir=$HOME/.password-store/personal/.git"
# alias vim=nvim
# alias vimdiff='nvim -d'
alias g=git
alias tf=terraform
alias ls='ls -G'
alias dircolors=gdircolors
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias dh='dirs -v'
alias cd=pushd
# alias emacs='env TERM=xterm-24bits emacs'
# alias emacsclient='env TERM=xterm-24bits emacsclient'
# alias e='emacsclient -t $@'
# alias ec='emacsclient -nc $@'
alias todos="rg -i 'todo[^:]*haran'"
alias sbt='env TERM=xterm-color sbt'
alias tmux='tmux -2'
alias lg=lazygit
alias colours='curl -s https://gist.githubusercontent.com/HaleTom/89ffe32783f89f403bba96bd7bcd1263/raw/ | bash'
alias k='kubectl'
# }}}
# {{{ func
function mkcd() { mkdir -p $1 && cd $1 }
# }}}
# {{{ prompt
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
# }}}

# editor {{{
if command -v nvim &>/dev/null; then
  alias vi=nvim
  alias vim=nvim
  export EDITOR=nvim
else
  export EDITOR=vim
  alias nvim=vim
fi
# }}}
# {{{ ripgrep
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
# }}}

# {{{ fzf
export FZF_CTRL_T_COMMAND="rg --files"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--bind 'change:top,ctrl-s:toggle' --bind 'ctrl-f:preview-page-down,ctrl-b:preview-page-up'"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# }}}
# {{{ fzf-tab
# zstyle ':completion:*:descriptions' format '[%d]'
# FZF_TAB_SHOW_GROUP=brief
# FZF_TAB_COMMAND=(
#     fzf
#     --ansi   # Enable ANSI color support, necessary for showing groups
#     --expect='$continuous_trigger,$print_query,ctrl-e' # For continuous completion and print query
#     '--color=hl:$(( $#headers == 0 ? 108 : 255 ))'
#     --nth=2,3 --delimiter='\x00'  # Don't search prefix
#     --layout=reverse --height='${FZF_TMUX_HEIGHT:=40%}'
#     --tiebreak=begin -m --bind=change:top,tab:jump --cycle
#     '--query=$query'   # $query will be expanded to query string at runtime.
#     '--header-lines=$#headers' # $#headers will be expanded to lines of headers at runtime
#     --print-query
# )
# zstyle ':fzf-tab:*' no-group-color $'\033[93m'
# zstyle ':fzf-tab:*' command $FZF_TAB_COMMAND
# }}}
# {{{ fzf colors
_gen_fzf_colors() {
  if [ "$1" = "dark" ]; then
    # Solarized Dark color scheme for fzf
    export FZF_DEFAULT_OPTS="
      $FZF_DEFAULT_OPTS
      --color fg:#839496,bg:-1,fg+:#eee8d5,bg+:-1,hl:#6c71c4,hl+:#6c71c4
      --color info:#b58900,spinner:#b58900,prompt:#839496,pointer:#268bd2,marker:#268bd2
    "
  else
    # Solarized Light color scheme for fzf
    export FZF_DEFAULT_OPTS="
      $FZF_DEFAULT_OPTS
      --color fg:#839496,bg:-1,fg+:#586e75,bg+:-1,hl:#6c71c4,hl+:#6c71c4
      --color info:#b58900,spinner:#b58900,prompt:#839496,pointer:#268bd2,marker:#268bd2
    "
  fi
}

_gen_fzf_colors "$(cat ~/.theme.conf)"
# }}}
# {{{ git❤️fzf
is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf "$@" --border
}

gf() {
  is_in_git_repo || return
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git --no-pager diff --color=always --pretty=format:%b -- {-1} | sed 1,4d; cat {-1}) ' |
  cut -c4- | sed 's/.* -> //'
}

gb() {
  is_in_git_repo || return
  git branch -a --color=always -vv | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --reverse --preview-window down:80% \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1)' |
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

# {{{ minikube & docker
command -v minikube &>/dev/null && 
  command -v docker &>/dev/null &&
  function docker() {
    eval $(minikube -p minikube docker-env)
    unset -f docker
    command docker "$@"
  }
# }}}
# {{{ rbenv
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
# }}}
# {{{ nodenv
if command -v nodenv 1>/dev/null 2>&1; then
    eval "$(nodenv init -)"
fi

# command -v nodenv &>/dev/null &&
#   function nodenv() {
#     eval "$(command nodenv init -)"
#     nodenv "$@";
#   }

# command -v nodenv &>/dev/null &&
#   function yarn() {
#     eval "$(command nodenv init -)"
#     unset -f yarn
#     command yarn "$@";
#   }

# command -v nodenv &>/dev/null &&
#   function esy() {
#     eval "$(command nodenv init -)"
#     unset -f esy
#     command esy "$@";
#   }

# command -v nodenv &>/dev/null &&
#   function npm() {
#     eval "$(command nodenv init -)"
#     unset -f npm
#     command npm "$@";
#   }

# command -v nodenv &>/dev/null &&
#   function npx() {
#     eval "$(command nodenv init -)"
#     unset -f npx
#     command npx "$@";
#   }

# # for coc.vim
# command -v nodenv &>/dev/null &&
#   function nvim() {
#     eval "$(command nodenv init -)"
#     unset -f nvim
#     command nvim "$@";
#   }
# }}}
# {{{ pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export PIPENV_VENV_IN_PROJECT=1
export PYTHON_CONFIGURE_OPTS="--enable-shared"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  # eval "$(pyenv virtualenv-init -)"
fi

autoload -Uz add-zsh-hook
function autovenv() {
  local venvdir
  if [[ -z "$VIRTUAL_ENV" ]]; then
    if [[ -d ./.venv ]]; then
      source ./.venv/bin/activate
    fi
  else
    venvdir=$(dirname $VIRTUAL_ENV)
    if [[ "${PWD##$venvdir}" == "$PWD" ]]; then
      command -v deactivate &>/dev/null && deactivate
    fi
  fi
}

add-zsh-hook -D chpwd autovenv
add-zsh-hook chpwd autovenv
autovenv

# command -v pyenv &>/dev/null &&
#   function pyenv() {
#     eval "$(command pyenv init -)"
#     eval "$(command pyenv virtualenv-init -)"
#     pyenv "$@"
#   }

# command -v python &>/dev/null &&
#   function python() {
#     eval "$(command pyenv init -)"
#     eval "$(command pyenv virtualenv-init -)"
#     unset -f python
#     command python "$@"
#   }

# command -v pip &>/dev/null &&
#   function pip() {
#     eval "$(command pyenv init -)"
#     eval "$(command pyenv virtualenv-init -)"
#     unset -f pip
#     command pip "$@"
#   }

# command -v pip3 &>/dev/null &&
#   function pip3() {
#     eval "$(command pyenv init -)"
#     eval "$(command pyenv virtualenv-init -)"
#     unset -f pip3
#     command pip3 "$@"
#   }

# # command -v pipenv &>/dev/null &&
#   function pipenv() {
#     eval "$(command pyenv init -)"
#     eval "$(command pyenv virtualenv-init -)"
#     unset -f pipenv
#     command pipenv "$@"
#   }
# }}}
# sdkman {{{
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/haran/.sdkman"
[[ -s "/Users/haran/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/haran/.sdkman/bin/sdkman-init.sh"
# }}}
# {{{ Intellij
if [[ "$TERMINAL_EMULATOR" == "JetBrains-JediTerm" ]]; then
  bindkey "∫" backward-word # Option-b
  bindkey "ƒ" forward-word  # Option-f
  bindkey "∂" delete-word   # Option-d
fi
# }}}

# {{{ fzy
# bindkey '\ec' fzy-cd-widget
# bindkey '^T'  fzy-file-widget
# bindkey '^R'  fzy-history-widget
# bindkey '^P'  fzy-proc-widget
# zstyle :fzy:file command rg --files
# zstyle :fzy:tmux enabled yes
# }}}

# {{{ pass-store
PASSWORD_STORE_GENERATED_LENGTH=32
# }}}
# prompt themes {{{
function set-theme() {
  unlink $HOME/.config/nvim/background.vim 2>/dev/null
  ln -s $HOME/.config/nvim/background-$1.vim $HOME/.config/nvim/background.vim

  unlink $HOME/.tmux/theme.conf 2>/dev/null
  ln -s $HOME/.tmux/theme-$1.conf $HOME/.tmux/theme.conf
  tmux source-file $HOME/.tmux/theme.conf
  tmux source-file $HOME/.tmux.conf

  echo "$1" > $HOME/.theme.conf
  _gen_fzf_colors "$1"
  echo -e "\033]50;SetProfile=$1\a"
}

# }}}
# {{{ zsh autosuggestions
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=14'
# }}}

# {{{ zgen
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  zgen load "mafredri/zsh-async"
  # zgen load "dfurnes/purer" "pure.zsh"
  # zgen load "sindresorhus/pure" "pure.zsh"
  # zgen load "reobin/typewritten" "typewritten" "main"
  zgen load "romkatv/powerlevel10k" "powerlevel10k"
  zgen load "agkozak/zsh-z"
  zgen load "zsh-users/zsh-autosuggestions"
  zgen load "zsh-users/zsh-history-substring-search"
  zgen load "joel-porquet/zsh-dircolors-solarized.git"
  # zgen load "Aloxaf/fzf-tab"
  # zgen load "matthieusb/zsh-sdkman"
  # zgen load "jackwish/bazel"
  # zgen load 'aperezdc/zsh-fzy'
  zgen save
fi
# }}}

# {{{ compinit
typeset -aU fpath
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit
autoload -Uz bashcompinit
# for dump in ~/.zcompdump(N.mh+24); do
  compinit
  bashcompinit
# done
compinit -C
bashcompinit -C

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
# complete -F __start_kubectl k
# }}}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
