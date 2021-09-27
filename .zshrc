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
setopt histignorespace
setopt share_history
setopt interactivecomments
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
# {{{ funcs
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
# {{{ less
export LESS=-j2g
# }}
# {{{ ripgrep
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
# }}}

# {{{ golang
export GO111MODULE=on
# }}}

# {{{ fzf
export FZF_CTRL_T_COMMAND="rg --files"
export FZF_DEFAULT_COMMAND="rg --files"
export FZF_DEFAULT_OPTS="--bind 'change:top,ctrl-s:toggle' --bind 'ctrl-f:preview-page-down,ctrl-b:preview-page-up'"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# }}}
# {{{ fzf-tab
zstyle ':fzf-tab:*' fzf-bindings 'ctrl-e:accept'
# zstyle ":completion:*:git-checkout:*" sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:(cd|pushd):*' fzf-preview \
  'exa -1 --color=always $realpath'
# }}}
# {{{ git❤️fzf
# https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
is_in_git_repo() {
  git rev-parse HEAD > /dev/null 2>&1
}

fzf-down() {
  fzf --min-height 20 --border --bind ctrl-/:toggle-preview "$@"
  # fzf "$@" --border
}

_gf() {
  is_in_git_repo || return
  git -c color.status=always status --short |
  fzf-down -m --ansi --nth 2..,.. \
    --preview '(git diff --color=always -- {-1} | sed 1,4d; cat {-1})' |
  cut -c4- | sed 's/.* -> //'
}

_gb() {
  is_in_git_repo || return
  git branch -a --color=always | grep -v '/HEAD\s' | sort |
  fzf-down --ansi --multi --tac --preview-window right:70% \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1)' |
  sed 's/^..//' | cut -d' ' -f1 |
  sed 's#^remotes/##'
}

_gt() {
  is_in_git_repo || return
  git tag --sort -version:refname |
  fzf-down --multi --preview-window right:70% \
    --preview 'git show --color=always {}'
}

_gh() {
  is_in_git_repo || return
  git log --date=short --format="%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)" --graph --color=always |
  fzf-down --ansi --no-sort --reverse --multi --bind 'ctrl-s:toggle-sort' \
    --header 'Press CTRL-S to toggle sort' \
    --preview 'grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git show --color=always' |
  grep -o "[a-f0-9]\{7,\}"
}

_gr() {
  is_in_git_repo || return
  git remote -v | awk '{print $1 "\t" $2}' | uniq |
  fzf-down --tac \
    --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" {1}' |
  cut -d$'\t' -f1
}

_gs() {
  is_in_git_repo || return
  git stash list | fzf-down --reverse -d: --preview 'git show --color=always {1}' |
  cut -d: -f1
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
    eval "fzf-g$c-widget() { local result=\$(_g$c | join-lines); zle reset-prompt; LBUFFER+=\$result }"
    eval "zle -N fzf-g$c-widget"
    eval "bindkey '^g^$c' fzf-g$c-widget"
  done
}
bind-git-helper f b t r h s
unset -f bind-git-helper
# }}}

# {{{ zsh-autosuggestions
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(
  end-of-line
  vi-forward-char
  vi-end-of-line
  vi-add-eol
)
# }}}
# {{{ minikube & docker
# command -v minikube &>/dev/null && 
#   command -v docker &>/dev/null &&
#   function docker() {
#     minikube status >/dev/null && eval $(minikube -p minikube docker-env)
#     unset -f docker
#     command docker "$@"
#   }
# }}}
# {{{ rbenv
command -v rbenv &>/dev/null &&
  function rbenv() {
    eval "$(command rbenv init -)"
    rbenv "$@"
  }
# }}}
# {{{ jenv
if command -v jenv 1>/dev/null 2>&1; then
    eval "$(jenv init -)"
fi
# }}}
# {{{ nodenv
command -v nodenv &>/dev/null &&
  function nodenv() {
    eval "$(command nodenv init -)"
    nodenv "$@";
  }

command -v nvim &>/dev/null &&
  function nvim() {
    eval "$(command nodenv init -)"
    unset -f nvim
    command nvim "$@"
  }
# }}}
# {{{ pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export PIPENV_VENV_IN_PROJECT=1
export PYTHON_CONFIGURE_OPTS="--enable-shared"

command -v pyenv &>/dev/null &&
  function pyenv() {
    eval "$(command pyenv init -)"
    eval "$(command pyenv virtualenv-init -)"
    pyenv "$@"
  }
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
  # _gen_fzf_colors "$1"
  echo -e "\033]50;SetProfile=$1\a"
}

# }}}
# {{{ zsh autosuggestions
ZSH_AUTOSUGGEST_USE_ASYNC=1
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=14'
# }}}
# {{{ autoenv
export AUTOENV_FILE_LEAVE=".autoenv.zsh"
export AUTOENV_HANDLE_LEAVE=1
# }}}
# {{{ zgen
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  zgen load "mafredri/zsh-async"
  zgen load "romkatv/powerlevel10k" "powerlevel10k"
  zgen load "agkozak/zsh-z"
  zgen load "zsh-users/zsh-autosuggestions"
  zgen load "zsh-users/zsh-history-substring-search"
  zgen load "joel-porquet/zsh-dircolors-solarized.git"
  zgen load "zdharma/fast-syntax-highlighting"
  zgen load "Aloxaf/fzf-tab"
  zgen load "Tarrasch/zsh-autoenv"
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
# autoload -Uz bashcompinit
# for dump in ~/.zcompdump(N.mh+24); do
compinit
# bashcompinit
# done
# compinit -C
# bashcompinit -C

# zstyle ':completion:*' use-cache on
# zstyle ':completion:*' cache-path ~/.zsh/cache
# complete -F __start_kubectl k
# }}}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
