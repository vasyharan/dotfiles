# path
path=(~/go/bin
      ~/.fzf/bin
      ~/.cargo/bin
      ~/.pyenv/bin
      /usr/local/bin
      /usr/local/sbin
      /usr/local/opt/bison/bin
      $path)
export PATH

# envs
which rbenv 2>&1 >/dev/null && eval "$(rbenv init -)"

export PYENV_ROOT="/Users/haran/.pyenv"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

export GOPATH="$HOME/go"
