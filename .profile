[ -f /usr/share/defaults/etc/profile ] && source /usr/share/defaults/etc/profile
[ -f ~/.profile.local ] && source ~/.profile.local

# path
path=(~/.bin
      ~/go/bin
      ~/.fzf/bin
      ~/.cargo/bin
      ~/.pyenv/bin
      ~/.emacs.d/bin
      ~/.yarn/bin
      ~/.config/yarn/global/node_modules/.bin
      ~/.jenv/bin
      ~/.krew/bin
      /usr/local/bin
      /usr/local/sbin
      $path)
export PATH

export GOPATH="$HOME/go"

export PYENV_ROOT="$HOME/.pyenv"
eval "$(pyenv init --path)"

. "$HOME/.cargo/env"
