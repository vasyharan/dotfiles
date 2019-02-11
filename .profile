[ -f /usr/share/defaults/etc/profile ] && source /usr/share/defaults/etc/profile
[ -f ~/.profile.local ] && source ~/.profile.local

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
export GOPATH="$HOME/go"
