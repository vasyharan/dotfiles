[ -f /usr/share/defaults/etc/profile ] && source /usr/share/defaults/etc/profile

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

# BEGIN STRIPE NODE CONFIG
#      To undo the following behavior, comment it out, dont delete it;
#      'pay-server/scripts/frontend/install_node_modules' will just add it again.
#      Ask in #dashboard-platform if you have questions.
export PATH="./node_modules/.bin:$PATH"
# END STRIPE NODE CONFIG
