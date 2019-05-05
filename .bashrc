. ~/.bash_functions
. ~/.bash_aliases
. ~/.bash_miscs

# +========================================================+
# | Environment variables                                  |
# +========================================================+
export LANG="C.UTF-8"

# +========================================================+
# | stty                                                   |
# +========================================================+
# Disable C-S locks
stty -ixon

# +========================================================+
# | Bash Completion                                        |
# +========================================================+
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

