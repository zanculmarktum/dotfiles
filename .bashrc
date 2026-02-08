. ~/.bash_functions
. ~/.bash_aliases
. ~/.bash_miscs

# +========================================================+
# | Environment variables                                  |
# +========================================================+
#export LANG="C.UTF-8"
export HISTSIZE=100000
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=erasedups:ignoreboth

shopt -s histappend                      # append to history, don't overwrite it
export PROMPT_COMMAND="prompt; history -a; history -c; history -r"
unset PROMPT_DIRTRIM

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

