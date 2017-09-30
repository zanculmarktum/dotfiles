. ~/.bash_functions
. ~/.bash_aliases
. ~/.bash_miscs

# +========================================================+
# | Environment variables                                  |
# +========================================================+
export LANG="en_US.UTF-8"
export PATH="$HOME/.local/bin:$PATH"
export MANPATH="$HOME/.local/man:$MANPATH"
export TERMINAL="urxvt"
export EDITOR="vim"
#export LYNX_CFG="~/.lynx.cfg"
#export LYNX_LSS="~/.lynx.lss"
#export PYTHONDOCS="/usr/share/doc/python2/html/"
#export XDG_CONFIG_HOME="$HOME/.config"
export PYTHONSTARTUP=~/.pystartup
export QT_QPA_PLATFORMTHEME=qt5ct
#export QT_QPA_PLATFORMTHEME=gtk2

# +========================================================+
# | History                                                |
# +========================================================+
export HISTSIZE=100000
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=erasedups:ignoreboth

shopt -s histappend                      # append to history, don't overwrite it
if [[ -z "$PROMPT_COMMAND" ]]; then
	export PROMPT_COMMAND="prompt; history -a; history -c; history -r"
else
	export PROMPT_COMMAND="prompt; history -a; history -c; history -r; $PROMPT_COMMAND"
fi

# +========================================================+
# | Colors                                                 |
# +========================================================+
export LESS="$LESS -r"

export LESS_TERMCAP_mb=$'\e[01;31m'      # Begins blinking.
export LESS_TERMCAP_md=$'\e[01;31m'      # Begins bold.
export LESS_TERMCAP_me=$'\e[0m'          # Ends mode.
export LESS_TERMCAP_se=$'\e[0m'          # Ends standout-mode.
export LESS_TERMCAP_so=$'\e[00;47;30m'   # Begins standout-mode.
export LESS_TERMCAP_ue=$'\e[0m'          # Ends underline.
export LESS_TERMCAP_us=$'\e[01;32m'      # Begins underline.

export GREP_COLORS="mt=37;45"

#export GIT_PAGER=""

# +========================================================+
# | Starts the X server                                    |
# +========================================================+
if [[ -t 0 ]] && [[ "$(tty)" == "/dev/tty1" ]] && [[ -z "$DISPLAY" ]] && [[ "$(id -u)" != "0" ]]; then
  exec startx ~/.xinitrc dwm
fi
