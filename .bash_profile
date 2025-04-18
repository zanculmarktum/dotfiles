. ~/.bash_functions
. ~/.bash_aliases
. ~/.bash_miscs

# +========================================================+
# | Environment variables                                  |
# +========================================================+
#export LANG="C.UTF-8"
export PATH="$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.node_modules/bin:$HOME/.config/composer/vendor/bin${PATH:+":$PATH"}"
export MANPATH="$HOME/.local/man:${MANPATH:+"$MANPATH"}"
export EDITOR="emacsclient -nw"
export BROWSER="chromium"
#export LYNX_CFG="~/.lynx.cfg"
#export LYNX_LSS="~/.lynx.lss"
#export PYTHONDOCS="/usr/share/doc/python2/html/"
#export XDG_CONFIG_HOME="$HOME/.config"
export PYTHONSTARTUP=~/.pystartup
export QT_QPA_PLATFORMTHEME=qt5ct
#export QT_QPA_PLATFORMTHEME=gtk2
if command -v ruby >/dev/null 2>&1; then
	export GEM_HOME=$(ruby -e 'print Gem.user_dir')
fi
export DEBEMAIL="zanculmarktum@gmail.com"
export DEBFULLNAME="Azure Zanculmarktum"
export npm_config_prefix=~/.node_modules
export NODE_PATH="$HOME/.node_modules/lib/node_modules${NODE_PATH:+":$NODE_PATH"}"
export NODE_REPL_MODE=strict
export NODE_REPL_HISTORY_SIZE=100000
export GIT_PS1_SHOWCOLORHINTS=true \
	GIT_PS1_SHOWDIRTYSTATE=true \
	GIT_PS1_SHOWUNTRACKEDFILES=true

# +========================================================+
# | History                                                |
# +========================================================+
export HISTSIZE=100000
export HISTFILESIZE=$HISTSIZE
export HISTCONTROL=erasedups:ignoreboth

shopt -s histappend                      # append to history, don't overwrite it
export PROMPT_COMMAND="prompt; history -a; history -c; history -r${PROMPT_COMMAND:+"; "}$PROMPT_COMMAND"
unset PROMPT_DIRTRIM

# +========================================================+
# | Colors                                                 |
# +========================================================+
export GROFF_NO_SGR=1 # returns the colors in groff 1.23.0 and above

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
    exec startx ~/.xinitrc xmonad -- -keeptty 2>~/.xsession-errors
fi
