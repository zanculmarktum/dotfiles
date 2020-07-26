alias ls='ls -hFT 0 --color=auto --group-directories-first'
alias l='ls -1'
alias ll='ls -l'
alias la='ls -A'
alias lt='ls -tr'
alias lz='ls -sh'

# Safety net
alias cp='cp -ia'
alias mv='mv -i'
alias rm='rm -i'
alias ln='ln -i'

# Open files according to .desktop file
#alias open='xdg-open'

# Grep
alias grep='grep --color=auto'

# Clear out everything, with no turning back (S-PageUp|S-PageDown)
alias clear='printf '"'"'\x1bc'"'"

# Haha, no more `sudo /sbin/poweroff|reboot' shit
if [[ $UID -ne 0 ]]; then
	if command -v systemctl >/dev/null 2>&1; then
		alias poweroff='systemctl poweroff'
		alias reboot='systemctl reboot'
		alias suspend='systemctl suspend'
		alias hibernate='systemctl hibernate'
	else
		alias poweroff='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop'
		alias reboot='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart'
		alias suspend='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Suspend  boolean:true'
		alias hibernate='dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Hibernate  boolean:true'
	fi
fi

# `gvim <file>' now uses previous gvim instance, if any
alias gvim='gvim --remote-tab-silent'
# Safety net
#alias view='vim -Rm'
alias view='vim -R'

# ed
alias ed='ed -p\*'

# (en|de)code url
alias urlencode='php -r '"'"'echo urlencode(fgets(STDIN));'"' <<<"
alias urldecode='php -r '"'"'echo urldecode(fgets(STDIN));'"' <<<"

# Yanks PWD
alias ywd='printf "$PWD" | xclip -sel c'

# Appends line number
#alias dirs='dirs -v | head -n 10'
alias dirs='dirs -v'

# Human-readable
alias df='df -h'
alias du='du -h'
alias free='free -h'

# Security, maybe?
alias firefox='firefox -offline'

# Removes duplicate lines
alias clean='awk '"'"'{if(!_[$0]++){print}}'"'"

# screenfetch without nonsense
alias screenfetch='screenfetch -d '"'"'-host;-res;-cpu;-gpu'"'"

alias dquilt='quilt --quiltrc=${HOME}/.quiltrc-dpkg'
