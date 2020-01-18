# PS1
prompt() {
	local exitcode="$?"

#	local black="\[\e[0;30m\]"
	local red="\[\e[0;31m\]"
#	local green="\[\e[0;32m\]"
#	local yellow="\[\e[0;33m\]"
#	local blue="\[\e[0;34m\]"
#	local purple="\[\e[0;35m\]"
#	local cyan="\[\e[0;36m\]"
	local white="\[\e[0;37;1m\]"
#	local orange="\[\e[0;91m\]"
#
#	local bold_black="\[\e[30;1m\]"
#	local bold_red="\[\e[31;1m\]"
#	local bold_green="\[\e[32;1m\]"
#	local bold_yellow="\[\e[33;1m\]"
#	local bold_blue="\[\e[34;1m\]"
#	local bold_purple="\[\e[35;1m\]"
#	local bold_cyan="\[\e[36;1m\]"
#	local bold_white="\[\e[37;1m\]"
#	local bold_orange="\[\e[91;1m\]"
#
#	local underline_black="\[\e[30;4m\]"
#	local underline_red="\[\e[31;4m\]"
#	local underline_green="\[\e[32;4m\]"
#	local underline_yellow="\[\e[33;4m\]"
#	local underline_blue="\[\e[34;4m\]"
#	local underline_purple="\[\e[35;4m\]"
#	local underline_cyan="\[\e[36;4m\]"
#	local underline_white="\[\e[37;4m\]"
#	local underline_orange="\[\e[91;4m\]"
#
#	local background_black="\[\e[40m\]"
#	local background_red="\[\e[41m\]"
#	local background_green="\[\e[42m\]"
#	local background_yellow="\[\e[43m\]"
#	local background_blue="\[\e[44m\]"
#	local background_purple="\[\e[45m\]"
#	local background_cyan="\[\e[46m\]"
#	local background_white="\[\e[47;1m\]"
#	local background_orange="\[\e[101m\]"

	local normal="\[\e[0m\]"
	local reset_color="\[\e[39m\]"

	local TITLEBAR
	local ps_color
	local ps_exitcode

	case "$TERM" in
		xterm*|rxvt-*|st-*|screen)
			TITLEBAR="\[\033]0;\w\007\]"
			;;
		*)
			TITLEBAR=""
			;;
	esac

	ps_color="$white"
	ps_exitcode=""
	#                                  148 = ^Z                      130 = ^C
	if [[ "$exitcode" != "0" ]] && [[ "$exitcode" != "148" ]] && [[ "$exitcode" != "130" ]]; then
		ps_color="$red"
		ps_exitcode="[\$?]"
	fi

	if [[ "$UID" -eq 0 ]] || [[ "${LANG: -6}" != ".UTF-8" ]]; then
		export PS1="${TITLEBAR}${ps_color}[\w]\\\$${normal} "
	else
		PS1=""
		__git_ps1 "" "" "%s"
		PS1="${TITLEBAR}${ps_color}┌─${ps_exitcode}[\w]${PS1:+" ["}${PS1}${PS1:+"${ps_color}]"}${normal}
${ps_color}└─[\\\$]${normal} "
		PS2="└─[\\\$] "
		export PS1 PS2
	fi
}

#dirs() {
#	local d i
#	i=0
#	for d in "${DIRSTACK[@]::10}"; do
#		echo " $i $d"
#		i=$(($i+1))
#	done
#}

# Stores cd in directory stack
cd() {
	local DIR
	local EXITCODE=0
	local d
	local i
	local newdirs
	declare -A dups
	
	if [[ $# -eq 0 ]]; then
		DIR="$HOME"
	else
		DIR="$1"
	fi
	
	if [[ x"$DIR"x == x"-"x ]]; then
		DIR="+1"
	fi

	if [[ ! $DIR =~ ^\+[0-9]+ ]]; then
		if [[ ! -e "$DIR" ]]; then
			echo "bash: cd: $DIR: No such file or directory"
			EXITCODE=1
		elif [[ ! -d "$DIR" ]]; then
			echo "bash: cd: $DIR: Not a directory"
			EXITCODE=1
		fi
	fi
	
	if (($EXITCODE)); then
		return $EXITCODE
	fi

	builtin pushd "$DIR" > /dev/null 2>&1
	
	dups["$PWD"]=1
	for d in "${DIRSTACK[@]:1:9}"; do
		if [[ -z "${dups["$d"]}" ]]; then
			newdirs+=("$d")
			dups["$d"]=1
		fi
	done
	builtin dirs -c
	for ((i="${#newdirs[@]}"-1; i>=0; i--)); do
		builtin pushd -n "${newdirs[$i]}" >/dev/null 2>&1
	done
}

# ls recursively
lr() {
	if [[ -n "$1" ]]; then
		find "$1" -mindepth 1 -exec ls --color=auto -d {} +
	else
		find -mindepth 1 -exec ls --color=auto -d {} +
	fi
}

getcrx() {
	echo 'https://clients2.google.com/service/update2/crx?response=redirect&x=id%3D'"$1"'%26uc&prodversion=32'
}

(
	if [[ -f /etc/os-release ]]; then
		. /etc/os-release
		[[ $NAME == Slackware ]] && exit 0 || exit 1
	else
		exit 1
	fi
) && {
	package_name() {
		local NAME="${1##*/}"
		NAME="${NAME%-*}"
		NAME="${NAME%-*}"
		NAME="${NAME%-*}"
		echo "$NAME"
	}
}

homestead() {
	( cd ~/Homestead && vagrant $* )
}

# vim:ft=sh
