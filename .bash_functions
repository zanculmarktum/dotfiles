# PS1
function prompt {
    local exitcode="$?"

    #    local black="\[\e[0;30m\]"
    #    local red="\[\e[0;31m\]"
    #    local green="\[\e[0;32m\]"
    #    local yellow="\[\e[0;33m\]"
    #    local blue="\[\e[0;34m\]"
    #    local purple="\[\e[0;35m\]"
    #    local cyan="\[\e[0;36m\]"
    #    local white="\[\e[0;37m\]"
    #    local orange="\[\e[0;91m\]"
    #
    #    local bold_black="\[\e[30;1m\]"
    local bold_red="\[\e[31;1m\]"
    #    local bold_green="\[\e[32;1m\]"
    #    local bold_yellow="\[\e[33;1m\]"
    #    local bold_blue="\[\e[34;1m\]"
    #    local bold_purple="\[\e[35;1m\]"
    #    local bold_cyan="\[\e[36;1m\]"
    local bold_white="\[\e[37;1m\]"
    #    local bold_orange="\[\e[91;1m\]"
    #
    #    local underline_black="\[\e[30;4m\]"
    #    local underline_red="\[\e[31;4m\]"
    #    local underline_green="\[\e[32;4m\]"
    #    local underline_yellow="\[\e[33;4m\]"
    #    local underline_blue="\[\e[34;4m\]"
    #    local underline_purple="\[\e[35;4m\]"
    #    local underline_cyan="\[\e[36;4m\]"
    #    local underline_white="\[\e[37;4m\]"
    #    local underline_orange="\[\e[91;4m\]"
    #
    #    local background_black="\[\e[40m\]"
    #    local background_red="\[\e[41m\]"
    #    local background_green="\[\e[42m\]"
    #    local background_yellow="\[\e[43m\]"
    #    local background_blue="\[\e[44m\]"
    #    local background_purple="\[\e[45m\]"
    #    local background_cyan="\[\e[46m\]"
    #    local background_white="\[\e[47;1m\]"
    #    local background_orange="\[\e[101m\]"

    local normal="\[\e[0m\]"
    local reset_color="\[\e[39m\]"

    local TITLEBAR
    local ps_color
    local ps_exitcode

    case "$TERM" in
        xterm*|rxvt-*|st-*|alacritty|screen)
            TITLEBAR="\[\033]0;\w\007\]"
            ;;
        *)
            TITLEBAR=""
            ;;
    esac

    ps_color="$bold_white"
    ps_exitcode=""
    #                                  148 = ^Z                      130 = ^C
    if [[ "$exitcode" != "0" ]] && [[ "$exitcode" != "148" ]] && [[ "$exitcode" != "130" ]]; then
        ps_color="$bold_red"
        ps_exitcode="[\$?]"
    fi

    if [[ "$UID" -eq 0 ]] || [[ "${LANG: -6}" != ".UTF-8" ]]; then
        export PS1="${TITLEBAR}${ps_color}[\w]\\\$${normal} "
    else
        PS1=""
        #if command -v __git_ps1 >/dev/null 2>&1; then
        #    __git_ps1 "" "" "%s"
        #fi
        PS1="${TITLEBAR}${ps_color}┌─${ps_exitcode}[\w]${SSH_CLIENT:+" [ssh: $HOSTNAME]"}${PS1:+" [${PS1}${ps_color}]"}${normal}
${ps_color}└─[\\\$]${normal} "
        PS2="└─[\\\$] "
        export PS1 PS2
    fi
}

# Push cd'ed directories into stack
function cd {
    local dir _dirstack d i n found is_home

    dir="$1"

    if [[ "$dir" =~ ^\+([0-9]+)$ ]]; then
        n="${BASH_REMATCH[1]}"
        if [[ "$n" == "0" ]] || (( "${#DIRSTACK[@]}" <= "1" )) || (( "$n" >= "${#DIRSTACK[@]}" )); then
            return 0
        fi
        dir="${DIRSTACK[$n]}"
        popd -n "+$n" >/dev/null
    elif [[ "$dir" =~ ^-$ ]]; then
        if (( "${#DIRSTACK[@]}" <= "1" )); then
            return 0
        fi
        dir="${DIRSTACK[1]}"
        popd -n +1 >/dev/null
    else
        is_home=0

        if [[ ! "$dir" ]]; then
            dir="$HOME"
            is_home=1
        fi

        if [[ -d "$dir" ]]; then
            :
        elif [[ -f "$dir" ]]; then
            echo >&2 "bash: cd: $dir: Not a directory"
            return 1
        else
            echo >&2 "bash: cd: $dir: No such file or directory"
            return 1
        fi

        #if (( $is_home )) && (( "${#DIRSTACK[@]}" <= "1" )); then
        #    return 0
        #fi

        while [[ "$dir" =~ (.*)/$ ]]; do
            dir="${BASH_REMATCH[1]}"
        done

        dir=$(builtin cd "$dir" && pwd)

        if [[ "$dir" == "${DIRSTACK[0]}" ]]; then
            return 0
        fi

        _dirstack=("${DIRSTACK[@]}")
        i=1
        while (( "$i" < "${#_dirstack[@]}" )); do
            popd -n >/dev/null
            i=$(( $i + 1 ))
        done
        i=$(( "${#_dirstack[@]}" - 1 ))
        while (( "$i" > 0 )); do
            if [[ "${_dirstack[$i]}" != "$dir" ]]; then
                pushd -n "${_dirstack[$i]}" >/dev/null
            fi
            i=$(( $i - 1 ))
        done
    fi

    if (( "${#DIRSTACK[@]}" >= "10" )); then
        popd -n +9 >/dev/null
    fi

    pushd "$dir" >/dev/null 2>&1

    if [[ -d "$dir" ]]; then
        :
    elif [[ -f "$dir" ]]; then
        echo >&2 "bash: cd: $dir: Not a directory"
        return 1
    else
        echo >&2 "bash: cd: $dir: No such file or directory"
        return 1
    fi
}

# ls recursively
function lr {
    if [[ -n "$1" ]]; then
        find "$1" -mindepth 1 -exec ls --color=auto -d {} +
    else
        find . -mindepth 1 -exec ls --color=auto -d {} +
    fi
}

function getcrx {
    echo 'https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=85&x=id%3D'"$1"'%26uc'
}

(
    if [[ -f /etc/os-release ]]; then
        . /etc/os-release
        [[ $NAME == Slackware ]] && exit 0 || exit 1
    else
        exit 1
    fi
) && {
    function package_name {
        local NAME="${1##*/}"
        NAME="${NAME%-*}"
        NAME="${NAME%-*}"
        NAME="${NAME%-*}"
        echo "$NAME"
    }
}

function homestead {
    ( cd ~/Homestead && vagrant $* )
}

function urlencode {
    # urlencode <string>
    old_lc_collate=$LC_COLLATE
    LC_COLLATE=C

    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        case $c in
            [a-zA-Z0-9.~_-]) printf "$c" ;;
            *) printf '%%%02X' "'$c" ;;
        esac
    done
    printf '\n'

    LC_COLLATE=$old_lc_collate
}

function urldecode {
    # urldecode <string>

    local url_encoded="${1//+/ }"
    printf '%b\n' "${url_encoded//%/\\x}"
}

function ypath {
    printf "$(realpath -s "$1")" | xclip -sel c
}

function tmux-kill {
    for i in $(tmux ls -F '#S'); do
        tmux kill-session -t $i
    done
}

# Usage: echo -n 'λ' | unicode-hex
function unicode-hex {
    printf '%s' '\u'
    iconv -f UTF8 -t ISO-10646 | xxd -s 2 -ps
}

function e {
    local TMP;
    if [[ "$1" == "-" ]]; then
        TMP="$(mktemp /tmp/emacsstdinXXX)";
        cat >"$TMP";
        emacsclient -nw --eval "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"${TMP}\") (delete-file \"${TMP}\"))"
    else
        emacsclient -nw "$@"
    fi;
}

# vim:ft=sh
# Local Variables:
# mode: Shell-script
# End:
