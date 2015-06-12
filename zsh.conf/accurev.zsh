#!/bin/zsh

ACCUREV=$(whence accurev)
ECMERGE=$(whence ecmerge.sh)

alias ac-init="ac-login && ac-chws-loc"
alias ac-wspaces="$ACCUREV show streams | awk 'NR == 1 {print}; \$1 ~ /_sgillespie\$/ {print}' | column -t"
alias ac-release-streams="$ACCUREV show streams | awk '\$1 ~ /^Release_[0-9]+$/ {print}' | column -t"

# accurev/emacs convenience functions
export AC_MERGE_CLI="$ECMERGE %1 %2 %a %o"

function _ac-_ {
    alias | awk 'BEGIN {FS="[ =]"}; $2 ~ /^ac-/ {print $2}' 
    compgen -A function | egrep '^ac-'
}

function ac-_ {
    _ac-_ | sort
}

function ac-chws-loc {
    _ac-chws-loc | column -s ":" -t
}

function _ac-chws-loc {
    workspaces=$(accurev show wspaces | awk 'NR > 5 {print $1}')
    refs=$(accurev show refs | awk '$1 ~ /_'$USER'$/ {print $1}')

    for ws in ${=workspaces}; do
	echo -n "${ws}: "
	accurev chws -w $ws -m $(hostname)
    done

    for ref in ${=refs}; do
	echo -n "$ref: "
	accurev chref -r $ref -m $(hostname)
    done
}

function ac-ws-name {
    accurev show wspaces | awk '$1 ~ /_'$USER'$/ && _pwd == $2 {print $1}' _pwd="$(pwd)"
}

function ac-chws {
    accurev chws -w $(ac-ws-name) $@
}

function ac-stat {
    accurev stat $@ | column -t | ecbuffer
}

function ac-diff {
    accurev diff $@ | ecbuffer
}

function ac-help {
    accurev help $@ | ecbuffer
}

function ac-promote {
    comment_file=/tmp/ac-promote.txt

    $EDITOR $comment_file
    accurev promote -c "@${comment_file}" $@
}

function ac-chws-release {
    stream=$1
    next_stream=$2
    workspaces=$(accurev show -s "$stream" -1 -fv streams | awk '$5 == "3" { print $1 }')

    for ws in $=workspaces; do 
	accurev chws -s "$ws" -b "$next_stream"
    done
}

function ac-chstream-release {
    stream=$1
    next_stream=$2
    workspaces=$(accurev show -s "$stream" -1 -fv streams | awk '$5 == "1" || $5 == "8" { print $1 }')

    for ws in $=workspaces; do 
	accurev chstream -s "$ws" -b "$next_stream"
    done
}
