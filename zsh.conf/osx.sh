#!/bin/sh

EC=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

# aliases
alias pbcp="pbcopy && pbpaste"
alias cw="cd ~/Dev/workspaces"
alias cr="cd ~/Dev/repositories"

alias g="bash gradle.sh"
alias gu="g update"
alias ge="g eclipse"
alias gi="g initWorkspace cleanEclipse eclipse"

export DYLD_LIBRARY_PATH=~/local/oracle

export ws=~/Dev/workspaces

export PUBLIC_SHARE="//sgillespie@jxwprdfil01.availity.com/bts/Public/Sean"
export GUIDES_SHARE='//sgillespie@jxwprdfil02.availity.com/Company/Finance%20and%20Administration/Public/Hipaa/Implementation%20Guides'

function mount-smb {
    if [[ $# != "1" ]]; then
	echo "Usage: mount-smb <share>"
	return
    fi

    share=/Volumes/$(echo $1 | awk -F "/" '{ print $NF }' | sed 's/%20/_/g')

    if ! ([[ -d $share ]] || mkdir $share); then
	echo "can't create $share"
	return
    fi

    mount -t smbfs $1 $share
}

function ac-login {
    password=$(security find-generic-password -a accurev -g 2>&1 | egrep '^[^ ]' | awk -F ": *" '$1 == "password" {gsub(/^\"|\"$/, "", $2); print $2}')
    accurev login -n $USER $password
}

