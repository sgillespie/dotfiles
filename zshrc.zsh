#!/bin/zsh

# Find script location
# From https://stackoverflow.com/questions/9901210/bash-source0-equivalent-in-zsh#23259585
# and http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SOURCE=${(%):-%N}
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$(dirname "$SOURCE")" && pwd)"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$(cd -P "$(dirname "$SOURCE")" && pwd)"

export INIT_SCRIPT_DIR=${DIR}

# Load files in ~/.zsh.d
if [[ -d ~/.zsh.d ]]; then
    for src in $(find ~/.zsh.d -type f -name \*.zsh); do
	source "$src"
    done
fi

# History
HISTFILE=~/.histfile
HISTSIZE=9999
SAVEHIST=9999

# Key bindings
bindkey -e

# zsh completion
autoload -Uz compinit
compinit

# Prompt
source $INIT_SCRIPT_DIR/zsh.conf/prompt.zsh
set_prompt

setopt interactivecomments

# aliases
if [[ ! -n "${EC+DEFINED}" ]]; then
    EC=emacsclient
fi

alias ec="$EC -n"
export EDITOR="emacsclient"

# make sure color output is set
export CLICOLOR=

# SSH Agent, if we can find it
if [[ -S "${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh" ]]; then
    export GPG_TTY="$(tty)"
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
elif [[ -S "${XDG_RUNTIME_DIR}/ssh-agent.socket" ]]; then
    export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
fi

# Editors
alias ec="$EC -n"

function ecbuffer {
    TMP="$(mktemp /tmp/emacsstdinXXX)"
    cat > $TMP
    ec --alternate-editor /usr/bin/false --eval "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"${TMP}\"))"
    rm $TMP
}

