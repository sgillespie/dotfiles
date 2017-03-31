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

# zsh completion
autoload -Uz compinit
compinit

fpath=(~/.zsh.d $fpath)

# Prompt
source $INIT_SCRIPT_DIR/prompt.zsh
setprompt

# aliases
if [[ ! -n "${EC+DEFINED}" ]]; then
    EC=emacsclient
fi

alias ec="$EC -n"
export EDITOR="$EC"

# make sure color output is set
export CLICOLOR=

function ecbuffer {
    TMP="$(mktemp /tmp/emacsstdinXXX)"
    cat > $TMP
    ec --alternate-editor /usr/bin/false --eval "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"${TMP}\"))"
    rm $TMP
}
