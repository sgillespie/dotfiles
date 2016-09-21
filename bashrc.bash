#!/bin/bash

# Find the current script dir
# From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

export INIT_SCRIPT_DIR=${DIR}

# Find platform
case $(uname) in
    Linux)
        source $INIT_SCRIPT_DIR/bash.conf/linux.sh
        ;;
    Darwin)
        source $INIT_SCRIPT_DIR/bash.conf/osx.sh
        ;;
esac

# Load files in ~/.zsh.d
if [[ -d ~/.bash.d ]]; then
    for src in $(find ~/.bash.d -type f -name \*.bash); do
	source "$src"
    done
fi

# Prompt
source $INIT_SCRIPT_DIR/bash.conf/prompt.bash

# aliases
if [[ ! -n "${EC+DEFINED}" ]]; then
    EC=emacsclient
fi

# Variables
alias ec="$EC -n"
export EDITOR="$EC"

# make sure color output is set
export CLICOLOR=

# Make it easy to redirect output to emacs
function ecbuffer {
    TMP="$(mktemp /tmp/emacsstdinXXX)"
    cat > $TMP
    ec --alternate-editor /usr/bin/false --eval "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"${TMP}\"))"
    rm $TMP
}
