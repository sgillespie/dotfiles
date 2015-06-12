#!/bin/zsh

# Find script location
SCRIPT_PATH="$HOME/.zshrc"	# Assume ~/.zshrc

while [[ -h "$SCRIPT_PATH" ]]; do
  SCRIPT_PATH="$(readlink "$SCRIPT_PATH")"
done
SCRIPT_DIR="$(dirname $SCRIPT_PATH)"

if [[ ! $SCRIPT_DIR =~ ^/ ]]; then
    SCRIPT_DIR="$HOME/$SCRIPT_DIR"
fi

export INIT_SCRIPT_DIR=${SCRIPT_DIR}

# Load files in ~/.zsh.d
if [[ -d ~/.zsh.d ]]; then
    for src in $(find ~/.zsh.d -type f -name \*.zsh); do
	source "$src"
    done
fi

# Find platform
case $(uname) in
    Linux)
        source $SCRIPT_DIR/zsh.conf/linux.sh
        ;;
    Darwin)
        source $SCRIPT_DIR/zsh.conf/osx.sh
        ;;
esac

# Find accurev
accurev=$(whence accurev)
if [[ $? -eq 0 ]]; then
    source $SCRIPT_DIR/zsh.conf/accurev.zsh
fi

# zsh completion
fpath=("$INIT_SCRIPT_DIR" $fpath)

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

export GRADLE_OPTS="-Xmx1024m -XX:MaxPermSize=128m -Djavax.net.ssl.trustStore=/Users/sgillespie/local/java/cacerts -Djavax.net.ssl.trustStorePassword=changeit"

function ecbuffer {
    TMP="$(mktemp /tmp/emacsstdinXXX)"
    cat > $TMP
    ec --alternate-editor /usr/bin/false --eval "(let ((b (create-file-buffer \"*stdin*\"))) (switch-to-buffer b) (insert-file-contents \"${TMP}\"))"
    rm $TMP
}

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/sgillespie/.gvm/bin/gvm-init.sh" ]] && source "/Users/sgillespie/.gvm/bin/gvm-init.sh"
