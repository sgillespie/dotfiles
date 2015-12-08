#!/bin/bash

### Find the current script dir
### From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

### Utility functions
# Creates a symbolic link, checking to see if it exists first
# Usage: mkLink SRC DEST
function mkLink {
    local SRC=$1
    local DEST=$2

    if [[ -e "$DEST" || -L "$DEST" ]]; then
        echo "$DEST exists, overwriting!"
        rm "$DEST"
    fi

    ln -s "$SRC" "$DEST"
}

### Set up config files
## Create Links
mkLink "$DIR/zshrc.zsh" "$HOME/.zshrc"
mkLink "$DIR/emacs.el" "$HOME/.emacs"
mkLink "$DIR/screenrc" "$HOME/.screenrc"
mkLink "$DIR/tmux.conf" "$HOME/.tmux.conf"
mkLink "$DIR/bashrc.bash" "$HOME/.bashrc"
mkLink "$DIR/bashrc.bash" "$HOME/.bash_profile"

## Add gitconfig
git config --global include.path "$DIR/gitconfig"
