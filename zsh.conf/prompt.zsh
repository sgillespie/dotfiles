#!/bin/zsh

autoload -U colors zsh/terminfo
colors

set_prompt () {
    Last_Command=$? # Must come first!

    Blue='\[\e[01;34m\]'
    White='\[\e[01;37m\]'
    Red='\[\e[01;31m\]'
    Green='\[\e[01;32m\]'
    Reset='\[\e[00m\]'
    Sad='\342\234\227'
    Happy='\342\234\223'

    PROMPT="┌%{$fg_bold[cyan]%}─(%{$fg_bold[green]%}%n@%m%{$fg_bold[cyan]%})─%{$reset_color%}─%{$fg_bold[cyan]%}(%{$fg[magenta]%}%~%{$fg_bold[cyan]%})
└%{$fg_bold[cyan]%}─("
    

    # If it was successful, print a happy. Otherwise, print
    # a sad.
    if [[ $Last_Command == 0 ]]; then
        PROMPT+="%{$fg_bold[green]%}✓"
    else
        PROMPT+="%{$fg_bold[red]%}✗"
    fi

    PROMPT+="%{$fg_bold[cyan]%}:%{$fg_bold[yellow]%}%T%{$fg_bold[cyan]%})─%{$reset_color%}─ "
}
PROMPT_COMMAND='set_prompt'
