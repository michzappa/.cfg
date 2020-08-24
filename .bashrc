#
# ~/.bashrc
#

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH
export PATH=~/.emacs.d/bin:$PATH

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
source ~/.dotbare/dotbare.plugin.bash

alias dots=~/.scripts/dotbare_auto.sh
dotbare pull
