#
# ~/.bashrc
#
export PATH=~/.emacs.d/bin:$PATH
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
source ~/.dotbare/dotbare.plugin.bash

alias dots=~/.scripts/dotbare_auto.sh
